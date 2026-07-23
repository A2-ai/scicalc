#' Build a Human-Readable Assembly Audit Report
#'
#' Builds a reviewer-oriented view over the immutable event log returned by
#' [scicalc_audit()]. For each unit-bearing final column the report states its
#' unit story -- which call attached or converted its units, from what evidence
#' (source unit column, conversion, arithmetic on unit-bearing columns, or a
#' spec attach flagged for review) -- by joining runtime unit events to the
#' tagged expressions captured by [audit_script()]. Runtime events that match
#' no tagged call are listed as unattributed rather than dropped. The original
#' events remain available as `report$events`.
#'
#' @param name Audit name; reads `<dir>/<name>.audit.log`.
#' @param dir Directory holding named audit logs.
#' @param log_file Explicit audit log path, overriding `name` and `dir`.
#' @param trace Columns to include in the printed column sections: final
#'   unit-bearing and unitless numeric columns (`"units"`, the default), or
#'   every final column (`"all"`).
#'
#' @return An object of class `scicalc_audit_report` with `overview`, `files`,
#'   final-column `columns`, AST `lineage`, per-column `units` (unit stories
#'   and unattributed residual), runtime `evidence` and `transformations`,
#'   `findings`, and the original `events` tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' report <- scicalc_audit_report("pk")
#' report
#' report$transformations
#' }
scicalc_audit_report <- function(name = NULL, dir = default_audit_dir(), log_file = NULL, trace = c("units", "all")) {
  trace <- match.arg(trace)
  events <- scicalc_audit(name = name, dir = dir, log_file = log_file)
  run <- audit_report_run(events)
  files <- audit_report_files(events)
  columns <- audit_report_columns(events)
  lineage <- audit_report_lineage(events)
  units <- audit_report_units(events, columns, lineage, files)
  transformations <- audit_report_transformations(events)
  evidence <- audit_report_evidence(transformations)
  findings <- audit_report_findings(events, files, transformations)

  status <- if (identical(run$phase[[1]], "failed")) {
    "run failed"
  } else if (any(findings$severity == "error")) {
    "attention required"
  } else if (any(findings$severity == "warning")) {
    "review recommended"
  } else {
    "evidence captured"
  }

  structure(
    list(
      overview = tibble::tibble(
        status = status,
        inputs = sum(files$role == "input"),
        specifications = sum(files$role == "specification"),
        outputs = sum(files$role == "output"),
        transformations = nrow(transformations)
      ),
      run = run,
      files = files,
      columns = columns,
      lineage = lineage,
      units = units,
      trace = trace,
      evidence = evidence,
      transformations = transformations,
      findings = findings,
      events = events
    ),
    class = "scicalc_audit_report"
  )
}

# Final schema supplied to audit_script(data = final).
audit_report_columns <- function(events) {
  keep <- audit_report_field(events, "event_type") == "schema"
  if (!any(keep)) {
    return(tibble::tibble(target = character(), data_type = character(), has_units = logical(), unit = character()))
  }
  rows <- events[keep, , drop = FALSE]
  dplyr::distinct(tibble::tibble(
    target = audit_report_field(rows, "target"),
    data_type = audit_report_field(rows, "data_type"),
    has_units = audit_report_field(rows, "has_units") == "TRUE",
    unit = audit_report_field(rows, "unit")
  ))
}

# AST-derived final-column lineage captured by audit_script(data = final).
audit_report_lineage <- function(events) {
  keep <- audit_report_field(events, "event_type") == "lineage"
  if (!any(keep)) return(audit_empty_lineage())
  rows <- events[keep, , drop = FALSE]
  tibble::tibble(
    target = audit_report_field(rows, "target"),
    relation = audit_report_field(rows, "relation"),
    object = audit_report_field(rows, "object"),
    symbol = audit_report_field(rows, "symbol"),
    expression = audit_report_field(rows, "expression"),
    detail = audit_report_field(rows, "detail"),
    source_object = audit_report_field(rows, "source_object"),
    source_column = audit_report_field(rows, "source_column"),
    path = audit_report_field(rows, "path"),
    depth = audit_report_field(rows, "depth"),
    order = audit_report_field(rows, "order")
  )
}

# ---------------------------------------------------------------------------
# Unit provenance: join runtime unit events to the static tags that created
# each final column. Events are logged in execution order and a top-to-bottom
# assembly script runs its tagged calls in textual (AST `order`) order, so the
# k-th event group with an identical (fn, input) expression belongs to the k-th
# textual occurrence of that call. Events matching no tagged call (e.g. fired
# inside a sourced helper) are reported as unattributed rather than shifting
# other matches.
audit_report_units <- function(events, columns, lineage, files) {
  empty_stories <- tibble::tibble(
    target = character(), kind = character(), line = character(), refs = character()
  )
  result <- list(stories = empty_stories, residual = character())
  raw <- events[audit_report_field(events, "event_type") == "unit", , drop = FALSE]
  if (nrow(raw) == 0L || nrow(columns) == 0L) return(result)

  unit_events <- tibble::tibble(
    fn = audit_report_field(raw, "fn"),
    input = audit_report_field(raw, "input"),
    from = audit_report_field(raw, "from"),
    to = audit_report_field(raw, "to"),
    transform = audit_report_field(raw, "transform"),
    detail = audit_report_field(raw, "detail"),
    evidence = audit_report_event_evidence(raw),
    basis = audit_report_field(raw, "basis"),
    context = audit_report_field(raw, "context"),
    n = suppressWarnings(as.numeric(audit_report_field(raw, "n")))
  )
  unit_events$n[is.na(unit_events$n)] <- 0

  stories <- list()
  residual <- character()
  add_story <- function(target, kind, line, refs = NA_character_) {
    stories[[length(stories) + 1L]] <<- tibble::tibble(
      target = target, kind = kind, line = line, refs = refs
    )
  }

  spec_file <- files$file[files$role == "specification"]
  spec_file <- if (length(spec_file) == 0L || is.na(spec_file[[1]]) || !nzchar(spec_file[[1]])) {
    "specification"
  } else {
    spec_file[[1]]
  }

  is_spec <- unit_events$fn %in% "convert_units_to_spec"
  spec_events <- unit_events[is_spec, , drop = FALSE]
  call_events <- unit_events[!is_spec, , drop = FALSE]

  # every textual occurrence of a tagged expression, in script order
  tags <- lineage[lineage$relation %in% c("definition", "step"), , drop = FALSE]
  tag_column <- ifelse(tags$relation == "definition", tags$target, tags$symbol)
  tag_order <- suppressWarnings(as.integer(tags$order))
  keep <- !duplicated(paste(tags$object, tag_column, tag_order))
  occurrences <- tibble::tibble(
    object = tags$object[keep], tag = tag_column[keep],
    expression = tags$expression[keep], order = tag_order[keep]
  )
  occurrences <- occurrences[order(occurrences$order, na.last = TRUE), , drop = FALSE]

  calls <- list()
  if (nrow(call_events) > 0L) {
    event_fns <- unique(call_events$fn)
    for (index in seq_len(nrow(occurrences))) {
      for (found in audit_ast_matching_calls(occurrences$expression[[index]], event_fns)) {
        calls[[length(calls) + 1L]] <- c(found, list(occurrence = index))
      }
    }
  }

  # A mixed-units call emits one event per distinct unit, marked by a
  # "row-level" basis; only those consecutive same-(fn, input) events are one
  # call. Identical single-unit calls each stay their own group.
  if (nrow(call_events) > 0L) {
    row_level <- grepl("^row-level", call_events$basis)
    group_id <- integer(nrow(call_events))
    current <- 0L
    last_key <- NULL
    seen_units <- character()
    for (row in seq_len(nrow(call_events))) {
      key <- paste(call_events$fn[[row]], call_events$input[[row]])
      continues <- !is.null(last_key) && key == last_key &&
        isTRUE(row_level[[row]]) && isTRUE(row_level[[row - 1L]]) &&
        !(call_events$to[[row]] %in% seen_units)
      if (!continues) {
        current <- current + 1L
        seen_units <- character()
      }
      last_key <- key
      seen_units <- c(seen_units, call_events$to[[row]])
      group_id[[row]] <- current
    }

    occurrence_taken <- integer()
    for (id in unique(group_id)) {
      group <- call_events[group_id == id, , drop = FALSE]
      merged <- group[1L, , drop = FALSE]
      merged$from <- paste(unique(stats::na.omit(group$from)), collapse = ", ")
      merged$to <- paste(unique(stats::na.omit(group$to)), collapse = ", ")
      merged$n <- sum(group$n)
      group_key <- paste(merged$fn, merged$input)
      k <- if (group_key %in% names(occurrence_taken)) occurrence_taken[[group_key]] + 1L else 1L
      occurrence_taken[[group_key]] <- k

      candidates <- Filter(
        function(call) call$fn == merged$fn[[1]] && merged$input[[1]] %in% call$args,
        calls
      )
      if (length(candidates) == 0L) {
        residual <- c(residual, audit_report_residual_line(merged))
        next
      }
      candidate <- candidates[[min(k, length(candidates))]]
      occurrence <- occurrences[candidate$occurrence, , drop = FALSE]
      line <- audit_report_unit_call_line(candidate, occurrence$object[[1]], merged)
      if (occurrence$tag[[1]] %in% columns$target) {
        add_story(occurrence$tag[[1]], "call", line)
        next
      }
      hosts <- unique(lineage$target[
        lineage$relation == "step" &
          lineage$object %in% occurrence$object[[1]] &
          lineage$symbol %in% occurrence$tag[[1]]
      ])
      hosts <- intersect(hosts, columns$target)
      if (length(hosts) == 0L) {
        residual <- c(residual, audit_report_residual_line(merged))
      } else {
        for (host in hosts) {
          add_story(host, "call", paste0("via ", occurrence$tag[[1]], ": ", line))
        }
      }
    }
  }

  # A spec invocation is located by matching its captured data expression to a
  # convert_units_to_spec() call inside an assignment. An invocation matching
  # no assignment (e.g. piped into a view) changed data that was then
  # discarded, so its events are excluded from column evidence.
  callsites <- lineage[lineage$relation == "callsite", , drop = FALSE]
  for (index in seq_len(nrow(spec_events))) {
    event <- spec_events[index, , drop = FALSE]
    context <- event$context[[1]]
    site <- callsites[!is.na(callsites$detail) & callsites$detail %in% context, , drop = FALSE]
    if (!is.na(context) && nrow(callsites) > 0L && nrow(site) == 0L) {
      residual <- c(residual, paste0(
        audit_report_residual_line(event),
        " \u2014 from convert_units_to_spec() on `", context,
        "`, whose result was not assigned"
      ))
      next
    }
    object <- if (nrow(site) > 0L) site$object[[1]] else NA_character_
    call_label <- if (nrow(site) > 0L) {
      audit_report_spec_call_label(site$expression[[1]])
    } else {
      "convert_units_to_spec()"
    }
    if (event$input[[1]] %in% columns$target) {
      add_story(
        event$input[[1]], "spec",
        audit_report_unit_spec_line(event, spec_file, object, call_label)
      )
    } else {
      residual <- c(residual, audit_report_residual_line(event))
    }
  }

  # A unit column created without its own unit call inherits units from its
  # unit-bearing operands -- unless a spec attach event exists for it, which
  # proves the column reached the spec unitless.
  bound <- if (length(stories) == 0L) empty_stories else dplyr::bind_rows(stories)
  spec_attached <- unique(spec_events$input[spec_events$transform %in% "attach"])
  unit_columns <- columns$target[columns$has_units]
  for (target in unit_columns) {
    if (any(bound$target == target & bound$kind == "call")) next
    if (target %in% spec_attached) next
    definitions <- lineage[lineage$target == target & lineage$relation == "definition", , drop = FALSE]
    for (index in seq_len(nrow(definitions))) {
      expression <- tryCatch(str2lang(definitions$expression[[index]]), error = function(e) NULL)
      if (is.null(expression)) next
      operands <- intersect(audit_ast_symbols(expression), setdiff(unit_columns, target))
      if (length(operands) == 0L) next
      labels <- vapply(operands, function(operand) {
        unit <- columns$unit[columns$target == operand][[1]]
        if (is.na(unit) || !nzchar(unit)) operand else paste0(operand, " [", unit, "]")
      }, character(1))
      add_story(
        target, "derived",
        paste0(
          definitions$expression[[index]], " in ", definitions$object[[index]],
          " \u2014 units derived by arithmetic from ", paste(labels, collapse = " and ")
        ),
        refs = paste(operands, collapse = ",")
      )
    }
  }

  stories <- if (length(stories) == 0L) empty_stories else dplyr::bind_rows(stories)
  list(stories = stories, residual = residual)
}

audit_report_unit_call_line <- function(candidate, object, event) {
  fn <- event$fn[[1]]
  detail <- event$detail[[1]]
  label <- if (identical(fn, "with_units")) {
    candidate$text
  } else if (fn %in% c("convert_mass_to_mol", "convert_mol_to_mass") && !is.na(detail) && nzchar(detail)) {
    paste0(fn, "(", audit_report_input_label(event$input[[1]]), ", ", detail, ")")
  } else {
    paste0(fn, "(", audit_report_input_label(event$input[[1]]), ")")
  }
  evidence <- event$evidence[[1]]
  basis <- event$basis[[1]]
  suffix <- if (identical(evidence, "assumed") && !is.na(basis) && nzchar(basis)) {
    # e.g. "unitless numeric interpreted as umol/L by convert_bili()"
    basis
  } else if (identical(evidence, "carried-converted")) {
    from <- event$from[[1]]
    if (is.na(from) || !nzchar(from)) {
      "input already carried units"
    } else {
      paste0("input already carried ", from)
    }
  } else {
    audit_report_evidence_short(evidence)
  }
  paste0(
    label, " in ", object, " \u2014 ", audit_report_unit_action(event),
    " \u2014 ", suffix,
    " (", format(event$n[[1]], big.mark = ",", trim = TRUE), " values)"
  )
}

audit_report_unit_action <- function(event) {
  transform <- event$transform[[1]]
  detail <- event$detail[[1]]
  if (identical(transform, "attach")) {
    source <- if (is.na(detail) || !nzchar(detail)) {
      ""
    } else if (startsWith(detail, "\"")) {
      paste0(" from literal ", detail)
    } else {
      paste0(" from unit column ", detail)
    }
    return(paste0("attached ", event$to[[1]], source))
  }
  text <- paste0(event$from[[1]], " \u2192 ", event$to[[1]])
  if (identical(transform, "log-shift")) text <- paste0(text, " (log reference shift)")
  text
}

audit_report_unit_spec_line <- function(event, spec_file, object = NA_character_,
                                        call_label = "convert_units_to_spec()") {
  transform <- event$transform[[1]]
  action <- if (identical(transform, "attach")) {
    paste0("attached ", event$to[[1]], " to unitless numeric")
  } else {
    audit_report_unit_action(event)
  }
  if (identical(transform, "failed")) action <- paste0(action, " failed")
  location <- if (is.na(object) || !nzchar(object)) "" else paste0(" in ", object)
  paste0(
    call_label, location, " \u2014 ", action, " \u2014 from ", spec_file,
    " (", format(event$n[[1]], big.mark = ",", trim = TRUE), " values)"
  )
}

# The convert_units_to_spec() call as the analyst wrote it, minus the piped
# data argument.
audit_report_spec_call_label <- function(text) {
  node <- tryCatch(str2lang(text), error = function(e) NULL)
  if (is.null(node) || !is.call(node) || length(node) < 2L) {
    return("convert_units_to_spec()")
  }
  paste(deparse(as.call(as.list(node)[-2L]), width.cutoff = 500L), collapse = " ")
}

audit_report_residual_line <- function(event) {
  row <- tibble::tibble(
    input = event$input[[1]], fn = event$fn[[1]], transform = event$transform[[1]],
    from = event$from[[1]], to = event$to[[1]], detail = event$detail[[1]],
    n = event$n[[1]]
  )
  paste0(
    audit_report_transformation_text(row), " \u2014 ",
    audit_report_evidence_short(event$evidence[[1]])
  )
}

audit_report_evidence_short <- function(evidence) {
  switch(
    evidence,
    `source-recorded` = "source-recorded",
    `carried-converted` = "carried",
    `analyst-declared` = "analyst-declared",
    evidence
  )
}

# Build the run manifest from the first/last run bookend currently available.
audit_report_run <- function(events) {
  run <- events[audit_report_field(events, "event_type") == "run", , drop = FALSE]
  if (nrow(run) == 0) {
    return(tibble::tibble(
      phase = NA_character_, script = NA_character_, script_hash = NA_character_,
      script_type = NA_character_, scicalc_version = NA_character_,
      r_version = NA_character_, error = NA_character_
    ))
  }

  tibble::tibble(
    phase = audit_report_field(run, "phase")[nrow(run)],
    script = audit_report_field(run, "script")[1],
    script_hash = audit_report_field(run, "script_hash")[1],
    script_type = audit_report_field(run, "script_type")[1],
    scicalc_version = audit_report_field(run, "scicalc_version")[1],
    r_version = audit_report_field(run, "r_version")[1],
    error = audit_report_field(run, "error")[nrow(run)]
  )
}

# Build the file/specification anchor table.
audit_report_files <- function(events) {
  event_type <- audit_report_field(events, "event_type")
  keep <- event_type %in% c("ingest", "spec", "write")
  events <- events[keep, , drop = FALSE]
  event_type <- event_type[keep]

  dplyr::distinct(tibble::tibble(
    role = c(ingest = "input", spec = "specification", write = "output")[event_type],
    file = ifelse(
      event_type == "spec",
      audit_report_field(events, "spec_file"),
      audit_report_field(events, "file")
    ),
    hash = ifelse(
      event_type == "spec",
      audit_report_field(events, "spec_hash"),
      audit_report_field(events, "hash")
    ),
    algo = audit_report_field(events, "algo")
  ))
}

# Group identical transformation records so grouped mutate() calls stay
# readable in the report.
audit_report_transformations <- function(events) {
  event_type <- audit_report_field(events, "event_type")
  events <- events[event_type == "unit", , drop = FALSE]

  if (nrow(events) == 0) {
    return(tibble::tibble(
      input = character(), fn = character(), transform = character(),
      from = character(), to = character(), detail = character(), evidence = character(),
      basis = character(), n = numeric()
    ))
  }

  transformations <- tibble::tibble(
    input = audit_report_field(events, "input"),
    fn = audit_report_field(events, "fn"),
    transform = audit_report_field(events, "transform"),
    from = audit_report_field(events, "from"),
    to = audit_report_field(events, "to"),
    detail = audit_report_field(events, "detail"),
    evidence = audit_report_event_evidence(events),
    basis = audit_report_field(events, "basis"),
    n = suppressWarnings(as.numeric(audit_report_field(events, "n")))
  )
  transformations$n[is.na(transformations$n)] <- 0

  dplyr::summarise(
    dplyr::group_by(
      transformations,
      .data$input, .data$fn, .data$transform, .data$from, .data$to,
      .data$detail, .data$evidence, .data$basis,
      .drop = FALSE
    ),
    n = sum(.data$n),
    .groups = "drop"
  )
}

# Derive a cautious label for old logs that predate explicit evidence fields.
audit_report_event_evidence <- function(events) {
  explicit <- audit_report_field(events, "evidence")
  missing <- is.na(explicit) | !nzchar(explicit)
  fn <- audit_report_field(events, "fn")
  transform <- audit_report_field(events, "transform")

  explicit[missing & fn == "with_units"] <- "source-recorded"
  explicit[missing & fn == "convert_units_to_spec" & transform == "attach"] <- "assumed"
  explicit[missing & fn == "convert_units_to_spec" & transform %in% c("convert", "log-shift")] <- "carried-converted"
  explicit[missing & transform == "failed"] <- "failed"
  explicit[is.na(explicit) | !nzchar(explicit)] <- "unclassified"
  explicit
}

# Compact counts for use in tables and programmatic review.
audit_report_evidence <- function(transformations) {
  if (nrow(transformations) == 0) {
    return(tibble::tibble(evidence = character(), transformations = integer(), values = numeric()))
  }
  dplyr::summarise(
    dplyr::group_by(transformations, .data$evidence),
    transformations = dplyr::n(),
    values = sum(.data$n),
    .groups = "drop"
  )
}

# Create explicit review findings from what the current event schema can prove.
audit_report_findings <- function(events, files, transformations) {
  findings <- list()
  add_finding <- function(severity, finding, detail = NA_character_) {
    findings[[length(findings) + 1L]] <<- tibble::tibble(
      severity = severity,
      finding = finding,
      detail = detail
    )
  }

  if (!any(files$role == "input")) {
    add_finding("warning", "No hash-anchored input files were recorded.")
  }
  if (!any(files$role == "output")) {
    add_finding("warning", "No hash-anchored output file was recorded.")
  }

  failed <- transformations[transformations$transform == "failed", , drop = FALSE]
  if (nrow(failed) > 0) {
    for (i in seq_len(nrow(failed))) {
      add_finding(
        "error",
        "A unit conversion failed.",
        audit_report_transformation_text(failed[i, , drop = FALSE])
      )
    }
  }

  if (length(findings) == 0) {
    return(tibble::tibble(
      severity = character(), finding = character(), detail = character()
    ))
  }
  dplyr::bind_rows(findings)
}

# Pull a character field from a sparse JSON event table.
audit_report_field <- function(events, field) {
  if (!field %in% names(events)) {
    return(rep(NA_character_, nrow(events)))
  }
  as.character(events[[field]])
}

# Make one unit event readable in a single sentence.
audit_report_transformation_text <- function(row) {
  input <- audit_report_input_label(row$input[[1]])
  from <- row$from[[1]]
  to <- row$to[[1]]
  detail <- row$detail[[1]]
  n <- row$n[[1]]

  action <- switch(
    row$transform[[1]],
    attach = paste0("attached ", to, if (!is.na(detail)) paste0(" from ", detail) else ""),
    convert = paste0(from, " \u2192 ", to),
    `log-shift` = paste0(from, " \u2192 ", to, " (log reference shift)"),
    failed = paste0(from, " \u2192 ", to),
    paste0(from, " \u2192 ", to)
  )
  show_detail <- row$transform[[1]] == "log-shift" ||
    row$fn[[1]] %in% c("convert_mass_to_mol", "convert_mol_to_mass")
  if (!is.na(detail) && show_detail) {
    action <- paste0(action, " [", detail, "]")
  }

  fn <- row$fn[[1]]
  via <- if (is.na(fn) || !nzchar(fn)) "" else paste0(" via ", fn, "()")

  paste0(
    input, ": ", action, via,
    " (", format(n, big.mark = ",", trim = TRUE), " values)"
  )
}

# Hide serialized values captured by older audit logs. A data object is never a
# useful reviewer-facing input label; new conversion events retain the caller's
# expression before their arguments are evaluated.
audit_report_input_label <- function(input) {
  if (is.na(input) || !nzchar(input)) return("<unnamed input>")
  if (startsWith(trimws(input), "structure(")) {
    return("<unlabelled mixed-units input>")
  }
  input
}
