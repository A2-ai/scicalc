#' Build a Human-Readable Assembly Audit Report
#'
#' Builds a reviewer-oriented view over the immutable event log returned by
#' [scicalc_audit()]. The report groups repeated transformations, separates
#' input/specification/output hash anchors, and states any findings that need
#' review. The original events remain available as `report$events`.
#'
#' This first report layer describes captured evidence. It does not yet claim
#' that every unit-bearing output column has been reconciled; that terminal
#' completeness check is added during audit capture in a later step.
#'
#' @param name Audit name; reads `<dir>/<name>.audit.log`.
#' @param dir Directory holding named audit logs.
#' @param log_file Explicit audit log path, overriding `name` and `dir`.
#' @param trace Columns to include in the printed lineage: final unit-bearing
#'   and unitless numeric columns (`"units"`, the default), or every final
#'   column (`"all"`).
#'
#' @return An object of class `scicalc_audit_report` with `overview`, `files`,
#'   final-column `columns` and AST `lineage`, runtime `evidence` and
#'   `transformations`, `findings`, and the original `events` tibble.
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
  transformations <- audit_report_transformations(events)
  evidence <- audit_report_evidence(transformations)
  findings <- audit_report_findings(events, files, transformations)

  status <- if (any(findings$severity == "error")) {
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
      trace = trace,
      evidence = evidence,
      transformations = transformations,
      findings = findings,
      events = events
    ),
    class = "scicalc_audit_report"
  )
}

#' @export
print.scicalc_audit_report <- function(x, ..., max_transformations = Inf) {
  if (!is.infinite(max_transformations)) {
    checkmate::assert_number(max_transformations, lower = 1, null.ok = FALSE)
  }

  overview <- x$overview
  cli::cli_h1("scicalc audit")
  cli::cli_text("{.strong Status:} {overview$status}")

  audit_report_print_run(x$run)
  audit_report_print_files(x$files)
  audit_report_print_output_lineage(x$columns, x$lineage, x$trace)
  audit_report_print_evidence(x$transformations, max_transformations)
  audit_report_print_findings(x$findings)

  invisible(x)
}

# Final schema supplied to audit_script(data = final).
#' @noRd
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
#' @noRd
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
    path = audit_report_field(rows, "path")
  )
}

# Build the run manifest from the first/last run bookend currently available.
#' @noRd
audit_report_run <- function(events) {
  run <- events[audit_report_field(events, "event_type") == "run", , drop = FALSE]
  if (nrow(run) == 0) {
    return(tibble::tibble(
      phase = NA_character_, script = NA_character_, script_hash = NA_character_,
      script_type = NA_character_, scicalc_version = NA_character_,
      r_version = NA_character_
    ))
  }

  tibble::tibble(
    phase = audit_report_field(run, "phase")[nrow(run)],
    script = audit_report_field(run, "script")[1],
    script_hash = audit_report_field(run, "script_hash")[1],
    script_type = audit_report_field(run, "script_type")[1],
    scicalc_version = audit_report_field(run, "scicalc_version")[1],
    r_version = audit_report_field(run, "r_version")[1]
  )
}

# Build the file/specification anchor table.
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
audit_report_field <- function(events, field) {
  if (!field %in% names(events)) {
    return(rep(NA_character_, nrow(events)))
  }
  as.character(events[[field]])
}

# Make one unit event readable in a single sentence.
#' @noRd
audit_report_transformation_text <- function(row) {
  input <- audit_report_input_label(row$input[[1]])
  from <- row$from[[1]]
  to <- row$to[[1]]
  detail <- row$detail[[1]]
  n <- row$n[[1]]

  action <- switch(
    row$transform[[1]],
    attach = paste0("attached ", to, if (!is.na(detail)) paste0(" from ", detail) else ""),
    convert = paste0(from, " → ", to),
    `log-shift` = paste0(from, " → ", to, " (log reference shift)"),
    failed = paste0(from, " → ", to),
    paste0(from, " → ", to)
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
#' @noRd
audit_report_input_label <- function(input) {
  if (is.na(input) || !nzchar(input)) return("<unnamed input>")
  if (startsWith(trimws(input), "structure(")) {
    return("<unlabelled mixed-units input>")
  }
  input
}

#' @noRd
audit_report_print_files <- function(files) {
  for (role in c("input", "specification", "output")) {
    rows <- files[files$role == role, , drop = FALSE]
    if (nrow(rows) == 0) next

    cli::cli_h2(paste0(tools::toTitleCase(role), if (nrow(rows) > 1) "s" else ""))
    cli::cli_ul()
    for (i in seq_len(nrow(rows))) {
      file <- rows$file[[i]]
      file <- if (is.na(file) || !nzchar(file)) "<path not recorded>" else file
      hash <- rows$hash[[i]]
      hash <- if (is.na(hash) || !nzchar(hash)) "<hash not recorded>" else substr(hash, 1, 12)
      algo <- rows$algo[[i]]
      algo <- if (is.na(algo) || !nzchar(algo)) "hash" else algo
      cli::cli_li("{file} {.comment [{algo}:{hash}]}")
    }
    cli::cli_end()
  }
}

#' @noRd
audit_report_print_run <- function(run) {
  if (is.na(run$phase[[1]])) return(invisible())

  cli::cli_h2("Run")
  script <- run$script[[1]]
  script <- if (is.na(script) || !nzchar(script)) "<script path not recorded>" else script
  hash <- run$script_hash[[1]]
  hash <- if (is.na(hash) || !nzchar(hash)) "<hash not recorded>" else substr(hash, 1, 12)
  cli::cli_text("{.strong Script:} {script} {.comment [blake3:{hash}]}")
  cli::cli_text("{.strong Capture:} {run$phase[[1]]}; scicalc {run$scicalc_version[[1]]}; R {run$r_version[[1]]}")
  invisible()
}

# Print final-column lineage from the static graph. Runtime transformations are
# printed separately below; this section is the authoritative answer to where
# a final column and its source symbols came from.
#' @noRd
audit_report_print_output_lineage <- function(columns, lineage, trace) {
  if (nrow(columns) == 0L) return(invisible())

  unit_columns <- columns[columns$has_units, , drop = FALSE]
  numeric_columns <- columns[!columns$has_units & columns$data_type == "numeric", , drop = FALSE]
  other_columns <- columns[!columns$has_units & columns$data_type != "numeric", , drop = FALSE]

  audit_report_print_lineage_group("Unit columns", unit_columns, lineage, unit = TRUE)
  audit_report_print_lineage_group("Unitless numeric columns — review", numeric_columns, lineage, unit = FALSE)
  if (identical(trace, "all")) {
    audit_report_print_lineage_group("Other final columns", other_columns, lineage, unit = FALSE)
  }
  invisible()
}

#' @noRd
audit_report_print_lineage_group <- function(title, columns, lineage, unit) {
  if (nrow(columns) == 0L) return(invisible())
  cli::cli_h2(title)
  for (index in seq_len(nrow(columns))) {
    column <- columns[index, , drop = FALSE]
    label <- column$target[[1]]
    if (unit) {
      label <- paste0(label, " [", column$unit[[1]], "]")
    } else if (column$data_type[[1]] == "numeric") {
      label <- paste0(label, " [no units]")
    }
    cli::cli_text("{.strong {label}}")

    rows <- lineage[lineage$target == column$target[[1]], , drop = FALSE]
    definitions <- rows[rows$relation == "definition", , drop = FALSE]
    sources <- rows[rows$relation == "source", , drop = FALSE]
    terminals <- rows[rows$relation == "terminal", , drop = FALSE]

    for (definition in seq_len(nrow(definitions))) {
      cli::cli_text(paste0("  defined as: ", definitions$expression[[definition]]))
    }
    for (source in seq_len(nrow(sources))) {
      symbol <- sources$symbol[[source]]
      source_text <- paste0(sources$source_object[[source]], "$", sources$source_column[[source]])
      cli::cli_text(paste0("  ", symbol, " <- ", source_text))
    }
    for (terminal in seq_len(nrow(terminals))) {
      text <- terminals$expression[[terminal]]
      if (!is.na(terminals$detail[[terminal]])) text <- paste0(text, " — ", terminals$detail[[terminal]])
      cli::cli_text(paste0("  terminal: ", text))
    }
    path <- c(definitions$path, sources$path, terminals$path)
    path <- unique(path[!is.na(path) & nzchar(path)])
    if (length(path) > 0L) cli::cli_text(paste0("  flow: ", audit_report_forward_path(path[[1]])))
    if (nrow(rows) == 0L) cli::cli_text("  No static lineage was captured for this column.")
    cli::cli_text("")
  }
  invisible()
}

# Static traversal starts at `final` and walks upstream. Readers need the
# opposite direction: creation source flowing into the submitted data frame.
#' @noRd
audit_report_forward_path <- function(path) {
  pieces <- strsplit(path, " -> ", fixed = TRUE)[[1]]
  paste(rev(pieces), collapse = " -> ")
}

#' @noRd
audit_report_print_evidence <- function(transformations, max_transformations) {
  cli::cli_h2("Runtime unit operations")
  if (nrow(transformations) == 0) {
    cli::cli_text("No unit transformations were recorded.")
    return(invisible())
  }

  order <- c("source-recorded", "carried-converted", "analyst-declared", "assumed", "unclassified", "failed")
  levels <- c(intersect(order, unique(transformations$evidence)), setdiff(unique(transformations$evidence), order))

  for (evidence in levels) {
    rows <- transformations[transformations$evidence == evidence, , drop = FALSE]
    cli::cli_h3(audit_report_evidence_label(evidence))
    shown <- if (is.infinite(max_transformations)) rows else utils::head(rows, max_transformations)
    cli::cli_ul()
    for (i in seq_len(nrow(shown))) {
      text <- audit_report_transformation_text(shown[i, , drop = FALSE])
      basis <- shown$basis[[i]]
      if (!is.na(basis) && nzchar(basis)) text <- paste0(text, " — ", basis)
      cli::cli_li(text)
    }
    cli::cli_end()
    if (nrow(rows) > nrow(shown)) {
      cli::cli_text("{.comment {nrow(rows) - nrow(shown)} more transformation(s) in this evidence class.}")
    }
  }
  invisible()
}

#' @noRd
audit_report_evidence_label <- function(evidence) {
  switch(
    evidence,
    `source-recorded` = "Source-recorded",
    `carried-converted` = "Carried / converted",
    `analyst-declared` = "Analyst-declared",
    assumed = "Assumed — review",
    unclassified = "Unclassified — legacy evidence",
    failed = "Failed",
    tools::toTitleCase(gsub("-", " ", evidence, fixed = TRUE))
  )
}

#' @noRd
audit_report_print_findings <- function(findings) {
  cli::cli_h2("Findings")
  if (nrow(findings) == 0) {
    cli::cli_text("No failed conversions or missing file anchors were recorded.")
    return(invisible())
  }

  cli::cli_ul()
  for (i in seq_len(nrow(findings))) {
    label <- paste0(toupper(findings$severity[[i]]), ": ", findings$finding[[i]])
    if (!is.na(findings$detail[[i]])) {
      label <- paste0(label, " ", findings$detail[[i]])
    }
    cli::cli_li(label)
  }
  cli::cli_end()
  invisible()
}
