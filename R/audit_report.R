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
#'
#' @return An object of class `scicalc_audit_report` with `overview`, `files`,
#'   `transformations`, `findings`, and the original `events` tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' report <- scicalc_audit_report("pk")
#' report
#' report$transformations
#' }
scicalc_audit_report <- function(name = NULL, dir = default_audit_dir(), log_file = NULL) {
  events <- scicalc_audit(name = name, dir = dir, log_file = log_file)
  run <- audit_report_run(events)
  files <- audit_report_files(events)
  transformations <- audit_report_transformations(events)
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
  audit_report_print_transformations(x$transformations, max_transformations)
  audit_report_print_findings(x$findings)

  invisible(x)
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
      from = character(), to = character(), detail = character(), n = numeric()
    ))
  }

  transformations <- tibble::tibble(
    input = audit_report_field(events, "input"),
    fn = audit_report_field(events, "fn"),
    transform = audit_report_field(events, "transform"),
    from = audit_report_field(events, "from"),
    to = audit_report_field(events, "to"),
    detail = audit_report_field(events, "detail"),
    n = suppressWarnings(as.numeric(audit_report_field(events, "n")))
  )
  transformations$n[is.na(transformations$n)] <- 0

  dplyr::summarise(
    dplyr::group_by(
      transformations,
      .data$input, .data$fn, .data$transform, .data$from, .data$to,
      .data$detail,
      .drop = FALSE
    ),
    n = sum(.data$n),
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

#' @noRd
audit_report_print_transformations <- function(transformations, max_transformations) {
  cli::cli_h2("Transformations")
  if (nrow(transformations) == 0) {
    cli::cli_text("No unit transformations were recorded.")
    return(invisible())
  }

  shown <- if (is.infinite(max_transformations)) {
    transformations
  } else {
    utils::head(transformations, max_transformations)
  }
  cli::cli_ul()
  for (i in seq_len(nrow(shown))) {
    cli::cli_li(audit_report_transformation_text(shown[i, , drop = FALSE]))
  }
  cli::cli_end()

  if (nrow(transformations) > nrow(shown)) {
    cli::cli_text("{.comment {nrow(transformations) - nrow(shown)} more transformation(s); inspect `$transformations` for all rows.}")
  }
  invisible()
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
