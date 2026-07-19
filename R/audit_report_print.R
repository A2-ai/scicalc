# Console rendering for scicalc_audit_report objects. The report is built in
# audit_report.R; everything here turns that object into cli output.

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
  if (nrow(x$columns) > 0L) {
    audit_report_print_unit_stories(x$columns, x$lineage, x$units$stories, x$trace)
    audit_report_print_residual(x$units$residual)
  } else {
    # no final schema (audit_script() ran without data=): fall back to the
    # grouped runtime evidence view
    audit_report_print_evidence(x$transformations, max_transformations)
  }
  audit_report_print_findings(x$findings)

  invisible(x)
}

#' Render an audit report inside a knitr / Quarto document
#'
#' Reuses the console [print][print.scicalc_audit_report] rendering — the report
#' object is already built (its log file read, its events transformed) by
#' [scicalc_audit_report()], so this only captures that same output as text and
#' emits it as a verbatim block. The knitted report reads the same as it does at
#' the console.
#'
#' @param x A `scicalc_audit_report`.
#' @param ... Passed to [print.scicalc_audit_report()] (e.g.
#'   `max_transformations`).
#'
#' @return A `knitr::asis_output` block (invisibly used by knitr).
#' @exportS3Method knitr::knit_print
knit_print.scicalc_audit_report <- function(x, ...) {
  lines <- cli::cli_fmt(print(x, ...), collapse = FALSE)
  text <- cli::ansi_strip(paste(lines, collapse = "\n"))
  knitr::asis_output(paste0("```\n", text, "\n```\n"))
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

# Per-column unit provenance: the section a reviewer reads to see where each
# final column's units came from.
#' @noRd
audit_report_print_unit_stories <- function(columns, lineage, stories, trace) {
  unit_columns <- columns[columns$has_units, , drop = FALSE]
  numeric_columns <- columns[!columns$has_units & columns$data_type == "numeric", , drop = FALSE]
  other_columns <- columns[!columns$has_units & columns$data_type != "numeric", , drop = FALSE]

  if (nrow(unit_columns) > 0L) {
    cli::cli_h2("Unit columns")
    for (index in seq_len(nrow(unit_columns))) {
      target <- unit_columns$target[[index]]
      label <- paste0(target, " [", unit_columns$unit[[index]], "]")
      cli::cli_text("{.strong {label}}")
      audit_report_print_story_lines(target, stories, indent = 1L, seen = target)
      cli::cli_text("")
    }
  }

  audit_report_print_column_definitions(
    "Unitless numeric columns", numeric_columns, lineage, " [no units]"
  )
  if (identical(trace, "all")) {
    audit_report_print_column_definitions("Other final columns", other_columns, lineage, NULL)
  }
  invisible()
}

#' @noRd
audit_report_print_column_definitions <- function(title, columns, lineage, suffix) {
  if (nrow(columns) == 0L) return(invisible())
  cli::cli_h2(title)
  for (index in seq_len(nrow(columns))) {
    target <- columns$target[[index]]
    label <- if (is.null(suffix)) target else paste0(target, suffix)
    cli::cli_text("{.strong {label}}")
    definitions <- lineage[lineage$target == target & lineage$relation == "definition", , drop = FALSE]
    sources <- lineage[lineage$target == target & lineage$relation == "source", , drop = FALSE]
    if (nrow(definitions) > 0L) {
      for (row in seq_len(nrow(definitions))) {
        cli::cli_verbatim(paste0(
          "  ", definitions$expression[[row]], " (in ", definitions$object[[row]], ")"
        ))
      }
    } else if (nrow(sources) > 0L) {
      cli::cli_verbatim(paste0("  ", sources$source_object[[1]], "$", sources$source_column[[1]]))
    } else {
      cli::cli_verbatim("  no static lineage captured")
    }
    cli::cli_text("")
  }
  invisible()
}

# One column's unit story. A derived line is followed by the story of each
# column it inherits units from, indented, so the chain reads in place; `seen`
# stops cycles.
#' @noRd
audit_report_print_story_lines <- function(target, stories, indent, seen) {
  prefix <- if (indent > 1L) paste0(target, ": ") else ""
  lines <- stories[stories$target == target, , drop = FALSE]
  lines <- lines[order(match(lines$kind, c("derived", "call", "spec"))), , drop = FALSE]
  if (nrow(lines) == 0L) {
    cli::cli_verbatim(paste0(
      strrep("  ", indent), prefix, "no unit evidence captured for this column"
    ))
    return(invisible())
  }
  for (index in seq_len(nrow(lines))) {
    cli::cli_verbatim(paste0(strrep("  ", indent), prefix, lines$line[[index]]))
    if (!identical(lines$kind[[index]], "derived")) next
    refs <- lines$refs[[index]]
    if (is.na(refs) || !nzchar(refs)) next
    for (ref in strsplit(refs, ",", fixed = TRUE)[[1]]) {
      if (ref %in% seen) next
      audit_report_print_story_lines(ref, stories, indent + 1L, c(seen, ref))
    }
  }
  invisible()
}

# Unit events that matched no tagged call or final column. Shown so the
# attribution join can never silently hide evidence.
#' @noRd
audit_report_print_residual <- function(residual) {
  if (length(residual) == 0L) return(invisible())
  cli::cli_h2("Unattributed unit operations")
  for (line in residual) cli::cli_verbatim(paste0("• ", line))
  invisible()
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
    assumed = "Assumed",
    unclassified = "Unclassified",
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
