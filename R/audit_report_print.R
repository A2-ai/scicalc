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
  if (identical(x$run$phase[[1]], "failed")) {
    audit_report_print_failure(x$run)
    errors <- x$findings[x$findings$severity == "error", , drop = FALSE]
    if (nrow(errors) > 0) {
      audit_report_print_findings(errors)
    }
    return(invisible(x))
  }
  audit_report_print_files(x$files)
  if (nrow(x$columns) > 0L) {
    audit_report_print_unit_stories(x$columns, x$lineage, x$units$stories, x$trace, x$spec_units)
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
#' Emits the report as HTML rather than console text. It reads the same
#' already-built report object as `print()` (its log file read and events
#' transformed by [scicalc_audit_report()]) and reuses the same content builders
#' (`audit_report_story_lines()` and `audit_report_column_definition_lines()`),
#' so the knitted and console reports carry identical information, differently
#' styled.
#'
#' @param x A `scicalc_audit_report`.
#' @param ... Unused.
#'
#' @return A `knitr::asis_output` HTML block.
#' @exportS3Method knitr::knit_print
knit_print.scicalc_audit_report <- function(x, ...) {
  head <- c(
    audit_html_heading("scicalc audit"),
    audit_html_para(paste0("<strong>Status:</strong> ", audit_html_escape(x$overview$status[[1]]))),
    audit_report_knit_run(x$run)
  )
  if (identical(x$run$phase[[1]], "failed")) {
    errors <- x$findings[x$findings$severity == "error", , drop = FALSE]
    out <- c(
      head,
      audit_report_knit_failure(x$run),
      if (nrow(errors) > 0) audit_report_knit_findings(errors)
    )
    return(knitr::asis_output(paste(out, collapse = "\n")))
  }
  out <- c(
    head,
    audit_report_knit_files(x$files),
    if (nrow(x$columns) > 0L) {
      c(
        audit_report_knit_unit_stories(x$columns, x$lineage, x$units$stories, x$trace, x$spec_units),
        audit_report_knit_residual(x$units$residual)
      )
    } else {
      audit_report_knit_evidence(x$transformations)
    },
    audit_report_knit_findings(x$findings)
  )
  knitr::asis_output(paste(out, collapse = "\n"))
}

# HTML fragment helpers ------------------------------------------------------

audit_html_escape <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  gsub(">", "&gt;", text, fixed = TRUE)
}

# A section label as its own block, plus a trailing blank line so pandoc treats
# the following block separately.
audit_html_heading <- function(text) {
  c(paste0("<p><strong>", audit_html_escape(text), "</strong></p>"), "")
}

audit_html_para <- function(html) {
  c(paste0("<p>", html, "</p>"), "")
}

# Text lines in a monospace block, HTML-escaped, indentation preserved. Empty
# input yields nothing.
audit_html_pre <- function(lines) {
  if (length(lines) == 0L) return(character())
  c("<pre>", audit_html_escape(lines), "</pre>", "")
}

# knitr section renderers ----------------------------------------------------

audit_report_knit_run <- function(run) {
  if (is.na(run$phase[[1]])) return(character())
  script <- run$script[[1]]
  script <- if (is.na(script) || !nzchar(script)) "<script path not recorded>" else script
  hash <- run$script_hash[[1]]
  hash <- if (is.na(hash) || !nzchar(hash)) "<hash not recorded>" else substr(hash, 1, 12)
  c(
    audit_html_heading("Run"),
    audit_html_pre(c(
      paste0("Script: ", script, " [blake3:", hash, "]"),
      paste0("Capture: ", run$phase[[1]], "; scicalc ", run$scicalc_version[[1]], "; R ", run$r_version[[1]])
    ))
  )
}

audit_report_knit_failure <- function(run) {
  error <- run$error[[1]]
  if (is.na(error) || !nzchar(error)) return(character())
  c(
    audit_html_heading("Failure"),
    audit_html_pre(strsplit(error, "\n", fixed = TRUE)[[1]])
  )
}

audit_report_knit_files <- function(files) {
  out <- character()
  for (role in c("input", "specification", "output")) {
    rows <- files[files$role == role, , drop = FALSE]
    if (nrow(rows) == 0) next
    lines <- character()
    for (i in seq_len(nrow(rows))) {
      file <- rows$file[[i]]
      file <- if (is.na(file) || !nzchar(file)) "<path not recorded>" else file
      hash <- rows$hash[[i]]
      hash <- if (is.na(hash) || !nzchar(hash)) "<hash not recorded>" else substr(hash, 1, 12)
      algo <- rows$algo[[i]]
      algo <- if (is.na(algo) || !nzchar(algo)) "hash" else algo
      lines <- c(lines, paste0(file, " [", algo, ":", hash, "]"))
    }
    title <- paste0(tools::toTitleCase(role), if (nrow(rows) > 1) "s" else "")
    out <- c(out, audit_html_heading(title), audit_html_pre(lines))
  }
  out
}

audit_report_knit_unit_stories <- function(columns, lineage, stories, trace, spec_units = NULL) {
  unit_columns <- columns[columns$has_units, , drop = FALSE]
  numeric_columns <- columns[!columns$has_units & columns$data_type == "numeric", , drop = FALSE]
  other_columns <- columns[!columns$has_units & columns$data_type != "numeric", , drop = FALSE]

  out <- character()
  if (nrow(unit_columns) > 0L) {
    out <- c(out, audit_html_heading("Unit columns"))
    for (index in seq_len(nrow(unit_columns))) {
      target <- unit_columns$target[[index]]
      label <- paste0(target, " [", unit_columns$unit[[index]], "]")
      out <- c(
        out,
        audit_html_para(paste0("<strong>", audit_html_escape(label), "</strong>")),
        audit_html_pre(audit_report_story_lines(target, stories))
      )
    }
  }
  out <- c(out, audit_report_knit_column_definitions("Unitless numeric columns", numeric_columns, lineage, " [no units]", spec_units))
  if (identical(trace, "all")) {
    out <- c(out, audit_report_knit_column_definitions("Other final columns", other_columns, lineage, NULL, spec_units))
  }
  out
}

audit_report_knit_column_definitions <- function(title, columns, lineage, suffix, spec_units = NULL) {
  if (nrow(columns) == 0L) return(character())
  out <- audit_html_heading(title)
  for (index in seq_len(nrow(columns))) {
    target <- columns$target[[index]]
    label <- if (is.null(suffix)) target else paste0(target, suffix)
    label <- paste0(label, audit_report_spec_expects(target, spec_units))
    out <- c(
      out,
      audit_html_para(paste0("<strong>", audit_html_escape(label), "</strong>")),
      audit_html_pre(audit_report_column_definition_lines(target, lineage))
    )
  }
  out
}

audit_report_knit_residual <- function(residual) {
  if (length(residual) == 0L) return(character())
  c(
    audit_html_heading("Unattributed unit operations"),
    audit_html_pre(paste0("\u2022 ", residual))
  )
}

audit_report_knit_evidence <- function(transformations) {
  out <- audit_html_heading("Runtime unit operations")
  if (nrow(transformations) == 0) {
    return(c(out, audit_html_para("No unit transformations were recorded.")))
  }
  levels <- audit_report_evidence_levels(transformations$evidence)
  for (evidence in levels) {
    rows <- transformations[transformations$evidence == evidence, , drop = FALSE]
    lines <- character()
    for (i in seq_len(nrow(rows))) {
      text <- audit_report_transformation_text(rows[i, , drop = FALSE])
      basis <- rows$basis[[i]]
      if (!is.na(basis) && nzchar(basis)) text <- paste0(text, " \u2014 ", basis)
      lines <- c(lines, paste0("\u2022 ", text))
    }
    out <- c(
      out,
      audit_html_para(paste0("<strong>", audit_html_escape(audit_report_evidence_label(evidence)), "</strong>")),
      audit_html_pre(lines)
    )
  }
  out
}

audit_report_knit_findings <- function(findings) {
  out <- audit_html_heading("Findings")
  if (nrow(findings) == 0) {
    return(c(out, audit_html_para("No failed conversions or missing file anchors were recorded.")))
  }
  lines <- character()
  for (i in seq_len(nrow(findings))) {
    label <- paste0(toupper(findings$severity[[i]]), ": ", findings$finding[[i]])
    if (!is.na(findings$detail[[i]])) label <- paste0(label, " ", findings$detail[[i]])
    lines <- c(lines, paste0("\u2022 ", label))
  }
  c(out, audit_html_pre(lines))
}

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

audit_report_print_failure <- function(run) {
  error <- run$error[[1]]
  if (is.na(error) || !nzchar(error)) return(invisible())
  cli::cli_h2("Failure")
  for (line in strsplit(error, "\n", fixed = TRUE)[[1]]) {
    cli::cli_verbatim(line)
  }
  invisible()
}

# Per-column unit provenance: the section a reviewer reads to see where each
# final column's units came from.
# " (spec expects: <unit>)" when the spec declared a unit for a column that
# reached final unitless; "" otherwise.
audit_report_spec_expects <- function(target, spec_units) {
  if (is.null(spec_units) || nrow(spec_units) == 0L) return("")
  unit <- spec_units$unit[spec_units$target == target]
  if (length(unit) == 0L || is.na(unit[[1]]) || !nzchar(unit[[1]])) return("")
  paste0(" (spec expects: ", unit[[1]], ")")
}

audit_report_print_unit_stories <- function(columns, lineage, stories, trace, spec_units = NULL) {
  unit_columns <- columns[columns$has_units, , drop = FALSE]
  numeric_columns <- columns[!columns$has_units & columns$data_type == "numeric", , drop = FALSE]
  other_columns <- columns[!columns$has_units & columns$data_type != "numeric", , drop = FALSE]

  if (nrow(unit_columns) > 0L) {
    cli::cli_h2("Unit columns")
    for (index in seq_len(nrow(unit_columns))) {
      target <- unit_columns$target[[index]]
      label <- paste0(target, " [", unit_columns$unit[[index]], "]")
      cli::cli_text("{.strong {label}}")
      audit_report_print_story_lines(target, stories)
      cli::cli_text("")
    }
  }

  audit_report_print_column_definitions(
    "Unitless numeric columns", numeric_columns, lineage, " [no units]", spec_units
  )
  if (identical(trace, "all")) {
    audit_report_print_column_definitions("Other final columns", other_columns, lineage, NULL, spec_units)
  }
  invisible()
}

audit_report_print_column_definitions <- function(title, columns, lineage, suffix, spec_units = NULL) {
  if (nrow(columns) == 0L) return(invisible())
  cli::cli_h2(title)
  for (index in seq_len(nrow(columns))) {
    target <- columns$target[[index]]
    label <- if (is.null(suffix)) target else paste0(target, suffix)
    label <- paste0(label, audit_report_spec_expects(target, spec_units))
    cli::cli_text("{.strong {label}}")
    for (line in audit_report_column_definition_lines(target, lineage)) {
      cli::cli_verbatim(paste0("  ", line))
    }
    cli::cli_text("")
  }
  invisible()
}

audit_report_print_story_lines <- function(target, stories) {
  for (line in audit_report_story_lines(target, stories)) cli::cli_verbatim(line)
  invisible()
}

# ---------------------------------------------------------------------------
# Medium-agnostic content builders. These produce the text a reader sees; the
# console (cli) and knitr (HTML) renderers below both consume them, so the two
# media stay in sync and the story-nesting/lineage logic is written once.

# One column's unit story as indented text lines. A derived line is followed by
# the story of each column it inherits units from, indented, so the chain reads
# in place; `seen` stops cycles.
audit_report_story_lines <- function(target, stories, indent = 1L, seen = target) {
  prefix <- if (indent > 1L) paste0(target, ": ") else ""
  lines <- stories[stories$target == target, , drop = FALSE]
  lines <- lines[order(match(lines$kind, c("pivot", "derived", "call", "spec"))), , drop = FALSE]
  if (nrow(lines) == 0L) {
    return(paste0(strrep("  ", indent), prefix, "no unit evidence captured for this column"))
  }
  out <- character()
  for (index in seq_len(nrow(lines))) {
    out <- c(out, paste0(strrep("  ", indent), prefix, lines$line[[index]]))
    if (!identical(lines$kind[[index]], "derived")) next
    refs <- lines$refs[[index]]
    if (is.na(refs) || !nzchar(refs)) next
    for (ref in strsplit(refs, ",", fixed = TRUE)[[1]]) {
      if (ref %in% seen) next
      out <- c(out, audit_report_story_lines(ref, stories, indent + 1L, c(seen, ref)))
    }
  }
  out
}

# The definition/source lines for a unitless column, from the static lineage.
audit_report_column_definition_lines <- function(target, lineage) {
  definitions <- lineage[lineage$target == target & lineage$relation == "definition", , drop = FALSE]
  sources <- lineage[lineage$target == target & lineage$relation == "source", , drop = FALSE]
  if (nrow(definitions) > 0L) {
    return(paste0(definitions$expression, " (in ", definitions$object, ")"))
  }
  if (nrow(sources) > 0L) {
    return(paste0(sources$source_object[[1]], "$", sources$source_column[[1]]))
  }
  "no static lineage captured"
}

# Unit events that matched no tagged call or final column. Shown so the
# attribution join can never silently hide evidence.
audit_report_print_residual <- function(residual) {
  if (length(residual) == 0L) return(invisible())
  cli::cli_h2("Unattributed unit operations")
  for (line in residual) cli::cli_verbatim(paste0("\u2022 ", line))
  invisible()
}

audit_report_print_evidence <- function(transformations, max_transformations) {
  cli::cli_h2("Runtime unit operations")
  if (nrow(transformations) == 0) {
    cli::cli_text("No unit transformations were recorded.")
    return(invisible())
  }

  levels <- audit_report_evidence_levels(transformations$evidence)

  for (evidence in levels) {
    rows <- transformations[transformations$evidence == evidence, , drop = FALSE]
    cli::cli_h3(audit_report_evidence_label(evidence))
    shown <- if (is.infinite(max_transformations)) rows else utils::head(rows, max_transformations)
    cli::cli_ul()
    for (i in seq_len(nrow(shown))) {
      text <- audit_report_transformation_text(shown[i, , drop = FALSE])
      basis <- shown$basis[[i]]
      if (!is.na(basis) && nzchar(basis)) text <- paste0(text, " \u2014 ", basis)
      cli::cli_li(text)
    }
    cli::cli_end()
    if (nrow(rows) > nrow(shown)) {
      cli::cli_text("{.comment {nrow(rows) - nrow(shown)} more transformation(s) in this evidence class.}")
    }
  }
  invisible()
}

# Evidence classes in reading order, unknown classes appended.
audit_report_evidence_levels <- function(evidence) {
  order <- c("source-recorded", "carried-converted", "analyst-declared", "assumed", "unclassified", "failed")
  present <- unique(evidence)
  c(intersect(order, present), setdiff(present, order))
}

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
