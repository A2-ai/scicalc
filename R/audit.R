# Assembly provenance audit log.
#
# Run-scoped, opt-in capture: unit and hashing functions only log while an
# audit run is active (the SCICALC_AUDITING env var is set). `audit_script()`
# re-runs an assembly script in a fresh subprocess with that var set, so a
# single clean run is captured to a named log file. Normal interactive work
# logs nothing. See PLAN.md.

# package-level state: cached log4r logger + its path (per process)
.audit <- new.env(parent = emptyenv())

#' Is an audit capture currently active?
#' @keywords internal
audit_active <- function() {
  nzchar(Sys.getenv("SCICALC_AUDITING", unset = ""))
}

#' Resolve the audit log file path for the active capture
#'
#' During an `audit_script()` run the driver sets `SCICALC_AUDIT_LOG`; that
#' wins. Otherwise falls back to `options(scicalc.audit_log=)` or a default.
#'
#' @keywords internal
audit_log_file <- function() {
  env <- Sys.getenv("SCICALC_AUDIT_LOG", unset = "")
  if (nzchar(env)) {
    return(env)
  }
  opt <- getOption("scicalc.audit_log", NULL)
  if (!is.null(opt)) {
    return(opt)
  }
  root <- if (requireNamespace("here", quietly = TRUE)) {
    tryCatch(here::here(), error = function(e) getwd())
  } else {
    getwd()
  }
  file.path(root, ".scicalc-logs", "audit.log")
}

#' Build (and cache) the audit logger for the active log path
#' @keywords internal
audit_logger <- function() {
  path <- audit_log_file()
  if (is.null(.audit$logger) || !identical(.audit$path, path)) {
    dir <- dirname(path)
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    .audit$path <- path
    .audit$logger <- log4r::logger(
      "INFO",
      appenders = list(
        log4r::file_appender(path, layout = log4r::json_log_layout())
      )
    )
  }
  .audit$logger
}

#' Append one event to the audit log (no-op unless a capture is active)
#'
#' @param event_type one of "ingest", "spec", "unit", "write".
#' @param ... structured fields for the event (scalars).
#' @keywords internal
log_audit_event <- function(event_type, ...) {
  if (!audit_active()) {
    return(invisible())
  }
  tryCatch(
    log4r::info(audit_logger(), event_type, ...),
    error = function(e) {
      if (is.null(.audit$warned)) {
        rlang::warn(paste0("scicalc audit logging failed: ", conditionMessage(e)))
        .audit$warned <- TRUE
      }
    }
  )
  invisible()
}

#' Audit an Assembly Script
#'
#' Re-runs a data-assembly script in a fresh subprocess with audit capture
#' enabled, recording a single clean provenance run (input/spec/output hashes
#' and every unit conversion) to a named log file. Quarto/R Markdown files are
#' converted to R with [knitr::purl()] first, then run with [callr::rscript()].
#'
#' Because capture is keyed to the `SCICALC_AUDITING` environment variable,
#' scicalc functions log only during this run — normal interactive work logs
#' nothing — and a nested `audit_script()` call inside the script is a no-op, so
#' a self-auditing script cannot recurse.
#'
#' @param script path to the assembly script (`.R`, `.qmd`, or `.Rmd`).
#' @param name audit name; the log is written to `<dir>/<name>.audit.log`.
#'   Defaults to the script's base name.
#' @param dir directory for the named audit log (default `.scicalc-logs`).
#'
#' @return the audit as a tibble (invisibly), read from the named log.
#'
#' @family audit
#' @export
#'
#' @examples \dontrun{
#' audit_script("assembly.qmd", name = "pk")
#' scicalc_audit("pk")
#' }
audit_script <- function(script, name = NULL, dir = ".scicalc-logs") {
  # re-entrancy guard: if we are already inside an audit run, do nothing
  if (audit_active()) {
    return(invisible(NULL))
  }
  checkmate::assert_file_exists(script, access = "r")
  rlang::check_installed("callr")

  if (is.null(name)) {
    name <- tools::file_path_sans_ext(basename(script))
  }
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  log_path <- normalizePath(
    file.path(dir, paste0(name, ".audit.log")),
    mustWork = FALSE
  )
  if (file.exists(log_path)) {
    file.remove(log_path)
  }

  ext <- tolower(tools::file_ext(script))
  run_file <- script
  if (ext %in% c("qmd", "rmd")) {
    rlang::check_installed("knitr")
    run_file <- tempfile(fileext = ".R")
    on.exit(unlink(run_file), add = TRUE)
    knitr::purl(script, output = run_file, quiet = TRUE)
  } else if (ext != "r") {
    rlang::abort(paste0(
      "Unsupported script type '.", ext, "'. Expected .R, .qmd, or .Rmd."
    ))
  }

  callr::rscript(
    run_file,
    libpath = .libPaths(),
    env = c(
      callr::rcmd_safe_env(),
      SCICALC_AUDITING = name,
      SCICALC_AUDIT_LOG = log_path
    ),
    show = TRUE
  )

  invisible(scicalc_audit(log_file = log_path))
}

#' Read a scicalc Assembly Audit Log
#'
#' Reads a provenance / chain-of-custody log written by [audit_script()] into a
#' tidy tibble. Each row is one event: an input file hash (`ingest`), a spec
#' hash (`spec`), a unit conversion (`unit`), or an output file hash (`write`).
#'
#' @param name audit name; reads `<dir>/<name>.audit.log`.
#' @param dir directory holding named logs (default `.scicalc-logs`).
#' @param log_file explicit path to a log file (overrides `name`/`dir`).
#'
#' @return a tibble of audit events (empty if the log does not exist).
#'
#' @family audit
#' @export
#'
#' @examples \dontrun{
#' scicalc_audit("pk")
#' }
scicalc_audit <- function(name = NULL, dir = ".scicalc-logs", log_file = NULL) {
  if (is.null(log_file)) {
    log_file <- if (!is.null(name)) {
      file.path(dir, paste0(name, ".audit.log"))
    } else {
      audit_log_file()
    }
  }
  if (!file.exists(log_file)) {
    rlang::inform("No scicalc audit log found.")
    return(tibble::tibble())
  }
  events <- jsonlite::stream_in(file(log_file), verbose = FALSE)
  events <- tibble::as_tibble(events)
  names(events)[names(events) == "1"] <- "event_type"
  events
}

#' Reset a scicalc Assembly Audit Log
#'
#' Deletes a named audit log and drops the cached logger.
#'
#' @param name audit name; deletes `<dir>/<name>.audit.log`.
#' @param dir directory holding named logs (default `.scicalc-logs`).
#' @param log_file explicit path (overrides `name`/`dir`).
#'
#' @return invisibly `NULL`.
#'
#' @family audit
#' @export
#'
#' @examples \dontrun{
#' scicalc_audit_reset("pk")
#' }
scicalc_audit_reset <- function(name = NULL, dir = ".scicalc-logs", log_file = NULL) {
  if (is.null(log_file)) {
    log_file <- if (!is.null(name)) {
      file.path(dir, paste0(name, ".audit.log"))
    } else {
      audit_log_file()
    }
  }
  if (file.exists(log_file)) {
    file.remove(log_file)
  }
  .audit$logger <- NULL
  .audit$path <- NULL
  .audit$warned <- NULL
  invisible()
}
