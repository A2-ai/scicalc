# Assembly provenance audit log.
#
# An out-of-band, append-only log (via log4r) that records unit provenance and
# file/spec hashes across a data assembly. It lives off the data frame so no
# dplyr/base operation can silently strip it, and in a single persistent
# project-level file so an assembly spanning multiple R sessions is never
# fragmented. See PLAN.md.

# package-level state: the log4r logger and resolved log file path
.audit <- new.env(parent = emptyenv())

#' Resolve the audit log file path
#'
#' `options(scicalc.audit_log = <path>)` overrides; otherwise a single
#' persistent file at `<project root>/.scicalc-logs/audit.log`.
#'
#' @return a file path.
#' @keywords internal
audit_log_file <- function() {
  p <- getOption("scicalc.audit_log", NULL)
  if (!is.null(p)) {
    return(p)
  }
  root <- tryCatch(here::here(), error = function(e) getwd())
  file.path(root, ".scicalc-logs", "audit.log")
}

#' Build (and cache) the audit logger
#'
#' Lazily creates the log directory and a log4r JSON file appender on first
#' use, so setting `options(scicalc.audit_log=)` before the first event is
#' respected.
#'
#' @return a log4r logger, or `NULL` if auditing is disabled.
#' @keywords internal
audit_logger <- function() {
  if (isTRUE(getOption("scicalc.no_audit", FALSE))) {
    return(NULL)
  }
  if (is.null(.audit$logger)) {
    log_file <- audit_log_file()
    dir <- dirname(log_file)
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    .audit$log_file <- log_file
    .audit$logger <- log4r::logger(
      "INFO",
      appenders = list(
        log4r::file_appender(log_file, layout = log4r::json_log_layout())
      )
    )
  }
  .audit$logger
}

#' Append one event to the audit log
#'
#' Never breaks the caller: a logging failure warns once rather than erroring,
#' so data processing is unaffected.
#'
#' @param event_type one of "ingest", "spec", "unit", "write".
#' @param ... structured fields for the event (scalars).
#' @keywords internal
log_audit_event <- function(event_type, ...) {
  logger <- audit_logger()
  if (is.null(logger)) {
    return(invisible())
  }
  tryCatch(
    log4r::info(logger, event_type, ...),
    error = function(e) {
      if (is.null(.audit$warned)) {
        rlang::warn(paste0("scicalc audit logging failed: ", conditionMessage(e)))
        .audit$warned <- TRUE
      }
    }
  )
  invisible()
}

#' Read the scicalc Assembly Audit Log
#'
#' Reads the provenance / chain-of-custody log written by scicalc's unit and
#' hashing functions into a tidy tibble. Each row is one event: an input file
#' hash (`ingest`), a spec hash (`spec`), a unit conversion (`unit`), or an
#' output file hash (`write`).
#'
#' @param log_file path to the audit log. Defaults to the active log
#'   (`options(scicalc.audit_log=)`, else `<project root>/.scicalc-logs/audit.log`).
#'
#' @return a tibble of audit events (empty if no log exists).
#'
#' @family audit
#' @export
#'
#' @examples \dontrun{
#' scicalc_audit()
#' }
scicalc_audit <- function(log_file = audit_log_file()) {
  if (!file.exists(log_file)) {
    rlang::inform("No scicalc audit log found.")
    return(tibble::tibble())
  }
  events <- jsonlite::stream_in(file(log_file), verbose = FALSE)
  events <- tibble::as_tibble(events)
  # log4r writes the positional message (our event type) as column "1"
  names(events)[names(events) == "1"] <- "event_type"
  events
}

#' Reset the scicalc Assembly Audit Log
#'
#' Deletes the current audit log file and drops the cached logger so the next
#' event starts a fresh log. Use at the start of a new assembly.
#'
#' @param log_file path to the audit log (see [scicalc_audit()]).
#'
#' @return invisibly `NULL`.
#'
#' @family audit
#' @export
#'
#' @examples \dontrun{
#' scicalc_audit_reset()
#' }
scicalc_audit_reset <- function(log_file = audit_log_file()) {
  if (file.exists(log_file)) {
    file.remove(log_file)
  }
  .audit$logger <- NULL
  .audit$warned <- NULL
  invisible()
}
