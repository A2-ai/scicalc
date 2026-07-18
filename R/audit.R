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

#' Does a directory hold a project-root marker?
#'
#' An existing `.scicalc-logs` (so logs stay put once started), an RStudio
#' `.Rproj`, a `.git` dir/file, or a `.here` file.
#' @keywords internal
has_root_marker <- function(dir) {
  dir.exists(file.path(dir, ".scicalc-logs")) ||
    file.exists(file.path(dir, ".here")) ||
    file.exists(file.path(dir, ".git")) ||
    length(list.files(dir, pattern = "[.]Rproj$")) > 0
}

#' Project root for audit logs
#'
#' Walks up from `start` (the working directory) and returns the nearest
#' ancestor holding a root marker (see [has_root_marker()]); falls back to
#' `start` if none is found.
#' @keywords internal
scicalc_project_root <- function(start = getwd()) {
  dir <- normalizePath(start, mustWork = FALSE)
  repeat {
    if (has_root_marker(dir)) {
      return(dir)
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      break # reached the filesystem root
    }
    dir <- parent
  }
  start
}

#' Default audit log directory (`.scicalc-logs` at the project root)
#' @keywords internal
default_audit_dir <- function() {
  file.path(scicalc_project_root(), ".scicalc-logs")
}

#' Express a path relative to the project root for audit logging
#'
#' Portable, machine-independent paths in the audit (e.g.
#' `data/derived/pk.parquet`). Files outside the root get a `../` relative
#' path. `NULL`/`NA`/empty return `NA`.
#' @keywords internal
audit_rel_path <- function(path) {
  if (is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(NA_character_)
  }
  root <- scicalc_project_root()
  tryCatch(
    as.character(fs::path_rel(fs::path_abs(path), start = root)),
    error = function(e) path
  )
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
  file.path(default_audit_dir(), "audit.log")
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
#' @param event_type one of "run", "ingest", "spec", "unit", "write".
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
#' @param dir directory for the named audit log (default: `.scicalc-logs` at the project root).
#' @param overwrite if `FALSE` (default), error rather than replace an existing
#'   audit log of the same name. Pass `TRUE` to re-run and replace it.
#' @param quiet if `TRUE`, suppress the assembly script's console output. The
#'   error message on a failed run still includes the script's error.
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
audit_script <- function(script, name = NULL, dir = default_audit_dir(), overwrite = FALSE, quiet = FALSE) {
  # re-entrancy guard: if we are already inside an audit run, do nothing
  if (audit_active()) {
    return(invisible(NULL))
  }
  checkmate::assert_file_exists(script, access = "r")
  checkmate::assert_flag(overwrite)
  checkmate::assert_flag(quiet)
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
    if (!overwrite) {
      rlang::abort(paste0(
        "Audit log already exists: ", log_path, "\n",
        "Pass `overwrite = TRUE` to re-run and replace it."
      ))
    }
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

  audit_log_run_event(log_path, name, script, ext, phase = "started")

  res <- callr::rscript(
    run_file,
    libpath = .libPaths(),
    wd = getwd(),
    env = c(
      callr::rcmd_safe_env(),
      SCICALC_AUDITING = name,
      SCICALC_AUDIT_LOG = log_path
    ),
    show = !quiet,
    fail_on_status = FALSE
  )

  if (!is.null(res$status) && res$status != 0L) {
    audit_log_run_event(log_path, name, script, ext, phase = "failed")
    detail <- if (!is.null(res$stderr) && nzchar(res$stderr)) {
      paste0("\n", res$stderr)
    } else {
      " See the script's output above."
    }
    rlang::abort(paste0(
      "The audited assembly script failed (exit status ", res$status, ").",
      detail
    ))
  }

  audit_log_run_event(log_path, name, script, ext, phase = "completed")

  invisible(scicalc_audit(log_file = log_path))
}

# Write a run-manifest event from the parent process. The child process writes
# the assembly events; these bookends identify the exact audited script and
# whether that run completed.
#' @noRd
audit_log_run_event <- function(log_path, name, script, script_type, phase) {
  prior_active <- Sys.getenv("SCICALC_AUDITING", unset = "")
  prior_log <- Sys.getenv("SCICALC_AUDIT_LOG", unset = "")
  on.exit({
    Sys.setenv(SCICALC_AUDITING = prior_active, SCICALC_AUDIT_LOG = prior_log)
  }, add = TRUE)

  Sys.setenv(SCICALC_AUDITING = name, SCICALC_AUDIT_LOG = log_path)
  log_audit_event(
    "run",
    fn = "audit_script",
    phase = phase,
    script = audit_rel_path(script),
    script_hash = digest::digest(file = script, algo = "blake3"),
    script_type = script_type,
    scicalc_version = as.character(utils::packageVersion("scicalc")),
    r_version = paste(R.version$major, R.version$minor, sep = ".")
  )
}

#' Read a scicalc Assembly Audit Log
#'
#' Reads a provenance / chain-of-custody log written by [audit_script()] into a
#' tidy tibble. Each row is one event: an input file hash (`ingest`), a spec
#' hash (`spec`), a unit conversion (`unit`), or an output file hash (`write`).
#'
#' @param name audit name; reads `<dir>/<name>.audit.log`.
#' @param dir directory holding named logs (default: `.scicalc-logs` at the project root).
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
scicalc_audit <- function(name = NULL, dir = default_audit_dir(), log_file = NULL) {
  if (is.null(log_file)) {
    log_file <- if (!is.null(name)) {
      file.path(dir, paste0(name, ".audit.log"))
    } else {
      audit_log_file()
    }
  }
  if (!file.exists(log_file)) {
    rlang::abort(paste0(
      "No scicalc audit log found at: ",
      normalizePath(log_file, mustWork = FALSE), "\n",
      "Run `audit_script()` to produce it (from the same working directory), ",
      "and check the `name`/`dir`."
    ))
  }
  events <- jsonlite::stream_in(file(log_file), verbose = FALSE)
  events <- tibble::as_tibble(events)
  names(events)[names(events) == "1"] <- "event_type"

  # log4r's severity level and timestamp are not meaningful for a single run
  events[["level"]] <- NULL
  events[["time"]] <- NULL
  # drop columns that are empty for this run (e.g. spec_file when the spec
  # carries no source path)
  keep <- vapply(events, function(col) !all(is.na(col)), logical(1))
  events <- events[, keep, drop = FALSE]
  # sensible column order: what happened, via which function, then details
  preferred <- c(
    "event_type", "phase", "fn", "script", "script_hash", "script_type",
    "scicalc_version", "r_version", "input", "from", "to", "transform", "n",
    "detail", "file", "hash", "algo", "spec_hash", "spec_file"
  )
  ord <- c(intersect(preferred, names(events)), setdiff(names(events), preferred))
  events[, ord, drop = FALSE]
}

#' Reset a scicalc Assembly Audit Log
#'
#' Deletes a named audit log and drops the cached logger.
#'
#' @param name audit name; deletes `<dir>/<name>.audit.log`.
#' @param dir directory holding named logs (default: `.scicalc-logs` at the project root).
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
scicalc_audit_reset <- function(name = NULL, dir = default_audit_dir(), log_file = NULL) {
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
