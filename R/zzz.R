.onLoad <- function(libname, pkgname) {
  op <- options()
  op_scicalc <- list(
    scicalc.missing_value = -999
  )
  toset <- !(names(op_scicalc) %in% names(op))
  if (any(toset)) options(op_scicalc[toset])

  tryCatch(
    units::install_unit("bsa_ref", "1.73 m^2", "reference_bsa"),
    error = function(e) {
      # Already registered (e.g. during load_all/test cycling) — safe to ignore
    }
  )

  tryCatch(
    units::install_unit("U"),
    error = function(e) {
      # Already registered (e.g. during load_all/test cycling) — safe to ignore
    }
  )

  invisible()
}

.onUnload <- function(libpath) {
  tryCatch(
    units::remove_unit(symbol = "bsa_ref", name = "reference_bsa"),
    error = function(e) NULL
  )
  tryCatch(
    units::remove_unit(symbol = "U"),
    error = function(e) NULL
  )
}

.onAttach <- function(libname, pkgname) {
  scicalc_options_message()
}

scicalc_options_message <- function() {
  root <- scicalc_project_root()
  root_msg <- cli::format_inline("{.alert-info scicalc project root: {root}}")
  rule <- cli::rule(left = "scicalc options")
  msg <- cli::format_inline("{.alert-success scicalc.missing_value : {getOption('scicalc.missing_value', -999)}}")
  bsa_msg <- cli::format_inline("{.alert-info bsa_ref unit = 1.73 m^2}")
  u_msg <- cli::format_inline("{.alert-info U unit = enzyme activity (units/L as U/L)}")
  packageStartupMessage(root_msg)
  packageStartupMessage(rule)
  packageStartupMessage(msg)
  packageStartupMessage(bsa_msg)
  packageStartupMessage(u_msg)
}

check_mv_computation <- function(x, name) {
  mv <- getOption("scicalc.missing_value", -999)
  if (is.na(mv)) return(rep(FALSE, length(x)))
  mask <- x == mv
  mask[is.na(mask)] <- FALSE
  if (any(mask)) {
    rlang::warn(paste0(name, " contains missing value indicator (", mv, ")"))
  }
  mask
}

check_mv_reference <- function(x, name) {
  mv <- getOption("scicalc.missing_value", -999)
  if (is.na(mv)) return(x)
  hits <- x == mv
  hits[is.na(hits)] <- FALSE
  if (any(hits)) {
    rlang::warn(paste0(name, " contains missing value indicator (", mv, "). Related checks may be unreliable."))
    x[hits] <- NA
  }
  x
}

apply_mv_mask <- function(result, ...) {
  masks <- list(...)
  masks <- Filter(Negate(is.null), masks)
  if (length(masks) == 0) return(result)
  combined <- Reduce(`|`, masks)
  result[combined] <- getOption("scicalc.missing_value", -999)
  result
}
