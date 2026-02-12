.onLoad <- function(libname, pkgname) {
  op <- options()
  op_scicalc <- list(
    scicalc.missing_value = -999
  )
  toset <- !(names(op_scicalc) %in% names(op))
  if (any(toset)) options(op_scicalc[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  scicalc_options_message()
}

scicalc_options_message <- function() {
  rule <- cli::rule(left = "scicalc options")
  msg <- cli::format_inline("{.alert-success scicalc.missing_value : {getOption('scicalc.missing_value', -999)}}")
  packageStartupMessage(rule)
  packageStartupMessage(msg)
}

check_mv_computation <- function(x, name) {
  mv <- getOption("scicalc.missing_value", -999)
  if (is.na(mv)) return(rep(FALSE, length(x)))
  mask <- x == mv
  mask[is.na(mask)] <- FALSE
  if (any(mask)) {
    warning(name, " contains missing value indicator (", mv, ")")
  }
  mask
}

check_mv_reference <- function(x, name) {
  mv <- getOption("scicalc.missing_value", -999)
  if (is.na(mv)) return(invisible(NULL))
  hits <- x == mv
  hits[is.na(hits)] <- FALSE
  if (any(hits)) {
    warning(name, " contains missing value indicator (", mv, "). Related checks may be unreliable.")
  }
  invisible(NULL)
}

apply_mv_mask <- function(result, ...) {
  masks <- list(...)
  masks <- Filter(Negate(is.null), masks)
  if (length(masks) == 0) return(result)
  combined <- Reduce(`|`, masks)
  result[combined] <- getOption("scicalc.missing_value", -999)
  result
}
