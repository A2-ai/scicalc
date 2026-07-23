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
    units::remove_unit(symbol = "U", name = "enzyme_unit"),
    error = function(e) {
      # `U` may not have been registered in this R session.
    }
  )

  tryCatch(
    units::install_unit("U", "umol/min", "enzyme_unit"),
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
    units::remove_unit(symbol = "U", name = "enzyme_unit"),
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
  u_msg <- cli::format_inline("{.alert-info 1 U = 16.67 nkat (enzyme activity)}")
  packageStartupMessage(root_msg)
  packageStartupMessage(rule)
  packageStartupMessage(msg)
  packageStartupMessage(bsa_msg)
  packageStartupMessage(u_msg)
  for (line in scicalc_config_messages()) {
    packageStartupMessage(line)
  }
}

# Startup lines for any categorical config options that are set, under their
# own section rule. Empty when none are set.
scicalc_config_messages <- function() {
  lines <- character()

  racen <- getOption("scicalc.racen_config", NULL)
  if (!is.null(racen)) {
    text <- paste0("scicalc.racen_config: ", format_racen_config(racen))
    lines <- c(lines, cli::format_inline("{.alert-info {text}}"))
  }

  bands <- list(scicalc.agec_config = "age", scicalc.bmic_config = "bmi")
  for (opt in names(bands)) {
    config <- getOption(opt, NULL)
    if (!is.null(config)) {
      lines <- c(lines, cli::format_inline("{.alert-info {opt}:}"), paste0("    ", format_band_config(config, bands[[opt]])))
    }
  }

  if (length(lines) == 0) {
    return(character())
  }
  c(cli::rule(left = "Categorical Configurations"), lines)
}

format_racen_config <- function(config) {
  if (is.numeric(config) && !is.null(names(config))) {
    paste(paste0(names(config), "=", config), collapse = ", ")
  } else {
    "set"
  }
}

# One string per band: `min <= <var> < next-min -> label, [code]`, top band
# open-ended.
format_band_config <- function(config, var) {
  if (!is.data.frame(config) || !all(c("label", "min", "code") %in% names(config))) {
    return("set")
  }
  config <- config[order(config$min), , drop = FALSE]
  n <- nrow(config)
  vapply(seq_len(n), function(i) {
    bound <- if (i < n) paste0(" < ", config$min[i + 1]) else ""
    paste0(config$min[i], " <= ", var, bound, " -> ", config$label[i], ", [", config$code[i], "]")
  }, character(1))
}

check_mv_computation <- function(x, name) {
  mv <- getOption("scicalc.missing_value", -999)
  if (is.na(mv)) return(rep(FALSE, length(x)))
  mask <- is_missing_value(x, mv)
  if (any(mask)) {
    rlang::warn(paste0(name, " contains missing value indicator (", mv, ")"))
  }
  mask
}

# Replace the configured missing-value indicator with NA before an arithmetic
# calculation, retaining the row mask so the caller can restore the indicator
# on its result with apply_mv_mask().
#' @noRd
mask_missing_computation_input <- function(x, name) {
  mask <- check_mv_computation(x, name)
  if (inherits(x, "mixed_units")) {
    x[mask] <- lapply(x[mask], function(value) {
      value[] <- NA_real_
      value
    })
  } else {
    x[mask] <- NA
  }
  list(value = x, mask = mask)
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
  missing_value <- getOption("scicalc.missing_value", -999)
  if (inherits(result, "mixed_units")) {
    result[combined] <- lapply(result[combined], function(value) {
      value[] <- missing_value
      value
    })
  } else {
    result[combined] <- missing_value
  }
  result
}
