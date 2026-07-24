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
    lines <- c(lines, cli::format_inline("{.alert-info scicalc.racen_config:}"), paste0("    ", format_racen_config(racen)))
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

# The full resolved racen mapping, one `label -> code` line per category.
format_racen_config <- function(config) {
  resolved <- tryCatch(resolve_racen_codes(config), error = function(e) NULL)
  if (is.null(resolved)) {
    return("set")
  }
  paste0(names(resolved), " -> ", unname(resolved))
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
  x[mask] <- NA
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
  result[combined] <- missing_value
  result
}
