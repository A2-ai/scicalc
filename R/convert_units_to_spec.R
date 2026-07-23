#' Convert Data Frame Columns to Spec Units
#'
#' @description
#' Converts each unit-carrying column of `data` to the unit declared for it in
#' a data specification. Columns that are plain numeric get the spec unit
#' attached (assuming the values are already in that unit) with a warning.
#'
#' Every column is attempted, then, if any column's current units could not be
#' converted to its spec unit, the function aborts and lists all offenders. A
#' failed conversion would leave a column carrying the wrong units in the
#' written dataset, so it stops the assembly rather than warning; the per-column
#' failures are still recorded in the audit log for a run captured by
#' [audit_script()].
#'
#' Pairs with [pivot_with_units()]: pivot attaches source units, then
#' `convert_units_to_spec()` harmonizes them to the specification.
#'
#' @param data a data frame.
#' @param spec a data specification object; currently a `yspec` object.
#' @param ... reserved for methods.
#'
#' @return `data` with columns converted (values rescaled) or assigned units
#'   per the spec. Columns not in the spec, or with no unit in the spec, are
#'   returned untouched. Errors if any spec conversion fails.
#'
#' @family unit_checking
#' @export
#'
#' @examples
#' \dontrun{
#' spec <- yspec::ys_load("analysis.yml")
#' df <- convert_units_to_spec(df, spec)
#' }
convert_units_to_spec <- function(data, spec, ...) {
  UseMethod("convert_units_to_spec", spec)
}

#' @rdname convert_units_to_spec
#' @export
convert_units_to_spec.yspec <- function(data, spec, ...) {
  rlang::check_installed("yspec")
  # the caller's data expression: lets the audit report tie this invocation to
  # the assignment (if any) whose result it produced
  context <- deparse1(substitute(data))
  spec_file <- attr(spec, "meta")$spec_file
  log_audit_event(
    "spec",
    fn = "convert_units_to_spec",
    spec_hash = digest::digest(spec, algo = "blake3"),
    spec_file = audit_rel_path(spec_file),
    context = context
  )
  unit_map <- unlist(yspec::ys_get_unit(spec))
  convert_units_to_map(data, unit_map, context = context)
}

#' @rdname convert_units_to_spec
#' @export
convert_units_to_spec.default <- function(data, spec, ...) {
  rlang::abort(paste0(
    "No `convert_units_to_spec()` method for class <",
    paste(class(spec), collapse = "/"),
    ">."
  ))
}

convert_units_to_map <- function(data, unit_map, context = NA_character_) {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(unit_map, names = "named")

  unit_map <- unit_map[!is.na(unit_map) & unit_map != ""]
  unit_map <- unit_map[names(unit_map) %in% colnames(data)]

  attached <- character(0)
  failed <- character(0)

  for (col in names(unit_map)) {
    target <- unit_map[[col]]
    tgt_log <- parse_log_spec(target)
    missing_mask <- is_missing_value(data[[col]])
    n_col <- sum(!is.na(as.numeric(data[[col]])) & !missing_mask)

    if (inherits(data[[col]], "units")) {
      current <- as.character(units(data[[col]]))
      src_log <- parse_log_unit(current)

      if (!is.null(src_log) || !is.null(tgt_log)) {
        # at least one side is a log unit: reconcile via a reference shift
        shifted <- shift_log_column(data[[col]], src_log, tgt_log)
        if (is.null(shifted)) {
          failed <- c(failed, paste0(col, " [", current, "] -> [", target, "]"))
          log_unit_conversion(col, current, target, "failed", n_col, "failed", "conversion to specification failed", context)
        } else {
          data[[col]] <- apply_mv_mask(
            restore_attrs(shifted, data[[col]]), missing_mask
          )
          log_unit_conversion(col, current, as.character(units(shifted)), "log-shift", n_col, "carried-converted", "input column carried units", context)
        }
      } else {
        converted <- tryCatch(
          units::set_units(data[[col]], target, mode = "standard"),
          error = function(e) NULL
        )
        if (is.null(converted)) {
          failed <- c(failed, paste0(col, " [", current, "] -> [", target, "]"))
          log_unit_conversion(col, current, target, "failed", n_col, "failed", "conversion to specification failed", context)
        } else {
          data[[col]] <- apply_mv_mask(
            restore_attrs(converted, data[[col]]), missing_mask
          )
          log_unit_conversion(col, current, as.character(units(converted)), "convert", n_col, "carried-converted", "input column carried units", context)
        }
      }
    } else if (is.numeric(data[[col]])) {
      if (!is.null(tgt_log)) {
        # plain numeric assumed already log-transformed on the target basis
        with_unit <- tryCatch(
          {
            tmp <- data[[col]]
            units(tmp) <- tgt_log$unit
            tmp
          },
          error = function(e) NULL
        )
        if (is.null(with_unit)) {
          failed <- c(failed, paste0(col, " [unitless] -> [", target, "]"))
          log_unit_conversion(col, NA_character_, target, "failed", n_col, "failed", "unit attachment from specification failed", context)
        } else {
          data[[col]] <- apply_mv_mask(
            restore_attrs(with_unit, data[[col]]), missing_mask
          )
          attached <- c(attached, paste0(col, " [", target, "]"))
          log_unit_conversion(col, NA_character_, as.character(units(with_unit)), "attach", n_col, "assumed", "unitless numeric labelled from specification", context)
        }
      } else {
        with_unit <- tryCatch(
          units::set_units(data[[col]], target, mode = "standard"),
          error = function(e) NULL
        )
        if (is.null(with_unit)) {
          failed <- c(failed, paste0(col, " [unitless] -> [", target, "]"))
          log_unit_conversion(col, NA_character_, target, "failed", n_col, "failed", "unit attachment from specification failed", context)
        } else {
          data[[col]] <- apply_mv_mask(
            restore_attrs(with_unit, data[[col]]), missing_mask
          )
          attached <- c(attached, paste0(col, " [", target, "]"))
          log_unit_conversion(col, NA_character_, as.character(units(with_unit)), "attach", n_col, "assumed", "unitless numeric labelled from specification", context)
        }
      }
    }
  }

  if (length(attached) > 0) {
    rlang::warn(paste0(
      "Attached spec units to unitless column(s): ",
      paste(attached, collapse = ", ")
    ))
  }

  # A failed spec conversion means a column would keep the wrong units in the
  # written dataset. Abort rather than warn so the assembly stops here; the
  # per-column "failed" events were already logged above, so an audit of the
  # aborted run still shows which columns failed. Every column is attempted
  # first, so the message lists all offenders at once.
  if (length(failed) > 0) {
    rlang::abort(paste0(
      "Could not convert column(s) to spec units:\n",
      paste0("  ", failed, collapse = "\n")
    ))
  }

  data
}

#' Log one `convert_units_to_spec()` per-column conversion event
#'
#' No-op conversions and log reference shifts (already in the target unit) are
#' not logged, to keep the audit focused on columns that actually changed.
#' @noRd
log_unit_conversion <- function(col, from, to, transform, n, evidence, basis, context = NA_character_) {
  if (transform %in% c("convert", "log-shift") && !is.na(from) && identical(from, to)) {
    return(invisible())
  }
  log_audit_event(
    "unit",
    fn = "convert_units_to_spec",
    input = col,
    from = from,
    to = to,
    transform = transform,
    detail = NA_character_,
    evidence = evidence,
    basis = basis,
    n = n,
    context = context
  )
}

#' Parse a udunits logarithmic-unit deparse
#'
#' Splits e.g. `"ln(re 1e-06 m-3.kg)"` into its log base symbol, the numeric
#' coefficient of the (SI-reduced) reference level, and the reference dimension.
#'
#' @param unit_str character deparse of a unit.
#' @return a list(base, coef, dim), or `NULL` if `unit_str` is not a log unit.
#' @noRd
parse_log_unit <- function(unit_str) {
  m <- regmatches(unit_str, regexec("^(ln|lg|lb)[(]re (.+)[)]$", unit_str))[[1]]
  if (length(m) == 0) {
    return(NULL)
  }
  base <- m[2]
  ref <- m[3]

  nm <- regmatches(ref, regexec("^([0-9.eE+-]+)[ ](.+)$", ref))[[1]]
  if (length(nm) == 0) {
    coef <- 1
    dim <- ref
  } else {
    coef <- as.numeric(nm[2])
    dim <- nm[3]
  }

  list(base = base, coef = coef, dim = dim)
}

#' Parse a spec log-unit string into a target template
#'
#' Recognizes `log(<u>)` (natural), `log10(<u>)`/`lg(<u>)`, and `log2(<u>)`/
#' `lb(<u>)`, builds a `units` template carrying the corresponding log unit, and
#' returns its parsed components plus the template's units object.
#'
#' @param spec_unit character spec unit string.
#' @return a list(base, coef, dim, unit), or `NULL` if `spec_unit` is not a log
#'   spec or its inner unit is unparseable.
#' @noRd
parse_log_spec <- function(spec_unit) {
  m <- regmatches(
    spec_unit,
    regexec("^(log10|log2|log|ln|lg|lb)[(](.+)[)]$", spec_unit)
  )[[1]]
  if (length(m) == 0) {
    return(NULL)
  }
  word <- m[2]
  inner <- m[3]

  logfun <- switch(
    word,
    log = log,
    ln = log,
    log10 = log10,
    lg = log10,
    log2 = log2,
    lb = log2
  )

  tmpl <- tryCatch(
    logfun(units::set_units(1, inner, mode = "standard")),
    error = function(e) NULL
  )
  if (is.null(tmpl)) {
    return(NULL)
  }

  parsed <- parse_log_unit(as.character(units(tmpl)))
  if (is.null(parsed)) {
    return(NULL)
  }
  parsed$unit <- units(tmpl)
  parsed
}

#' Shift a log-unit column to a target log reference
#'
#' Changing a log unit's reference level is a constant additive shift,
#' `value - log(coef_target / coef_source)`, taken in the unit's own log base.
#' Both sides must be log units of the same base and dimension.
#'
#' @param col a `units` vector carrying a log unit.
#' @param src_log parsed source log unit (from `parse_log_unit()`).
#' @param tgt_log parsed target log spec (from `parse_log_spec()`).
#' @return the shifted `units` vector, or `NULL` if the sides are incompatible.
#' @noRd
shift_log_column <- function(col, src_log, tgt_log) {
  if (is.null(src_log) || is.null(tgt_log)) {
    return(NULL)
  }
  if (src_log$base != tgt_log$base || src_log$dim != tgt_log$dim) {
    return(NULL)
  }

  logfun <- switch(src_log$base, ln = log, lg = log10, lb = log2)
  shifted <- as.numeric(col) - logfun(tgt_log$coef / src_log$coef)
  units(shifted) <- tgt_log$unit
  shifted
}
