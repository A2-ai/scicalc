#' Get Unique Parameter-Unit Combinations
#'
#' @param params a column from a dataset with lab parameters
#' @param units a column from a dataset with units associated with those parameters
#'
#' @return a dataframe with distinct units and parameters with IU replaced to U and mu replaced with u
#'
#' @family unit_checking
#' @export
#'
#' @examples
#' df <- data.frame(
#'   PARAM = c(
#'     "ALB","ALT","AST","CR","TBIL",
#'     "ALB","CR","TBIL","ALT","AST"),
#'   UNIT = c(
#'     "g/L","U/L","U/L","umol/L","umol/L",
#'     "U/L","μmol/L","μmol/L","IU/L","IU/L")
#' )
#' get_unique_units_df(df$PARAM, df$UNIT)
get_unique_units_df <- function(params, units) {
  checkmate::assertCharacter(params)
  checkmate::assertCharacter(units)

  df <- data.frame(
    PARAM = params,
    UNIT = units
  ) |>
    dplyr::distinct()

  df$UNIT <- normalize_unit_string(df$UNIT)

  df <- df |>
    dplyr::distinct()

  df
}

#' Normalize non-udunits unit spellings
#'
#' Rewrites unit strings so common clinical spellings parse under udunits:
#' `IU` (international units) becomes `U`, and the micro sign `µ` becomes
#' `u`.
#'
#' @param x a character vector of unit strings.
#' @return `x` with the substitutions applied (`NA` preserved).
#' @keywords internal
normalize_unit_string <- function(x) {
  x <- stringr::str_replace_all(x, "\U03BC", "u")
  x <- stringr::str_replace_all(x, "IU", "U")
  x
}

#' Attach Units from a Units Column
#'
#' @description
#' Sets units on a values vector using a companion column that records each
#' value's unit (e.g. `PCSTRESN` with `PCSTRESU`). If the unit column resolves
#' to one unit after normalizing `IU`/`µ`, a standard `units` vector is
#' returned. If multiple units remain, a warning is issued and a `mixed_units`
#' vector is returned with the corresponding unit attached to each value.
#' Blank (`NA`/`""`) entries are ignored with a warning.
#'
#' Intended for use in a `mutate()` before [convert_units_to_spec()], replacing
#' a hand-typed `units::set_units(values, "ng/mL")` with the unit carried in the
#' data.
#'
#' @param values a numeric vector of values.
#' @param units a character vector of unit strings, one per value.
#'
#' @return `values` as a `units` object when there is one distinct unit, or as
#'   a `mixed_units` object when there are multiple distinct units.
#'
#' @family unit_checking
#' @export
#'
#' @examples
#' with_units(c(10, 20, 30), c("ng/mL", "ng/mL", "ng/mL"))
#'
#' # IU is normalized to U
#' with_units(c(15, 20), c("IU/L", "IU/L"))
#'
#' # Multiple units are retained row by row
#' with_units(c(1, 500), c("ug/mL", "ng/mL"))
with_units <- function(values, units) {
  values_name <- deparse1(substitute(values))
  units_name <- deparse1(substitute(units))
  checkmate::assert_numeric(values)

  norm <- normalize_unit_string(as.character(units))
  missing_mask <- is.na(norm) | norm == ""
  if (any(missing_mask)) {
    rlang::warn(paste0(
      sum(missing_mask),
      " missing/blank unit(s) encountered in `units` and ignored."
    ))
  }

  distinct_units <- unique(norm[!missing_mask])

  if (length(distinct_units) == 0) {
    rlang::abort("`units` contains no usable unit; cannot attach units to `values`.")
  }
  if (length(distinct_units) > 1) {
    missing_value_units <- missing_mask & !is.na(values)
    if (any(missing_value_units)) {
      rlang::abort(paste0(
        sum(missing_value_units),
        " non-missing value(s) have a missing/blank unit; cannot create a ",
        "`mixed_units` vector."
      ))
    }

    rlang::warn(paste0(
      "Multiple units found in `", units_name, "`: ",
      paste0('"', distinct_units, '"', collapse = ", "),
      ". Returning a `mixed_units` vector rather than a standard `units` vector."
    ))

    for (unit in distinct_units) {
      log_audit_event(
        "unit",
        fn = "with_units",
        input = values_name,
        from = NA_character_,
        to = unit,
        transform = "attach",
        detail = units_name,
        evidence = "source-recorded",
        basis = paste0("row-level unit column: ", units_name),
        n = sum(!missing_mask & norm == unit)
      )
    }

    # `mixed_units()` requires a unit for every element. A missing value has no
    # magnitude to interpret, so use the first observed unit only as its typed
    # NA placeholder; non-missing values with absent units were rejected above.
    norm[missing_mask] <- distinct_units[[1]]
    return(units::mixed_units(values, norm))
  }

  log_audit_event(
    "unit",
    fn = "with_units",
    input = values_name,
    from = NA_character_,
    to = distinct_units,
    transform = "attach",
    detail = units_name,
    evidence = "source-recorded",
    basis = paste0("unit column: ", units_name),
    n = sum(!missing_mask)
  )

  units::set_units(values, distinct_units, mode = "standard")
}

# Apply supported mathematical operations element by element because
# `mixed_units` is list-like and the units package does not provide a Math
# method for it. The actual operation on each element is still handled by the
# units package, so its dimensional checks and unit propagation remain in
# force.
#' @export
#' @noRd
Math.mixed_units <- function(x, ...) {
  supported <- c(
    "abs", "sign", "sqrt", "floor", "ceiling", "trunc", "round",
    "signif", "exp", "expm1", "log", "log10", "log2", "log1p"
  )

  if (!.Generic %in% supported) {
    rlang::abort(paste0(
      "`", .Generic, "()` is not supported for a `mixed_units` vector."
    ))
  }

  input <- mask_missing_computation_input(x, "x")
  x <- input$value

  out <- lapply(
    unclass(x),
    function(value) do.call(.Generic, c(list(value), list(...)))
  )

  if (all(vapply(out, inherits, logical(1), "units"))) {
    return(apply_mv_mask(structure(out, class = class(x)), input$mask))
  }

  apply_mv_mask(unlist(out, use.names = TRUE), input$mask)
}

#' Check for Unique Units per Parameter
#'
#' @param params a column from a dataset with lab parameters
#' @param units a column from a dataset with units associated with those parameters
#'
#' @return a boolean
#'
#' @family unit_checking
#' @export
#'
#' @examples
#' df <- data.frame(
#'   PARAM = c(
#'     "ALB","ALT","AST","CR","TBIL",
#'     "ALB","CR","TBIL","ALT","AST"),
#'   UNIT = c(
#'     "g/L","U/L","U/L","umol/L","umol/L",
#'     "U/L","μmol/L","μmol/L","IU/L","IU/L")
#' )
#' check_for_unique_units(df$PARAM, df$UNIT)
check_for_unique_units <- function(params, units) {
  checkmate::assertCharacter(params)
  checkmate::assertCharacter(units)

  df <- get_unique_units_df(params, units)

  if (length(unique(df$PARAM)) < length(df$PARAM)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
