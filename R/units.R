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
  ) %>%
    dplyr::distinct()

  df$UNIT <- normalize_unit_string(df$UNIT)

  df <- df %>%
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
#' value's unit (e.g. `PCSTRESN` with `PCSTRESU`). The unit column must resolve
#' to a single unit after normalizing `IU`/`µ`; blank (`NA`/`""`) entries
#' are ignored with a warning.
#'
#' Intended for use in a `mutate()` before [convert_units_to_spec()], replacing
#' a hand-typed `units::set_units(values, "ng/mL")` with the unit carried in the
#' data.
#'
#' @param values a numeric vector of values.
#' @param units a character vector of unit strings, one per value.
#'
#' @return `values` as a `units` object.
#'
#' @family unit_checking
#' @export
#'
#' @examples
#' with_units(c(10, 20, 30), c("ng/mL", "ng/mL", "ng/mL"))
#'
#' # IU is normalized to U
#' with_units(c(15, 20), c("IU/L", "IU/L"))
with_units <- function(values, units) {
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
    rlang::abort(paste0(
      "`units` must resolve to a single unit; found: ",
      paste0('"', distinct_units, '"', collapse = ", "),
      "."
    ))
  }

  units::set_units(values, distinct_units, mode = "standard")
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
