#' Test for the Missing Value Indicator
#'
#' @description
#' Returns `TRUE` for elements equal to the missing value indicator
#' (`getOption("scicalc.missing_value")`, default `-999`). Works whether or not
#' `x` carries units, so it can be used to filter columns that
#' [convert_units_to_spec()] or [pivot_with_units()] have made into `units`
#' objects, e.g. `dplyr::filter(df, !is_missing_value(NTFD))`.
#'
#' `NA` elements return `FALSE` (they are a separate representation of missing,
#' not the sentinel).
#'
#' @param x a numeric or `units` vector.
#' @param missing_value the sentinel to test against. Defaults to
#'   `getOption("scicalc.missing_value", -999)`.
#'
#' @return a logical vector the same length as `x`.
#'
#' @export
#'
#' @examples
#' is_missing_value(c(1, -999, 3))
#'
#' x <- units::set_units(c(1, -999, 3), "d", mode = "standard")
#' is_missing_value(x)
is_missing_value <- function(x, missing_value = getOption("scicalc.missing_value", -999)) {
  if (inherits(x, "units")) {
    x <- units::drop_units(x)
  }

  if (is.na(missing_value)) {
    return(rep(FALSE, length(x)))
  }

  mask <- x == missing_value
  mask[is.na(mask)] <- FALSE
  mask
}
