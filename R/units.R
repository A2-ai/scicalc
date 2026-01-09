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

  df$UNIT <- stringr::str_replace_all(df$UNIT, "\U03BC", "u")
  df$UNIT <- stringr::str_replace_all(df$UNIT, "IU", "U")

  df <- df %>%
    dplyr::distinct()

  df
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
