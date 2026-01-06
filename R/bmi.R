#' Calculates Baseline Body Mass Index based on Weight and Height
#'
#' @param weight weight of subject (kg)
#' @param height height of subject (cm)
#'
#' @return the BMI value (kg m^(-2))
#' @export
#'
#' @examples
#' b <- bmi(80.56, 167)
#'
#' df <- data.frame(
#'   "WT" = c(80.56, 71.53, 81.04, 70.17),
#'   "HT" = c(167, 161, 163, 164)
#' )
#' df <- dplyr::mutate(df, bmi = bmi(WT, HT))
#' df
bmi <- function(weight, height) {
  # check that weight and height are numeric
  checkmate::assertNumeric(weight)
  checkmate::assertNumeric(height)

  input_lengths <- lengths(list(weight, height))
  if (length(unique(input_lengths)) != 1) {
    warning("Inputs have different lengths! Please check data.")
  }

  # give message if any NAs
  if (any(is.na(weight))) {
    message("weight contains missing values")
  }
  if (any(is.na(height))) {
    message("height contains missing values")
  }

  bmi <- weight / ((height / 100)^2)
  attr(bmi, "units") <- "kg/m^2"
  return(bmi)
}


#' Calculate BMI (Deprecated)
#'
#' `bbmi()` was renamed to `bmi()` to improve function naming consistency.
#'
#' @param ... Arguments passed to [bmi()]
#' @keywords internal
#' @export
bbmi <- function(...) {
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "bbmi()",
    with = "bmi()"
  )
  bmi(...)
}
