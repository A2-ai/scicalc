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
#' "WT" = c(80.56, 71.53, 81.04, 70.17),
#' "HT" = c(167, 161, 163, 164)
#' )
#' df <- dplyr::mutate(df, bbmi = bmi(WT, HT))
bmi <- function(weight, height) {
  # check that weight and height are numeric
  checkmate::assertNumeric(weight)
  checkmate::assertNumeric(height)

  # give message if any NAs
  if (any(is.na(weight))) {
    message("weight contains missing values")
  }
  if (any(is.na(height))) {
    message("height contains missing values")
  }

  bmi <- weight / ((height / 100)^2)

  bmi
}
