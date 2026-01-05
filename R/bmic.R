#' BMI Category Function
#'
#' Categorizes individuals based on their Body Mass Index (BMI) according to
#' standard WHO obesity classification criteria. Validates age
#' to ensure appropriate application of adult BMI categories.
#'
#' @param bmi Numeric vector of baseline Body Mass Index values (kg/m²)
#' @param age Numeric vector of baseline age in years.
#'   the function will issue warnings for individuals under 18 years old, as
#'   adult BMI categories may not be appropriate for pediatric populations
#'
#' @details
#' \strong{BMI Categories:}
#' \itemize{
#'   \item 1: Underweight: BMI < 18.5 kg/m²
#'   \item 2: Normal weight: BMI 18.5 to < 25 kg/m²
#'   \item 3: Overweight: BMI 25 to < 30 kg/m²
#'   \item 4: Obese: BMI ≥ 30 kg/m²
#' }
#'
#' \strong{Age Considerations:}
#' The function will warn if any individuals are under
#' 18 years old, as standard adult BMI categories may not be appropriate for
#' children and adolescents.
#'
#' @return Integer vector of obesity categories (1-4). Returns \code{-999}
#'   for missing BMI values.
#'
#' @references
#' World Health Organization.
#' https://www.who.int/news-room/fact-sheets/detail/obesity-and-overweight
#'
#' @seealso
#' \code{\link{bmi}} for calculating BMI from weight and height,
#' \code{\link{agec}} for age categorization
#'
#' @examples
#' patients <- data.frame(
#'   ID = 1:6,
#'   AGE = c(25, 45, 17, 55, 30, 65),
#'   WEIGHT = c(60, 80, 70, 90, 75, 85),
#'   HEIGHT = c(165, 175, 170, 180, 160, 175)
#' )
#'
#' patients <-
#'   dplyr::mutate(
#'     patients,
#'     BMI = bmi(WEIGHT, HEIGHT),
#'     BMIC = bmic(BMI, AGE)
#'   )
#'
#' @export
bmic <- function(bmi, age) {
  checkmate::assertNumeric(bmi)
  checkmate::assertNumeric(age)

  # give message if any NAs in BMI
  if (any(is.na(bmi))) {
    message("BMI contains missing values")
  }
  if (any(bmi < 0, na.rm = TRUE)) {
    message("BMI contains negative values")
  }

  if (any(is.na(age))) {
    message("age contains missing values")
  }

  if (any(age < 18, na.rm = TRUE)) {
    warning(
      "Age contains values less than 18 years. Adult BMI categories may not be appropriate for pediatric populations."
    )
  }

  # Categorize BMI
  dplyr::case_when(
    0 < bmi & bmi < 18.5 ~ 1, # Underweight
    bmi >= 18.5 & bmi < 25 ~ 2, # Normal weight
    bmi >= 25 & bmi < 30 ~ 3, # Overweight
    bmi >= 30 ~ 4, # Obese
    .default = -999
  )
}
