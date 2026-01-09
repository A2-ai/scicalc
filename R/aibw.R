#' Calculate Adjusted Ideal Body Weight
#'
#' Calculates adjusted ideal body weight, which accounts for excess body weight
#' in obese patients.
#'
#' @param weight baseline weight of subject in kilograms
#' @param height baseline height of subject in centimeters
#' @param sexf 0 = male, 1 = female, from sexf()
#' @param age Numeric vector of baseline age in years.
#' @param allow_ibw_lt_intercept logical indicating whether to allow ideal
#'   body weight to be lower than intercepts (default TRUE).
#' @param allow_tbw_lt_ibw logical indicating whether to allow adjusted ideal
#'   body weight to be less than IBW (default TRUE).
#'
#' @details
#' Adjusted ideal body weight is calculated as:
#' \deqn{AIBW = IBW + 0.4 \cdot (TBW - IBW)}{AIBW = IBW + 0.4 * (TBW - IBW)}
#'
#' where:
#' \itemize{
#'   \item \eqn{IBW} = ideal body weight (kg)
#'   \item \eqn{TBW} = total (actual) body weight (kg)
#' }
#'
#' @return adjusted ideal body weight (kg)
#'
#' @family body_composition
#' @export
#'
#' @examples
#' df <- data.frame(
#'   ID = 1:6,
#'   HEIGHT = c(160, 170, 175, 165, 180, 150),
#'   WEIGHT = c(53, 71, 78, 55, 72, 43),
#'   SEX = c(1, 0, 0, 1, 0, 1),
#'   AGE = c(18, 27, 34, 33, 29, 30)
#' )
#' df <- dplyr::mutate(df, AIBW = aibw(WEIGHT, HEIGHT, SEX, AGE))
#' df
aibw <- function(
  weight,
  height,
  sexf,
  age,
  allow_ibw_lt_intercept = TRUE,
  allow_tbw_lt_ibw = TRUE
) {
  checkmate::assert_numeric(weight)

  input_lengths <- lengths(list(weight, height, sexf, age))
  if (length(unique(input_lengths)) != 1) {
    warning("Inputs have different lengths! Please check data.")
  }

  if (any(is.na(weight), na.rm = TRUE)) {
    message("weight contains missing values.")
  }

  ideal_bw <- ibw(height, sexf, age, allow_ibw_lt_intercept)

  mult <- weight - ideal_bw
  if (!allow_tbw_lt_ibw) {
    mult <- ifelse(mult < 0, 0, mult)
  }

  aibw <- ideal_bw + 0.4 * mult
  attr(aibw, "units") <- "kg"
  return(aibw)
}
