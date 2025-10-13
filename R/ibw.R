#' Ideal Body Weight function
#'
#' Calculates ideal body weight using the Devine equation. For men: 50 kg + 2.3 kg
#' for each inch above 5 feet. For women: 45.5 kg + 2.3 kg for each inch above 5 feet.
#' By default, applies intercepts for individuals shorter than 5 feet (152.4 cm).
#'
#' @param height baseline height of subject in centimeters
#' @param sexf 0 = male, 1 = female, from sexf()
#' @param intercept_for_short logical indicating whether to apply intercepts
#'   for heights < 152.4 cm (5 feet). When TRUE (default), ideal weight is
#'	 set to intercept weight (50 kg for males, 45.5 kg for females)
#'
#' @return ideal body weight in kilograms
#' @export
#'
#' @references
#' The Origin of the "Ideal" Body Weight Equations, by Pai and Paloucek.
#' The annals of Pharmacotherapy 2000 september, volumn 34
#'
#' @examples
#' df <- data.frame(
#'   ID = 1:6,
#'   HEIGHT = c(160, 170, 175, 165, 180, 150),
#'   SEX = c(1, 0, 0, 1, 0, 1)
#' )
#' df <- dplyr::mutate(df, IBW = ibw(HEIGHT, SEX))
#' df
ibw <- function(height, sexf, intercept_for_short = TRUE) {
  checkmate::assert_numeric(height)
  checkmate::assert_numeric(sexf)

  ideal_weight <- ifelse(sexf == 0, 50, 45.5)

  if (intercept_for_short) {
    height <- ifelse(height < 152.4, 152.4, height)
  }

  ideal_weight + 2.3 / 2.54 * (height - 152.4)
}
