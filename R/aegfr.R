#' Calculates Absolute estimated Glomerular Filtration Rate (eGFR)
#'
#' @param egfr estimated glomerular filtration rate (mL/min/1.73 m2)
#' @param bsa body surface area (m2)
#'
#' @return Absolute eGFR (mL/min)
#' @export
#'
#' @examples
#' aegfr(90, 1.9)
#'
#' df <- data.frame(
#'   ID = c(1, 2, 3, 4),
#'   EGFR = c(80, 95, 70, 60),
#'   BSA = c(1.60, 1.85, 1.75, 2.00)
#' )
#'
#' df <- df %>%
#'   dplyr::group_by(ID) %>%
#'   dplyr::mutate(AEGFR = aegfr(EGFR, BSA))
#' df
aegfr <- function(egfr, bsa) {
  checkmate::assertNumeric(egfr)
  checkmate::assertNumeric(bsa)

  if (any(is.na(egfr))) {
    message("egfr contains missing values")
  }
  if (any(is.na(bsa))) {
    message("bsa contains missing values")
  }

  aegfr <- egfr * (bsa / 1.73)
  return(aegfr)
}
