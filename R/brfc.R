# BRFC = case_when(BCRCL>=90   ~ 0,
#                  BCRCL>=60 & BCRCL<90 ~ 1,
#                  BCRCL>=30 & BCRCL<60 ~ 2,
#                  BCRCL<30 ~ 3,
#                  .default =  -999)

#' Calculates renal impairment categories based on CrCL
#'
#' @param crcl creatinine clearance rate (mL/min)
#'
#' @return integer renal impairment category
#' @export
#'
#' @examples
#' brfc(crcl(FALSE, 20, 10, 70))
#'
#' df <- data.frame(
#'   "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   "SEX" = c("F", "F", "F", "F", "M", "M", "M", "M"),
#'   "RACE" = c("WHITE", "WHITE", "WHITE", "WHITE", "BLACK", "BLACK", "BLACK", "BLACK"),
#'   "AGE" = c(24, 24, 24, 24, 22, 22, 22, 22),
#'   "CREAT" = c(1, 1, 1, 1, 4, 4, 4, 4),
#'   "WEIGHT" = c(70, 70, 70, 70, 65, 65, 65, 65)
#' )
#'
#' df <- df %>%
#'   dplyr::group_by(ID) %>%
#'   dplyr::mutate(
#'     CRCL = crcl(is_female(SEX), AGE, CREAT, WEIGHT),
#'     BRFC = brfc(CRCL)
#'   )
brfc <- function(crcl) {
  checkmate::assertNumeric(crcl)

  if (any(is.na(crcl))) {
    message("creatinine clearance input has missing values")
  }

  brfc <- dplyr::case_when(
    crcl >= 90 ~ 0,
    crcl >= 60 ~ 1,
    crcl >= 30 ~ 2,
    crcl <  30 ~ 3,
    .default = -999
  )
  return(brfc)
}
