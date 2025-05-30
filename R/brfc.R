# BRFC = case_when(BCRCL>=90   ~ 0,
#                  BCRCL>=60 & BCRCL<90 ~ 1,
#                  BCRCL>=30 & BCRCL<60 ~ 2,
#                  BCRCL<30 ~ 3,
#                  .default =  -999)

#' Calculates renal impairment categories based on Renal Function Estimator
#'
#' @param est Renal Function Estimator, either EGFR (mL/min/1.73m2), AEGFR (mL/min) or creatinine clearance rate (mL/min)
#' @param bsa Body surface are to normalize EGFR to AEGFR when calculating regulatory brfc
#' @param category_standard either regulatory or clinical
#'
#' @return integer renal impairment category
#' (Clinical category)
#' 1: Normal, Estimator >= 90 mL/min/1.73m2
#' 2: Mild, 90 > Estimator >=60
#' 3: Moderate, 60 > Estimator >= 30
#' 4: Severe, 30 > Estimator >= 15
#' 5: End Stage, Estimator < 15
#'
#' (Regulatory category)
#' 1: Normal, Estimator >= 90 mL/min
#' 2: Mild, 90 > Estimator >=60
#' 3: Moderate, 60 > Estimator >= 30
#' 4: Severe, Estimator < 30
#'
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
#'   "WEIGHT" = c(70, 70, 70, 70, 65, 65, 65, 65),
#'	 "HEIGHT" = c(167, 172, 168, 162, 159, 163, 170, 166)
#' )
#'
#' df <- df %>%
#'   dplyr::group_by(ID) %>%
#'   dplyr::mutate(
#'     CRCL = crcl(is_female(SEX), AGE, CREAT, WEIGHT),
#'     BRFC = brfc(CRCL, "mL/min")
#'   )
brfc <- function(
  est,
  est_units = c("mL/min", "mL/min/1.73m2"),
  bsa = NULL,
  category_standard = c("regulatory", "clinical")
) {
  checkmate::assertNumeric(est)

  if (!missing(est_units)) {
    est_units <- match.arg(est_units)
  } else {
    stop("Must supply estimator units: mL/min, mL/min/1.73m2")
  }
  category_standard <- match.arg(category_standard)

  if (any(is.na(est))) {
    message("Estimator input has missing values")
  }

  if (category_standard == "clinical") {
    brfc <- dplyr::case_when(
      est >= 90 ~ 1,
      est >= 60 ~ 2,
      est >= 30 ~ 3,
      est >= 15 ~ 4,
      est < 15 ~ 5,
      .default = -999
    )
  } else {
    if (est_units == "mL/min/1.73m2") {
      if (all(is.na(bsa))) {
        stop("must supply bsa")
      }
      abs_est <- est * (bsa / 1.73)
    } else {
      abs_est <- est
    }

    brfc <- dplyr::case_when(
      abs_est >= 90 ~ 1,
      abs_est >= 60 ~ 2,
      abs_est >= 30 ~ 3,
      abs_est < 30 ~ 4,
      .default = -999
    )
  }
  return(brfc)
}
