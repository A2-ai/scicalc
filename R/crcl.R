#' Calculates Creatinine clearance with Cockcroft-Gault equation
#'
#' @param age age of subject (years)
#' @param weight weight of subject (kg)
#' @param creat serum creatinine levels (mg/dL)
#' @param sexf bool of sex of subject. Female: True, Male: False
#'
#' @return CrCl (mL/min)
#' @export
#'
#' @examples
#' crcl(FALSE, 20, 10, 70)
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
#'   dplyr::mutate(CRCL = crcl(is_female(SEX), AGE, CREAT, WEIGHT))
crcl <- function(sexf, age, creat, weight) {
  checkmate::assertLogical(sexf)
  checkmate::assertNumeric(age)
  checkmate::assertNumeric(creat)
  checkmate::assertNumeric(weight)

  if (any(is.na(sexf))) {
    message('sexf contains missing values')
  }
  if (any(is.na(age))) {
    message('age contains missing values')
  }
  if (any(is.na(creat))) {
    message('creat contains missing values')
  }
  if (any(is.na(weight))) {
    message('weight contains missing values')
  }

  sex_mult <- ifelse(sexf, 0.85, 1)
  crcl <- (140 - age) * weight / (72 * creat) * sex_mult
  return (crcl)
}
