
#' Calculates hepatic function criteria
#'
#' @param ast Aspartate aminotransferase concentration (IU/L)
#' @param ulnast Upper limit of normal AST (IU/L), typically 33
#' @param bili bilirubin concentration (mg/dL)
#' @param ulnbili Upper limit of normal BILI (mg/dL), typically 1.2
#'
#' @return category of hepatic function
#' @export
#'
#' @examples
#' bhfc(15, 33, 0.6, 1.2)
#'
#' df <- data.frame(
#'   "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   "SEX" = c("F", "F", "F", "F", "M", "M", "M", "M"),
#'   "RACE" = c("WHITE", "WHITE", "WHITE", "WHITE", "BLACK", "BLACK", "BLACK", "BLACK"),
#'   "AGE" = c(24, 24, 24, 24, 22, 22, 22, 22),
#'   "CREAT" = c(1, 1, 1, 1, 4, 4, 4, 4),
#'   "WEIGHT" = c(70, 70, 70, 70, 65, 65, 65, 65),
#'   "AST" = c(15, 15, 15, 15, 23, 23, 23, 23),
#'   "ULNAST" = c(33, 33, 33, 33, 33, 33, 33, 33),
#'   "BILI" = c(1, 1, 1, 1, 0.4, 0.4, 0.4, 0.4),
#'   "ULNBILI" = c(1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2)
#' )
#'
#' df <- df %>%
#'   dplyr::group_by(ID) %>%
#'   dplyr::mutate(BHFC = bhfc(AST, ULNAST, BILI, ULNBILI))
bhfc <- function(ast, ulnast, bili, ulnbili) {
  checkmate::assertNumeric(ast)
  checkmate::assertNumeric(ulnast)
  checkmate::assertNumeric(bili)
  checkmate::assertNumeric(ulnbili)

  if (any(is.na(ast))) {
    message("AST contains missing values")
  }
  if (any(is.na(ulnast))) {
    message("ULNAST contains missing values")
  }
  if (any(is.na(bili))) {
    message("BILI contains missing values")
  }
  if (any(is.na(ulnbili))) {
    message("ULNBILI contains missing values")
  }

  edge_cases <- FALSE
  if (!any(is.na(bili)) & !any(is.na(ulnbili)) & any(dplyr::near(bili, 1.5 * ulnbili))) {
    edge_cases <- TRUE
  }
  if (!any(is.na(bili)) & !any(is.na(ulnbili)) & any(dplyr::near(bili, 3 * ulnbili))) {
    edge_cases <- TRUE
  }

  if (!edge_cases) {
    bhfc <- dplyr::case_when(
      ast <= ulnast & bili <= ulnbili                             ~ 1,
      ast > ulnast | dplyr::between(bili, ulnbili, 1.5 * ulnbili) ~ 2,
      dplyr::between(bili, 1.5 * ulnbili, 3 * ulnbili)            ~ 3,
      bili > 3 * ulnbili                                          ~ 4,
      .default = -999
    )
    return (bhfc)
  } else {
    bhfc <- dplyr::case_when(
      # bili is near 1.5 * ulnbili or 3 * ulnbili so it's either 2, 3
      ast > ulnast | dplyr::near(bili, 1.5 * ulnbili) ~ 2,
      dplyr::near(bili, 3 * ulnbili)                  ~ 3,
      .default = -999
    )
    return (bhfc)
  }
}
