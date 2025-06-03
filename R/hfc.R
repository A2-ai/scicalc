#' Calculates hepatic function categories based on NCI-ODWG criteria
#'
#' This function categorizes hepatic function impairment using the National Cancer Institute
#' Organ Dysfunction Working Group (NCI-ODWG) criteria. It evaluates aspartate aminotransferase
#' (AST) and bilirubin levels relative to their upper limits of normal to determine hepatic
#' impairment severity. The function handles edge cases where bilirubin values are very close
#' to category boundaries using floating-point tolerant comparisons.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param ast Numeric vector of aspartate aminotransferase concentrations (IU/L)
#' @param ulnast Numeric vector of upper limit of normal AST values (IU/L).
#'   Typically 33 IU/L for most laboratories
#' @param bili Numeric vector of total bilirubin concentrations (mg/dL)
#' @param ulnbili Numeric vector of upper limit of normal bilirubin values (mg/dL).
#'   Typically 1.2 mg/dL for most laboratories
#'
#' @details
#' \strong{The NCI-ODWG hepatic function categories are defined as}:
#' \itemize{
#'   \item 1: Normal: AST ≤ ULN AND bilirubin ≤ ULN
#'   \item 2: Mild impairment: AST > ULN OR bilirubin > ULN but ≤ 1.5 × ULN
#'   \item 3: Moderate impairment: Bilirubin > 1.5 × ULN but ≤ 3 × ULN
#'   \item 4: Severe impairment: Bilirubin > 3 × ULN
#' }
#'
#' \strong{Special handling:}
#' The function uses \code{dplyr::near()} for boundary comparisons when bilirubin
#' values are very close to 1.5 × ULN or 3 × ULN to handle floating-point precision
#' issues that can occur with calculated thresholds.
#'
#' @return Integer vector of hepatic function categories (1-4). Returns \code{-999}
#'   for missing values.
#'
#' @references
#' National Cancer Institute Organ Dysfunction Working Group criteria for hepatic impairment
#'
#' @examples
#' # Single patient with normal hepatic function
#' hfc(ast = 15, ulnast = 33, bili = 0.6, ulnbili = 1.2)
#'
#' # Multiple patients with different impairment levels
#' hfc(ast = c(25, 45, 30, 20),
#'      ulnast = c(33, 33, 33, 33),
#'      bili = c(0.8, 1.0, 2.5, 4.0),
#'      ulnbili = c(1.2, 1.2, 1.2, 1.2))
#'
#' # Edge case: bilirubin exactly at boundary
#' hfc(ast = 25, ulnast = 33, bili = 1.8, ulnbili = 1.2)  # 1.8 = 1.5 * 1.2
#'
#' # Pipeline example with realistic data
#' library(dplyr)
#'
#' patients <- data.frame(
#'   ID = 1:6,
#'   AST = c(15, 45, 28, 35, 22, 30),
#'   ULNAST = 33,
#'   BILI = c(0.8, 1.0, 2.2, 4.5, 1.8, 0.9),
#'   ULNBILI = 1.2
#' )
#'
#' patients %>%
#'   mutate(BHFC = hfc(AST, ULNAST, BILI, ULNBILI))
#'
#' @export
hfc <- function(ast, ulnast, bili, ulnbili) {
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
  if (
    !any(is.na(bili)) &&
      !any(is.na(ulnbili)) &&
      any(dplyr::near(bili, 1.5 * ulnbili))
  ) {
    edge_cases <- TRUE
  }
  if (
    !any(is.na(bili)) &&
      !any(is.na(ulnbili)) &&
      any(dplyr::near(bili, 3 * ulnbili))
  ) {
    edge_cases <- TRUE
  }

  if (!edge_cases) {
    hfc <- dplyr::case_when(
      ast <= ulnast & bili <= ulnbili ~ 1,
      ast > ulnast | dplyr::between(bili, ulnbili, 1.5 * ulnbili) ~ 2,
      dplyr::between(bili, 1.5 * ulnbili, 3 * ulnbili) ~ 3,
      bili > 3 * ulnbili ~ 4,
      .default = -999
    )
    hfc
  } else {
    hfc <- dplyr::case_when(
      # bili is near 1.5 * ulnbili or 3 * ulnbili so it's either 2, 3
      ast > ulnast | dplyr::near(bili, 1.5 * ulnbili) ~ 2,
      dplyr::near(bili, 3 * ulnbili) ~ 3,
      .default = -999
    )

    hfc
  }
}


#' Calculates hepatic function categories based on NCI-ODWG criteria
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `bhfc()` was renamed to `hfc()` to improve function naming consistency.
#'
#' @param ... Arguments passed to [hfc()]
#' @keywords internal
#' @export
bhfc <- function(...) {
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "bhfc()",
    with = "hfc()"
  )
  hfc(...)
}
