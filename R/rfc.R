#' Calculates renal impairment categories based on Renal Function Estimator
#'
#' This function categorizes renal function based on estimated glomerular filtration rate (eGFR),
#' creatinine clearance, or other renal function estimators. It supports both clinical and
#' regulatory categorization standards and can convert between absolute (mL/min) and relative
#' (mL/min/1.73m²) units using body surface area.
#'
#' @param estimator Numeric vector of renal function estimator values (eGFR, CrCL, etc.)
#' @param absolute_units Logical indicating if \code{estimator} units are mL/min (\code{TRUE})
#'   or mL/min/1.73m² (\code{FALSE})
#' @param bsa Numeric vector of body surface area in m² for unit conversion. Required when
#'   converting between absolute and relative units
#' @param category_standard Character string specifying categorization standard:
#'   \code{"regulatory"} (default) or \code{"clinical"}
#'
#' @details
#' The function applies different categorization schemes based on the \code{category_standard}:
#'
#' \strong{Regulatory categories} (uses mL/min):
#' \itemize{
#'   \item 1: Normal: ≥90 mL/min
#'   \item 2:  Mild impairment: 60-89 mL/min
#'   \item 3: Moderate impairment: 30-59 mL/min
#'   \item 4: Severe impairment: <30 mL/min
#' }
#'
#' \strong{Clinical categories} (uses mL/min/1.73m²):
#' \itemize{
#'   \item 1: Normal: ≥90 mL/min/1.73m²
#'   \item 2: Mild impairment: 60-89 mL/min/1.73m²
#'   \item 3: Moderate impairment: 30-59 mL/min/1.73m²
#'   \item 4: Severe impairment: 15-29 mL/min/1.73m²
#'   \item 5: End-stage: <15 mL/min/1.73m²
#' }
#'
#' When unit conversion is required, the function uses:
#' \itemize{
#'   \item Absolute to relative: \code{relative = 1.73 (absolute / bsa)}
#'   \item Relative to absolute: \code{absolute = relative (bsa / 1.73)}
#' }
#'
#' @return Integer vector of renal impairment categories (1-4 for regulatory, 1-5 for clinical).
#'   Returns \code{-999} for missing values.
#'
#' @seealso
#' \code{\link{egfr}} for calculating eGFR, \code{\link{crcl}} for creatinine clearance,
#' \code{\link{bsa}} for body surface area calculation
#'
#' @examples
#' # Regulatory categories with absolute units (creatinine clearance)
#' rfc(estimator = c(95, 75, 45, 25), absolute_units = TRUE)
#'
#' # Clinical categories with relative units (eGFR)
#' rfc(
#'   estimator = c(95, 75, 45, 25, 10),
#'   absolute_units = FALSE,
#'   category_standard = "clinical"
#' )
#'
#' # Convert relative eGFR to regulatory categories
#' rfc(
#'   estimator = 65,
#'   absolute_units = FALSE,
#'   bsa = 1.8
#' )
#'
#' # Pipeline example with realistic data
#' df <- data.frame(
#'   ID = 1:4,
#'   SEX = c("F", "M", "F", "M"),
#'   AGE = c(65, 45, 70, 50),
#'   CREAT = c(1.2, 0.9, 1.5, 1.1),
#'   WEIGHT = c(70, 80, 60, 85),
#'   HEIGHT = c(165, 175, 160, 180),
#'   RACE = c("WHITE", "BLACK", "OTHER", "ASIAN")
#' )
#'
#' library(dplyr)
#' df %>%
#'   mutate(
#'     EGFR = egfr(is_female(SEX), is_black(RACE), AGE, CREAT),
#'     BSA = bsa(WEIGHT, HEIGHT, method = "Dubois"),
#'     # Clinical categories using eGFR directly
#'     BRFC_CLINICAL = rfc(EGFR, FALSE, category_standard = "clinical"),
#'     # Regulatory categories converting eGFR to absolute
#'     BRFC_REGULATORY = rfc(EGFR, FALSE, BSA)
#'   )
#' df
#' @export
rfc <- function(
  estimator = NULL,
  absolute_units = NULL,
  bsa = NULL,
  category_standard = c("regulatory", "clinical")
) {
  checkmate::assert_numeric(estimator, null.ok = FALSE)
  if (missing(absolute_units)) {
    stop("Must supply absolute flag to describe units.")
  }
  category_standard <- match.arg(category_standard)

  if (category_standard == "clinical") {
    if (!absolute_units) {
      rel_est <- estimator
    } else {
      checkmate::assert_numeric(bsa, null.ok = FALSE)
      rel_est <- convert_abs_to_rel(estimator, bsa)
    }

    if (any(is.na(rel_est))) {
      message("Estimator input has missing values")
    }

    rfc <- clinical_rfc(rel_est)
  } else {
    if (absolute_units) {
      abs_est <- estimator
    } else {
      checkmate::assert_numeric(bsa, null.ok = FALSE)
      abs_est <- convert_rel_to_abs(estimator, bsa)
    }

    if (any(is.na(abs_est))) {
      message("Estimator input has missing values")
    }

    rfc <- regulatory_rfc(abs_est)
  }
  return(rfc)
}

convert_abs_to_rel <- function(est, bsa) {
  if (any(!is.na(est) & is.na(bsa))) {
    stop("bsa cannot be missing when absolute_est has values")
  }
  rel_est <- 1.73 * est / bsa
  rel_est
}

convert_rel_to_abs <- function(est, bsa) {
  if (any(!is.na(est) & is.na(bsa))) {
    stop("bsa cannot be missing when relative_est has values")
  }
  abs_est <- est * bsa / 1.73
  abs_est
}

clinical_rfc <- function(relative_est) {
  # units on 90/60/30/15 mL/min/1.73m2
  rfc <- dplyr::case_when(
    relative_est >= 90 ~ 1,
    relative_est >= 60 ~ 2,
    relative_est >= 30 ~ 3,
    relative_est >= 15 ~ 4,
    relative_est < 15 ~ 5,
    .default = -999
  )
  rfc
}

regulatory_rfc <- function(absolute_est) {
  # units on 90/60/30 mL/min
  rfc <- dplyr::case_when(
    absolute_est >= 90 ~ 1,
    absolute_est >= 60 ~ 2,
    absolute_est >= 30 ~ 3,
    absolute_est < 30 ~ 4,
    .default = -999
  )
  rfc
}
