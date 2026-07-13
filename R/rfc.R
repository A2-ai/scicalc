#' Categorize Renal Function
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
#'   Returns the value of \code{getOption("scicalc.missing_value")} (default \code{-999})
#'   for missing values. Includes a \code{category_standard} attribute
#'   indicating the source ("FDA" or "KDIGO").
#'
#' @references
#' FDA Guidance for Industry: Pharmacokinetics in Patients with Impaired Renal Function.
#' \url{https://www.fda.gov/media/78573/download}
#'
#' KDIGO 2024 Clinical Practice Guideline for the Evaluation and Management of Chronic Kidney Disease.
#' \url{https://www.kidney-international.org/action/showPdf?pii=S0085-2538(23)00766-4}
#'
#' @family renal_function
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
#' df <- df %>%
#'   mutate(
#'     BSA = bsa(WEIGHT, HEIGHT, method = "Dubois"),
#'     EGFR = egfr(is_female(SEX), is_black(RACE), AGE, CREAT),
#'     AEGFR = aegfr(EGFR, BSA),
#'     # Clinical categories using relative eGFR directly
#'     BRFC_CLINICAL = rfc(EGFR, category_standard = "clinical"),
#'     # Regulatory categories - convert relative eGFR to absolute
#'     BRFC_REGULATORY_REL = rfc(EGFR, BSA),
#'     # Regulatory categories - AEGFR already absolute
#'     BRFC_REGULATORY_ABS = rfc(AEGFR)
#'   )
#' df
#' @export
rfc <- function(
  estimator = NULL,
  bsa = NULL,
  category_standard = c("regulatory", "clinical"),
  absolute_units = NULL
) {
  checkmate::assert_numeric(
    if (inherits(estimator, "units")) units::drop_units(estimator) else estimator,
    null.ok = FALSE
  )
  category_standard <- match.arg(category_standard)

  # --- Infer units FIRST, then strip to plain numeric ---
  if (inherits(estimator, "units")) {
    abs_unit <- units::deparse_unit(units::set_units(1, "mL/min", mode = "standard"))
    rel_unit <- units::deparse_unit(units::set_units(1, "mL/min/bsa_ref", mode = "standard"))
    est_unit <- units::deparse_unit(estimator)
    is_abs <- (est_unit == abs_unit)
    is_rel <- (est_unit == rel_unit)
    if (!is_abs && !is_rel) {
      rlang::abort(paste0(
        "`estimator` has units [", as.character(est_unit),
        "] which is not a recognized eGFR/CrCL unit. ",
        "Expected [mL/min] (absolute) or [mL/min/bsa_ref] (relative)."
      ))
    }
    inferred_absolute <- is_abs
    if (!is.null(absolute_units) && absolute_units != inferred_absolute) {
      rlang::warn(paste0(
        "Provided absolute_units (", absolute_units, ") conflicts with input units [",
        as.character(est_unit), "]. Using the units of `estimator`."
      ))
    }
    absolute_units <- inferred_absolute
    estimator <- units::drop_units(estimator)
  } else {
    # Legacy attr fallback
    input_units <- attr(estimator, "units")
    if (!is.null(input_units)) {
      lifecycle::deprecate_warn(
        "0.4.0", I('attr(estimator, "units")'),
        details = "Pass a units object from egfr()/aegfr() instead."
      )
      inferred_absolute <- (input_units == "mL/min")
      if (!is.null(absolute_units) && absolute_units != inferred_absolute) {
        rlang::warn(paste0(
          "Provided absolute_units (", absolute_units, ") conflicts with input units attribute (",
          input_units, "). Using the units attribute of `estimator`."
        ))
      }
      absolute_units <- inferred_absolute
    } else if (is.null(absolute_units)) {
      rlang::abort("Must supply absolute_units when input has no units attribute.")
    }
  }

  # Now estimator is plain numeric — safe for sentinel checks
  mv_est <- check_mv_computation(estimator, "estimator")
  estimator[mv_est] <- NA
  mv_bsa <- NULL

  if (category_standard == "clinical") {
    if (!absolute_units) {
      rel_est <- estimator
    } else {
      bsa <- assert_and_strip_units(bsa, "m^2", "bsa")
      checkmate::assert_numeric(bsa, null.ok = FALSE)
      if (any(!is.na(estimator) & is.na(bsa))) {
        rlang::abort("bsa cannot be missing when absolute_est has values")
      }
      mv_bsa <- check_mv_computation(bsa, "bsa")
      bsa[mv_bsa] <- NA
      rel_est <- convert_abs_to_rel(estimator, bsa)
    }

    if (any(is.na(rel_est))) {
      rlang::inform("Estimator input has missing values")
    }

    rfc <- clinical_rfc(rel_est)
  } else {
    if (absolute_units) {
      abs_est <- estimator
    } else {
      bsa <- assert_and_strip_units(bsa, "m^2", "bsa")
      checkmate::assert_numeric(bsa, null.ok = FALSE)
      if (any(!is.na(estimator) & is.na(bsa))) {
        rlang::abort("bsa cannot be missing when relative_est has values")
      }
      mv_bsa <- check_mv_computation(bsa, "bsa")
      bsa[mv_bsa] <- NA
      abs_est <- convert_rel_to_abs(estimator, bsa)
    }

    if (any(is.na(abs_est))) {
      rlang::inform("Estimator input has missing values")
    }

    rfc <- regulatory_rfc(abs_est)
  }
  rfc <- apply_mv_mask(rfc, mv_est, mv_bsa)
  attr(rfc, "category_standard") <- if (category_standard == "clinical") "KDIGO" else "FDA"
  return(rfc)
}

clinical_rfc <- function(relative_est) {
  # units on 90/60/30/15 mL/min/1.73m2
  rfc <- dplyr::case_when(
    relative_est >= 90 ~ 1,
    relative_est >= 60 ~ 2,
    relative_est >= 30 ~ 3,
    relative_est >= 15 ~ 4,
    relative_est < 15 ~ 5,
    .default = getOption("scicalc.missing_value", -999)
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
    .default = getOption("scicalc.missing_value", -999)
  )
  rfc
}


#' @title Renal Function Classification (Deprecated)
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `brfc()` has been replaced by `rfc()` with a completely different interface.
#'
#' @param crcl Creatinine clearance value
#' @keywords internal
#' @export
brfc <- function(crcl) {
  lifecycle::deprecate_stop(
    when = "0.2.0",
    what = "brfc()",
    with = "rfc()",
    details = c(
      "The function signature has completely changed.",
      "Old: brfc(crcl)",
      "New: rfc(estimator, absolute_units, bsa, category_standard)",
      "Please see ?rfc for the new interface."
    )
  )
}
