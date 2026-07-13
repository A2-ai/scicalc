#' Calculate Absolute eGFR
#'
#' Converts relative eGFR (normalized to 1.73 m²) to absolute eGFR using
#' the patient's actual body surface area.
#'
#' @param egfr estimated glomerular filtration rate (mL/min/1.73 m²)
#' @param bsa body surface area (m²)
#'
#' @details
#' Absolute eGFR is calculated as:
#' \deqn{aGFR = \frac{eGFR \cdot BSA}{1.73}}{aGFR = eGFR * BSA / 1.73}
#'
#' where:
#' \itemize{
#'   \item \eqn{eGFR} = relative eGFR (mL/min/1.73m²)
#'   \item \eqn{BSA} = body surface area (m²)
#' }
#'
#' @return Absolute eGFR (mL/min)
#'
#' @family renal_function
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
  # --- Detect units (units class or legacy attr) and check if already absolute ---
  if (inherits(egfr, "units")) {
    abs_unit <- units::deparse_unit(units::set_units(1, "mL/min", mode = "standard"))
    if (units::deparse_unit(egfr) == abs_unit) {
      warning("Input eGFR already has absolute units (mL/min), returning unchanged")
      return(egfr)
    }
  } else {
    # Legacy attr fallback — support for one release cycle
    input_units <- attr(egfr, "units")
    if (!is.null(input_units)) {
      lifecycle::deprecate_warn(
        "0.4.0", I('attr(egfr, "units")'),
        details = "Pass a units object from egfr() instead."
      )
      if (input_units == "mL/min") {
        warning("Input eGFR already has absolute units (mL/min), returning unchanged")
        return(egfr)
      }
    }
  }

  # --- Convert to canonical units and strip before any checks ---
  egfr_val <- assert_and_strip_units(egfr, "mL/min/bsa_ref", "egfr")
  bsa_val <- assert_and_strip_units(bsa, "m^2", "bsa")
  checkmate::assertNumeric(egfr_val)
  checkmate::assertNumeric(bsa_val)

  if (any(is.na(egfr_val))) message("egfr contains missing values")
  if (any(is.na(bsa_val))) message("bsa contains missing values")

  aegfr <- convert_rel_to_abs(egfr_val, bsa_val)
  aegfr <- units::set_units(aegfr, "mL/min", mode = "standard")
  return(aegfr)
}

#' Convert absolute eGFR to relative eGFR
#'
#' @param est Absolute eGFR (mL/min)
#' @param bsa Body surface area (m²)
#' @return Relative eGFR (mL/min/1.73m²)
#' @keywords internal
convert_abs_to_rel <- function(est, bsa) {
  1.73 * est / bsa
}

#' Convert relative eGFR to absolute eGFR
#'
#' @param est Relative eGFR (mL/min/1.73m²)
#' @param bsa Body surface area (m²)
#' @return Absolute eGFR (mL/min)
#' @keywords internal
convert_rel_to_abs <- function(est, bsa) {
  est * bsa / 1.73
}
