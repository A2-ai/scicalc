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
  checkmate::assertNumeric(egfr)
  checkmate::assertNumeric(bsa)

  # Check if input already has absolute units

  input_units <- attr(egfr, "units")
  if (!is.null(input_units) && input_units == "mL/min") {
    warning("Input eGFR already has absolute units (mL/min), returning unchanged")
    return(egfr)
  }

  if (any(is.na(egfr))) {
    message("egfr contains missing values")
  }
  if (any(is.na(bsa))) {
    message("bsa contains missing values")
  }

  aegfr <- convert_rel_to_abs(egfr, bsa)
  attr(aegfr, "units") <- "mL/min"
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
