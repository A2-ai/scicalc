#' Age category function
#'
#' @param age baseline age of subject in years
#'
#' @return age category
#' @export
#'
#' @references
#' Pediatric Drug Development: Regulatory Considerations —
#' Complying With the Pediatric Research Equity Act and
#' Qualifying for Pediatric Exclusivity Under the Best
#' Pharmaceuticals for Children Act Guidance for Industry
#'
#' Guideline for Industry Studies in Support of
#' Special Populations: Geriatrics
#'
#' @examples
#' age_cat <- agec(24)
#'
#' df <- data.frame(
#'   ID = 1:12,
#'   AGE = c(0.07, 28 / 365, 0.25, 1, 2, 4, 12, 16, 18, 24, 65, 70)
#' )
#' df <- dplyr::mutate(df, AGEC = agec(AGE))
#' df
agec <- function(age) {
  checkmate::assertNumeric(age)

  # give message if any NAs
  if (any(is.na(age))) {
    message("age contains missing values")
  }

  if (any(age < 0, na.rm = TRUE)) {
    warning("age contains values less than 0 years. Confirm data is correct.")
  }

  # Oldest person alive currently is 116
  if (any(age > 116, na.rm = TRUE)) {
    warning("age contains values > 116 years. Confirm data is correct.")
  }

  if (any(0 <= age & age < 28 / 365, na.rm = TRUE)) {
    warning("Neonate ages detected. Confirm assignment.")
  }

  if (any(dplyr::near(age, 28 / 365), na.rm = TRUE)) {
    message("Age near Neonate boundary (28 days)")
  }

  # TODO: fix up division check. Maybe age * 365 < 28
  dplyr::case_when(
    0 <= age & age < 28 / 365 ~ 1, # Neonate
    28 / 365 <= age & age < 2 ~ 2, # Infant
    2 <= age & age < 12 ~ 3, # Child
    12 <= age & age < 18 ~ 4, # Adolescent
    18 <= age & age < 65 ~ 5, # Adult
    65 <= age ~ 6, # Elder Adult,
    .default = -999
  )
}
