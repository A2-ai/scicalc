#' Categorize Age
#'
#' Categorizes subjects into FDA-defined age groups for pediatric and geriatric
#' drug development studies.
#'
#' @param age Numeric vector of baseline age in years
#'
#' @details
#' \strong{FDA age categories}:
#' \itemize{
#'   \item 1: Neonate: 0 to <28 days
#'   \item 2: Infant: 28 days to <2 years
#'   \item 3: Child: 2 to <12 years
#'   \item 4: Adolescent: 12 to <18 years
#'   \item 5: Adult: 18 to <65 years
#'   \item 6: Elderly Adult: ≥65 years
#' }
#'
#' @return Integer vector of age categories (1-6). Returns \code{-999} for
#'   missing values. Includes a \code{category_standard} attribute set to "FDA".
#'
#' @family categorization
#'
#' @references
#' Pediatric Drug Development: Regulatory Considerations —
#' Complying With the Pediatric Research Equity Act and
#' Qualifying for Pediatric Exclusivity Under the Best
#' Pharmaceuticals for Children Act Guidance for Industry
#'
#' Guideline for Industry Studies in Support of
#' Special Populations: Geriatrics
#' @export
#' @examples
#' age_cat <- agec(24)
#'
#' df <- data.frame(
#'   ID = 1:12,
#'   AGE = c(0.07, 28 / 365, 0.25, 1, 2, 4, 12, 16, 18, 24, 65, 70)
#' )
#' dplyr::mutate(df, AGEC = agec(AGE))
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
  agec <- dplyr::case_when(
    0 <= age & age < 28 / 365 ~ 1, # Neonate
    28 / 365 <= age & age < 2 ~ 2, # Infant
    2 <= age & age < 12 ~ 3, # Child
    12 <= age & age < 18 ~ 4, # Adolescent
    18 <= age & age < 65 ~ 5, # Adult
    65 <= age ~ 6, # Elder Adult,
    .default = -999
  )
  attr(agec, "category_standard") <- "FDA"
  return(agec)
}
