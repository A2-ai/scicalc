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
#'    ID = 1:20,
#'    AGE = c(18, 24, 24, 23, 24, 40, 50, 55, 65, 70),
#'    )
#' df <- dplyr::mutate(df, egfr = egfr(SEXF, RACEB, AGE, CREAT, "CKDEPI 2009"))
#' df
agec <- function(age) {
  checkmate::assertNumeric(age)

  # give message if any NAs
  if (any(is.na(age))) {
    message("weight contains missing values")
  }
	
	#TODO: fix up division check. Maybe age * 365 < 28
  dplyr::case_when(
    age < 28 / 365 ~ 1, # Neonate
    28 / 265 <= age & age < 2 ~ 2, # Infant
    2 <= age & age < 12 ~ 3, # Child
    12 <= age & age < 18 ~ 4, # Adolescent
    18 <= age & age < 65 ~ 5, # Adult
    65 <= age ~ 6, # Elder Adult,
    .default = -999
  )
}
