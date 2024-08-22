#' Takes character input and returns standard yspec numeric value for Sex.
#'
#' @description
#' Also returns numeric for single character Sex characters "F" and "M"
#'
#' @param sex Sex character
#'
#' @return the standard yspec numeric value for the inputted Sex character
#' @export
#'
#' @examples
#' sexf("FEMALE") # 1
#' sexf("female") # 1
#' sexf("f") # 1
#'
#' sexf("MALE") # 0
#'
#' sexf("NOT SPECIFIED") # 0
sexf <- function(sex) {
  return(ifelse(is_female(sex), 1, 0))
}

#' Takes character input and returns standard yspec numeric value for Race
#'
#' @param racec Race character
#'
#' @return the standard yspec numeric value for the inputted Race character
#' @export
#'
#' @examples
#' racen("WHITE") # 1
#'
#' racen("BLACK") # 2
#'
#' racen("ASIAN") # 3
#'
#' racen("OTHER") # 4
#'
#' racen("UNKNOWN") # -999
racen <- function(racec) {
  # check that racec is character
  checkmate::assert_character(racec)
  racec <- tolower(racec)

  racen <- dplyr::case_when(
    is_white(racec) ~ 1,
    is_black(racec) ~ 2,
    is_asian(racec) ~ 3,
    is_other(racec) ~ 4,
    .default = -999
  )

  return(racen)
}

#' Takes character input and returns standard yspec numeric value for Ethnic
#'
#' @param ethnicc Ethnic character
#'
#' @return the standard yspec numeric value for the inputted Ethnic character
#' @export
#'
#' @examples
#' ethnicn("HISPANIC OR LATINO") # 1
#'
#' ethnicn("NOT HISPANIC OR LATINO") # 0
#'
#' ethnicn("UNKNOWN") # -999
ethnicn <- function(ethnicc) {
  # check that ethnicc is character
  checkmate::assert_character(ethnicc)
  ethnicc <- tolower(ethnicc)

  ethnicn <- dplyr::case_when(
    is_hispanic_or_latino(ethnicc) ~ 1,
    is_not_hispanic_or_latino(ethnicc) ~ 0,
    .default = -999
  )

  return(ethnicn)
}
