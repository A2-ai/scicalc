#' Convert Sex to Numeric Code
#'
#' @description
#' Also returns numeric for single character Sex characters "F" and "M"
#'
#' @param sex Sex character
#'
#' @return the standard yspec numeric value for the inputted Sex character
#'
#' @family demographics
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

#' Convert Race to Numeric Code
#'
#' @param racec Race character
#'
#' @return the standard yspec numeric value for the inputted Race character
#'
#' @family demographics
#' @export
#'
#' @details
#' Default mapping: White = 1, Black = 2, Asian = 3, American Native = 4,
#' Pacific Islander = 5, Other = 6. `"UNKNOWN"` and `NA` map to the missing
#' value indicator (`getOption("scicalc.missing_value", -999)`). Any other
#' value is treated as unspecified: it maps to `NA` and triggers a warning.
#'
#' The `scicalc.racen_config` option (a named numeric vector) customizes the
#' mapping. A name matching a built-in category (`"white"`, `"black"`,
#' `"asian"`, `"american native"`, `"pacific islander"`, `"other"`) overrides
#' that category's code (synonyms included). Any other name adds a new
#' exact-match category. For example
#' `options(scicalc.racen_config = c("WHITE" = 2, "JAPANESE" = 7))`.
#'
#' @examples
#' racen("WHITE") # 1
#'
#' racen("BLACK") # 2
#'
#' racen("ASIAN") # 3
#'
#' racen("AMERICAN INDIAN OR ALASKA NATIVE") # 4
#'
#' racen("PACIFIC ISLANDER") # 5
#'
#' racen("OTHER") # 6
#'
#' racen("UNKNOWN") # default missing value
racen <- function(racec) {
  # check that racec is character
  checkmate::assert_character(racec)
  racec <- tolower(racec)

  config <- validate_racen_config(getOption("scicalc.racen_config", NULL))
  if (is.null(config)) {
    config <- stats::setNames(numeric(0), character(0))
  }

  codes <- c(
    "white" = 1,
    "black" = 2,
    "asian" = 3,
    "american native" = 4,
    "pacific islander" = 5,
    "other" = 6
  )
  overrides <- config[names(config) %in% names(codes)]
  codes[names(overrides)] <- overrides

  novel <- config[!(names(config) %in% names(codes))]

  racen <- dplyr::case_when(
    racec %in% names(novel) ~ unname(novel[racec]),
    is_white(racec) ~ codes[["white"]],
    is_black(racec) ~ codes[["black"]],
    is_asian(racec) ~ codes[["asian"]],
    is_american_native(racec) ~ codes[["american native"]],
    is_pacific_islander(racec) ~ codes[["pacific islander"]],
    is_other(racec) ~ codes[["other"]],
    is_unspecified(racec, known = names(novel)) ~ NA_real_,
    .default = getOption("scicalc.missing_value", -999)
  )

  return(racen)
}

#' Validate the scicalc.racen_config option
#'
#' @param config the value of `getOption("scicalc.racen_config")`
#'
#' @return the validated config with lower-cased names, or `NULL`
#' @keywords internal
validate_racen_config <- function(config) {
  if (is.null(config)) {
    return(NULL)
  }

  checkmate::assert_numeric(config, names = "named", any.missing = FALSE)

  names(config) <- tolower(names(config))
  if (anyDuplicated(names(config)) > 0) {
    rlang::abort(
      "`scicalc.racen_config` has duplicate names (case-insensitive)."
    )
  }

  config
}

#' Convert Ethnicity to Numeric Code
#'
#' @param ethnicc Ethnic character
#'
#' @return the standard yspec numeric value for the inputted Ethnic character
#'
#' @family demographics
#' @export
#'
#' @examples
#' ethnicn("HISPANIC OR LATINO") # 1
#'
#' ethnicn("NOT HISPANIC OR LATINO") # 0
#'
#' ethnicn("UNKNOWN") # default missing value
ethnicn <- function(ethnicc) {
  # check that ethnicc is character
  checkmate::assert_character(ethnicc)
  ethnicc <- tolower(ethnicc)

  ethnicn <- dplyr::case_when(
    is_hispanic_or_latino(ethnicc) ~ 1,
    is_not_hispanic_or_latino(ethnicc) ~ 0,
    .default = getOption("scicalc.missing_value", -999)
  )

  return(ethnicn)
}
