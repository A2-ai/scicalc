#' Check if Sex is Female
#'
#' @param x input character representing female or male
#'
#' @return boolean representing female
#'
#' @family demographics
#' @export
#'
#' @examples
#' is_female("F")
#'
#' is_female(c("MALE", "FEMALE"))
#'
#' is_female(c(1, 0, -999))
is_female <- function(x) {
  if (is.numeric(x)) {
    message("Numeric input detected - assuming 1 = Female, 0 = Male.")
    return(dplyr::case_when(
      x == 1    ~ TRUE,
      x == 0    ~ FALSE,
      .default  = NA
    ))
  }

  checkmate::assert_character(x)
  x <- tolower(x)
  first_letter <- substr(x, 1, 1)
  return(ifelse(first_letter == "f", TRUE, FALSE))
}

#' Check if Race is White
#'
#' @param x input character representing race
#'
#' @return boolean representing Race == White
#'
#' @family demographics
#' @export
#'
#' @examples
#' is_white("WHITE")
#'
#' is_white("BLACK")
#'
#' is_white(1)
is_white <- function(x) {
  if (is.numeric(x)) {
    message("Numeric input detected - assuming 1 = White, 2 = Black, 3 = Asian, 4 = Other, -999 = Missing.")
    return(dplyr::case_when(
      x == 1     ~ TRUE,
      x %in% c(2, 3, 4) ~ FALSE,
      .default   = NA
    ))
  }

  checkmate::assert_character(x)

  x <- tolower(x)

  return(ifelse(
    x == "white",
    TRUE,
    FALSE
  ))
}

#' Check if Race is Black
#'
#' @param x input character representing race
#'
#' @return boolean representing Race == Black
#'
#' @family demographics
#' @export
#'
#' @examples
#' is_black("WHITE")
#'
#' is_black(c("AFRICAN AMERICAN", "BLACK"))
#'
#' is_black(2)
is_black <- function(x) {
  if (is.numeric(x)) {
    message("Numeric input detected - assuming 1 = White, 2 = Black, 3 = Asian, 4 = Other, -999 = Missing.")
    return(dplyr::case_when(
      x == 2     ~ TRUE,
      x %in% c(1, 3, 4) ~ FALSE,
      .default   = NA
    ))
  }

  checkmate::assert_character(x)

  x <- tolower(x)

  return(ifelse(
    x %in% c("black", "african american", "black or african american"),
    TRUE,
    FALSE
  ))
}

#' Check if Race is Asian
#'
#' @param x input character representing race
#'
#' @return boolean representing Race == Asian
#'
#' @family demographics
#' @export
#'
#' @examples
#' is_asian("ASIAN")
#'
#' is_asian("BLACK")
#'
#' is_asian(3)
is_asian <- function(x) {
  if (is.numeric(x)) {
    message("Numeric input detected - assuming 1 = White, 2 = Black, 3 = Asian, 4 = Other, -999 = Missing.")
    return(dplyr::case_when(
      x == 3     ~ TRUE,
      x %in% c(1, 2, 4) ~ FALSE,
      .default   = NA
    ))
  }

  checkmate::assert_character(x)

  x <- tolower(x)

  return(ifelse(
    x == "asian",
    TRUE,
    FALSE
  ))
}

#' Check if Race is Other
#'
#' @param x input character representing race
#'
#' @return boolean representing Race == Other
#'
#' @family demographics
#' @export
#'
#' @examples
#' is_other("OTHER")
#'
#' is_other("BLACK")
#'
#' is_other(4)
is_other <- function(x) {
  if (is.numeric(x)) {
    message("Numeric input detected - assuming 1 = White, 2 = Black, 3 = Asian, 4 = Other, -999 = Missing.")
    return(dplyr::case_when(
      x == 4     ~ TRUE,
      x %in% c(1, 2, 3) ~ FALSE,
      .default   = NA
    ))
  }

  checkmate::assert_character(x)

  x <- tolower(x)

  return(ifelse(
    x == "other",
    TRUE,
    FALSE
  ))
}

#' Check if Ethnicity is Hispanic or Latino
#'
#' @param x input character representing ethnicity
#'
#' @return boolean representing Ethnic == "Hispanic or Latino"
#'
#' @family demographics
#' @export
#'
#' @examples
#' is_hispanic_or_latino("HISPANIC OR LATINO")
#'
#' is_hispanic_or_latino("NOT HISPANIC OR LATINO")
#'
#' is_hispanic_or_latino("UNKNOWN")
#'
#' is_hispanic_or_latino(1)
is_hispanic_or_latino <- function(x) {
  if (is.numeric(x)) {
    message("Numeric input detected - assuming 1 = Hispanic or Latino, 0 = Not Hispanic or Latino, -999 = Missing.")
    return(dplyr::case_when(
      x == 1     ~ TRUE,
      x == 0     ~ FALSE,
      .default   = NA
    ))
  }

  checkmate::assert_character(x)

  x <- tolower(x)

  return(ifelse(
    x %in% c("hispanic or latino", "hispanic", "latino"),
    TRUE,
    FALSE
  ))
}

#' Check if Ethnicity is Not Hispanic or Latino
#'
#' @param x input character representing ethnicity
#'
#' @return boolean representing Ethnic == "Not Hispanic or Latino"
#'
#' @family demographics
#' @export
#'
#' @examples
#' is_not_hispanic_or_latino("HISPANIC OR LATINO")
#'
#' is_not_hispanic_or_latino("NOT HISPANIC OR LATINO")
#'
#' is_not_hispanic_or_latino("UNKNOWN")
#'
#' is_not_hispanic_or_latino(0)
is_not_hispanic_or_latino <- function(x) {
  if (is.numeric(x)) {
    message("Numeric input detected - assuming 1 = Hispanic or Latino, 0 = Not Hispanic or Latino, -999 = Missing.")
    return(dplyr::case_when(
      x == 0     ~ TRUE,
      x == 1     ~ FALSE,
      .default   = NA
    ))
  }

  checkmate::assert_character(x)

  x <- tolower(x)

  return(ifelse(
    x %in% c("not hispanic or latino", "not hispanic", "not latino"),
    TRUE,
    FALSE
  ))
}
