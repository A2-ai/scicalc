#' Takes character input and returns TRUE/FALSE if female/male
#'
#' @param x input character representing female or male
#'
#' @return boolean representing female
#' @export
#'
#' @examples
#' is_female("F")
#'
#' is_female(c("MALE", "FEMALE"))
is_female <- function(x) {
  checkmate::assert_character(x)
  x <- tolower(x)
  first_letter <- substr(x, 1, 1)
  return (ifelse(first_letter == "f", TRUE, FALSE))
}

#' Takes character input and returns TRUE/FALSE if white/other
#'
#' @param x input character representing race
#'
#' @return boolean representing Race == White
#' @export
#'
#' @examples
#' is_white("WHITE")
#'
#' is_white("BLACK")
is_white <- function(x) {
  checkmate::assert_character(x)

  x <- tolower(x)

  return (ifelse(
    x == "white",
    TRUE,
    FALSE
  ))
}

#' Takes character input and returns TRUE/FALSE if black/other also checks for "African American" and "Black or African American"
#'
#' @param x input character representing race
#'
#' @return boolean representing Race == Black
#' @export
#'
#' @examples
#' is_black("WHITE")
#'
#' is_black(c("AFRICAN AMERICAN", "BLACK"))
is_black <- function(x) {
  checkmate::assert_character(x)

  x <- tolower(x)

  return (ifelse(
    x == "black",
    TRUE,
    ifelse(
      x == "african american",
      TRUE,
      ifelse(
        x == "black or african american",
        TRUE,
        FALSE
      )
    )
  ))
}

#' Takes character input and returns TRUE/FALSE if asian/other
#'
#' @param x input character representing race
#'
#' @return boolean representing Race == Asian
#' @export
#'
#' @examples
#' is_asian("ASIAN")
#'
#' is_asian("BLACK")
is_asian <- function(x) {
  checkmate::assert_character(x)

  x <- tolower(x)

  return (ifelse(
    x == "asian",
    TRUE,
    FALSE
  ))
}

#' Takes character input and returns TRUE/FALSE if other/explicit race
#'
#' @param x input character representing race
#'
#' @return boolean representing Race == Other
#' @export
#'
#' @examples
#' is_other("OTHER")
#'
#' is_other("BLACK")
is_other <- function(x) {
  checkmate::assert_character(x)

  x <- tolower(x)

  return (ifelse(
    x == "other",
    TRUE,
    FALSE
  ))
}

#' Takes character input and returns TRUE/FALSE if "Hispanic or Latino" or other
#'
#' @param x input character representing ethnicity
#'
#' @return boolean representing Ethnic == "Hispanic or Latino"
#' @export
#'
#' @examples
#' is_hispanic_or_latino("HISPANIC OR LATINO")
#'
#' is_hispanic_or_latino("NOT HISPANIC OR LATINO")
#'
#' is_hispanic_or_latino("UNKNOWN")
is_hispanic_or_latino <- function(x) {
  checkmate::assert_character(x)

  x <- tolower(x)

  return (ifelse(
    x == "hispanic or latino",
    TRUE,
    FALSE
  ))
}

#' Takes character input and returns TRUE/FALSE if "Not Hispanic or Latino" or other
#'
#' @param x input character representing ethnicity
#'
#' @return boolean representing Ethnic == "Not Hispanic or Latino"
#' @export
#'
#' @examples
#' is_not_hispanic_or_latino("HISPANIC OR LATINO")
#'
#' is_not_hispanic_or_latino("NOT HISPANIC OR LATINO")
#'
#' is_not_hispanic_or_latino("UNKNOWN")
is_not_hispanic_or_latino <- function(x) {
  checkmate::assert_character(x)

  x <- tolower(x)

  return (ifelse(
    x == "not hispanic or latino",
    TRUE,
    FALSE
  ))
}





