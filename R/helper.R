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
    message("Numeric input detected - assuming 1 = White, 2 = Black, 3 = Asian, 4 = American Native, 5 = Pacific Islander, 6 = Other, -999 = Missing.")
    return(dplyr::case_when(
      x == 1     ~ TRUE,
      x %in% c(2, 3, 4, 5, 6) ~ FALSE,
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
    message("Numeric input detected - assuming 1 = White, 2 = Black, 3 = Asian, 4 = American Native, 5 = Pacific Islander, 6 = Other, -999 = Missing.")
    return(dplyr::case_when(
      x == 2     ~ TRUE,
      x %in% c(1, 3, 4, 5, 6) ~ FALSE,
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
    message("Numeric input detected - assuming 1 = White, 2 = Black, 3 = Asian, 4 = American Native, 5 = Pacific Islander, 6 = Other, -999 = Missing.")
    return(dplyr::case_when(
      x == 3     ~ TRUE,
      x %in% c(1, 2, 4, 5, 6) ~ FALSE,
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
#' is_other("MULTIPLE")
#'
#' is_other("BLACK")
#'
#' is_other(6)
is_other <- function(x) {
  if (is.numeric(x)) {
    message("Numeric input detected - assuming 1 = White, 2 = Black, 3 = Asian, 4 = American Native, 5 = Pacific Islander, 6 = Other, -999 = Missing.")
    return(dplyr::case_when(
      x == 6     ~ TRUE,
      x %in% c(1, 2, 3, 4, 5) ~ FALSE,
      .default   = NA
    ))
  }

  checkmate::assert_character(x)

  x <- tolower(x)

  return(ifelse(
    x == "other" | x == "multiple",
    TRUE,
    FALSE
  ))
}

#' Check if Race is American Native
#'
#' @param x input character representing race
#'
#' @return boolean representing Race == American Native
#'
#' @family demographics
#' @export
#'
#' @examples
#' is_american_native("AMERICAN INDIAN OR ALASKA NATIVE")
#'
#' is_american_native("BLACK")
#'
#' is_american_native(4)
is_american_native <- function(x) {
  if (is.numeric(x)) {
    message("Numeric input detected - assuming 1 = White, 2 = Black, 3 = Asian, 4 = American Native, 5 = Pacific Islander, 6 = Other, -999 = Missing.")
    return(dplyr::case_when(
      x == 4     ~ TRUE,
      x %in% c(1, 2, 3, 5, 6) ~ FALSE,
      .default   = NA
    ))
  }

  checkmate::assert_character(x)

  x <- tolower(x)

  return(ifelse(
    x %in% c(
      "american indian or alaska native",
      "american native",
      "native american",
      "american indian",
      "alaska native"
    ),
    TRUE,
    FALSE
  ))
}

#' Check if Race is Pacific Islander
#'
#' @param x input character representing race
#'
#' @return boolean representing Race == Pacific Islander
#'
#' @family demographics
#' @export
#'
#' @examples
#' is_pacific_islander("NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER")
#'
#' is_pacific_islander("BLACK")
#'
#' is_pacific_islander(5)
is_pacific_islander <- function(x) {
  if (is.numeric(x)) {
    message("Numeric input detected - assuming 1 = White, 2 = Black, 3 = Asian, 4 = American Native, 5 = Pacific Islander, 6 = Other, -999 = Missing.")
    return(dplyr::case_when(
      x == 5     ~ TRUE,
      x %in% c(1, 2, 3, 4, 6) ~ FALSE,
      .default   = NA
    ))
  }

  checkmate::assert_character(x)

  x <- tolower(x)

  return(ifelse(
    x %in% c(
      "native hawaiian or other pacific islander",
      "pacific islander",
      "native hawaiian"
    ),
    TRUE,
    FALSE
  ))
}

#' Check if Race is Unspecified
#'
#' @description
#' `TRUE` for a race value that is present but matches none of the known
#' categories (`is_white`, `is_black`, `is_asian`, `is_american_native`,
#' `is_pacific_islander`, `is_other`). `"unknown"` and `NA` are treated as
#' missing (not unspecified) and return `FALSE`. When unspecified values are
#' found a warning is emitted naming the distinct offenders.
#'
#' @param x input character representing race
#' @param known optional character vector of additional recognized values to
#'   treat as specified (used by [racen()] to honor `scicalc.racen_config`).
#'
#' @return boolean representing an unrecognized race value
#'
#' @family demographics
#' @export
#'
#' @examples
#' is_unspecified("ROMAN LATIN")
#'
#' is_unspecified("WHITE")
#'
#' is_unspecified("UNKNOWN")
is_unspecified <- function(x, known = character(0)) {
  if (is.numeric(x)) {
    mv <- getOption("scicalc.missing_value", -999)
    message("Numeric input detected - assuming 1 = White, 2 = Black, 3 = Asian, 4 = American Native, 5 = Pacific Islander, 6 = Other, ", mv, " = Missing.")
    return(!(x %in% c(1, 2, 3, 4, 5, 6, mv)) & !is.na(x))
  }

  checkmate::assert_character(x)

  x_lower <- tolower(x)
  known <- tolower(known)

  specified <- is_white(x_lower) |
    is_black(x_lower) |
    is_asian(x_lower) |
    is_american_native(x_lower) |
    is_pacific_islander(x_lower) |
    is_other(x_lower) |
    (x_lower %in% known) |
    (x_lower == "unknown")

  unspecified <- !specified & !is.na(x_lower)

  if (any(unspecified)) {
    offenders <- unique(x[unspecified])
    rlang::warn(paste0(
      "Unspecified race value(s) detected: ",
      paste0('"', offenders, '"', collapse = ", ")
    ))
  }

  unspecified
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
