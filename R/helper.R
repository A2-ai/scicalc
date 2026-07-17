#' Assert and strip units from input
#'
#' If `x` is a `units` object, convert to `expected_unit` and strip units.
#' If `x` is plain numeric, pass through unchanged. If units are incompatible,
#' throw an informative error.
#'
#' @param x numeric or units vector
#' @param expected_unit character string of the expected unit (e.g. "kg", "cm")
#' @param arg_name character name of the argument (for error messages)
#'
#' @return numeric vector
#' @keywords internal
assert_and_strip_units <- function(x, expected_unit, arg_name = deparse(substitute(x))) {
  if (!inherits(x, "units")) return(x)

  missing_mask <- is_missing_value(x)
  missing_value <- getOption("scicalc.missing_value", -999)
  supplied_unit <- as.character(units::deparse_unit(x))
  target <- units::as_units(expected_unit)
  result <- tryCatch(
    units::drop_units(units::set_units(x, target, mode = "standard")),
    error = function(e) {
      rlang::abort(paste0(
        "`", arg_name, "` has units [", supplied_unit,
        "] which cannot be converted to [", expected_unit, "]."
      ))
    }
  )

  if (as.character(units::deparse_unit(x)) != as.character(units::deparse_unit(target))) {
    rlang::inform(paste0(
      "`", arg_name, "`: converted from [", supplied_unit, "] to [", expected_unit, "]."
    ))
  }

  # The sentinel identifies missing data before unit conversion. Restore it
  # after conversion so downstream calculation functions can mask it reliably.
  result[missing_mask] <- missing_value

  result
}

#' Restore attributes dropped by a units operation
#'
#' Copies attributes present on `old` onto `new` where `new` lacks them, so
#' metadata such as a `label` survives operations (e.g. [units::set_units()])
#' that drop it. Structural and units-managed attributes (`class`, `units`,
#' `names`, `dim`, `dimnames`) are never copied, so the class, units, and shape
#' of `new` are left intact.
#'
#' @param new the vector produced by the operation.
#' @param old the vector whose attributes should be carried over.
#'
#' @return `new` with `old`'s non-structural attributes restored.
#' @keywords internal
restore_attrs <- function(new, old) {
  old_attrs <- attributes(old)
  if (is.null(old_attrs)) {
    return(new)
  }

  managed <- c("class", "units", "names", "dim", "dimnames")
  to_copy <- setdiff(names(old_attrs), c(managed, names(attributes(new))))

  for (a in to_copy) {
    attr(new, a) <- old_attrs[[a]]
  }
  new
}

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
    x == "other",
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
