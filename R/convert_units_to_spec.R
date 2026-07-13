#' Convert Data Frame Columns to Spec Units
#'
#' @description
#' Converts each unit-carrying column of `data` to the unit declared for it in
#' a data specification. Columns that are plain numeric get the spec unit
#' attached (assuming the values are already in that unit) with a warning.
#' Columns whose current units cannot be converted to the spec unit are left
#' untouched and reported together in a warning.
#'
#' Pairs with [pivot_with_units()]: pivot attaches source units, then
#' `convert_units_to_spec()` harmonizes them to the specification.
#'
#' @param data a data frame.
#' @param spec a data specification object; currently a `yspec` object.
#' @param ... reserved for methods.
#'
#' @return `data` with columns converted (values rescaled) or assigned units
#'   per the spec. Columns not in the spec, or with no unit in the spec, are
#'   returned untouched.
#'
#' @family unit_checking
#' @export
#'
#' @examples
#' \dontrun{
#' spec <- yspec::ys_load("analysis.yml")
#' df <- convert_units_to_spec(df, spec)
#' }
convert_units_to_spec <- function(data, spec, ...) {
  UseMethod("convert_units_to_spec", spec)
}

#' @rdname convert_units_to_spec
#' @export
convert_units_to_spec.yspec <- function(data, spec, ...) {
  rlang::check_installed("yspec")
  unit_map <- unlist(yspec::ys_get_unit(spec))
  .convert_units_to_map(data, unit_map)
}

#' @rdname convert_units_to_spec
#' @export
convert_units_to_spec.default <- function(data, spec, ...) {
  rlang::abort(paste0(
    "No `convert_units_to_spec()` method for class <",
    paste(class(spec), collapse = "/"),
    ">."
  ))
}

#' @noRd
.convert_units_to_map <- function(data, unit_map) {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(unit_map, names = "named")

  unit_map <- unit_map[!is.na(unit_map) & unit_map != ""]
  unit_map <- unit_map[names(unit_map) %in% colnames(data)]

  attached <- character(0)
  failed <- character(0)

  for (col in names(unit_map)) {
    target <- unit_map[[col]]

    if (inherits(data[[col]], "units")) {
      current <- as.character(units(data[[col]]))
      converted <- tryCatch(
        units::set_units(data[[col]], target, mode = "standard"),
        error = function(e) NULL
      )
      if (is.null(converted)) {
        failed <- c(failed, paste0(col, " [", current, "] -> [", target, "]"))
      } else {
        data[[col]] <- converted
      }
    } else if (is.numeric(data[[col]])) {
      with_unit <- tryCatch(
        units::set_units(data[[col]], target, mode = "standard"),
        error = function(e) NULL
      )
      if (is.null(with_unit)) {
        failed <- c(failed, paste0(col, " [unitless] -> [", target, "]"))
      } else {
        data[[col]] <- with_unit
        attached <- c(attached, paste0(col, " [", target, "]"))
      }
    }
  }

  if (length(attached) > 0) {
    rlang::warn(paste0(
      "Attached spec units to unitless column(s): ",
      paste(attached, collapse = ", ")
    ))
  }

  if (length(failed) > 0) {
    rlang::warn(paste0(
      "Could not convert column(s) to spec units: ",
      paste(failed, collapse = ", ")
    ))
  }

  data
}
