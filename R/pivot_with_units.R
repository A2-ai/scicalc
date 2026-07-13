#' Pivot Wider and Attach Units
#'
#' @description
#' A wrapper around [tidyr::pivot_wider()] that additionally attaches a `units`
#' object to each pivoted column, taken from a units column in the long data.
#'
#' Each value in `names_from` must map to a single unit in `units_from`
#' (after normalizing `IU` to `U` and `μ` to `u`); otherwise an error is
#' raised, since a column cannot carry more than one unit. Columns whose unit is
#' missing or empty are returned as plain numeric (no unit attached).
#'
#' @param data a long-format data frame.
#' @param values_from column holding the values to spread (as in
#'   [tidyr::pivot_wider()]).
#' @param units_from column holding the unit associated with each value.
#' @param names_from column whose values become the new column names.
#' @param ... additional arguments passed to [tidyr::pivot_wider()].
#'
#' @return a wide data frame with a `units` object attached to each pivoted
#'   column that has a parseable unit.
#'
#' @family unit_checking
#' @export
#'
#' @examples
#' df <- data.frame(
#'   ID = c(1, 1, 2, 2),
#'   TEST = c("ALT", "AST", "ALT", "AST"),
#'   VAL = c(20, 30, 25, 35),
#'   UNIT = c("U/L", "U/L", "U/L", "U/L")
#' )
#' pivot_with_units(df, values_from = VAL, units_from = UNIT, names_from = TEST)
pivot_with_units <- function(data, values_from, units_from, names_from, ...) {
  checkmate::assert_data_frame(data)

  units_name <- rlang::as_name(rlang::enquo(units_from))
  names_name <- rlang::as_name(rlang::enquo(names_from))
  values_name <- rlang::as_name(rlang::enquo(values_from))
  checkmate::assert_subset(c(values_name, units_name, names_name), colnames(data))

  values_col <- data[[values_name]]

  names_vec <- as.character(data[[names_name]])
  units_vec <- as.character(data[[units_name]])

  if (!check_for_unique_units(names_vec, units_vec)) {
    lookup <- get_unique_units_df(names_vec, units_vec)
    offenders <- unique(lookup$PARAM[duplicated(lookup$PARAM)])
    rlang::abort(paste0(
      "Each value in `", names_name, "` must map to a single unit. ",
      "Multiple units found for: ",
      paste0('"', offenders, '"', collapse = ", "),
      ". Standardize units before pivoting."
    ))
  }

  lookup <- get_unique_units_df(names_vec, units_vec)

  wide <- tidyr::pivot_wider(
    dplyr::select(data, -{{ units_from }}),
    values_from = {{ values_from }},
    names_from = {{ names_from }},
    ...
  )

  for (i in seq_len(nrow(lookup))) {
    param <- lookup$PARAM[i]
    unit <- lookup$UNIT[i]

    if (!param %in% colnames(wide)) next

    if (!is.na(unit) && unit != "") {
      wide[[param]] <- tryCatch(
        units::set_units(wide[[param]], unit, mode = "standard"),
        error = function(e) {
          rlang::abort(paste0(
            "Could not attach unit [", unit, "] to column `", param, "`: ",
            conditionMessage(e)
          ))
        }
      )
    }

    # carry the source value column's attributes (e.g. label) onto each column
    wide[[param]] <- restore_attrs(wide[[param]], values_col)
  }

  wide
}
