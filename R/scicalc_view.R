# Format each value of a mixed-units vector as its magnitude followed by its
# row-level unit. Used by scicalc_view() for interactive display.
#' @noRd
format_mixed_fast <- function(x) {
  elements <- unclass(x)

  # A malformed or legacy mixed-units vector can contain NULL for a missing
  # element. It has no magnitude or unit to display, but the viewer should
  # still be able to render the rest of the column.
  has_value <- vapply(elements, length, integer(1)) > 0L

  if (any(!has_value)) {
    output <- rep(NA_character_, length(elements))
    present <- elements[has_value]
    values <- vapply(present, as.numeric, numeric(1))
    unit_labels <- vapply(
      present,
      \(element) as.character(units(element)),
      character(1)
    )

    output[has_value] <- paste0(
      format(values, trim = TRUE, drop0trailing = TRUE),
      " [", unit_labels, "]"
    )

    return(output)
  }

  # Extract magnitudes without invoking format/drop_units repeatedly.
  values <- vapply(
    elements,
    as.numeric,
    numeric(1)
  )

  # Extract symbolic units, then stringify each distinct unit only once.
  unit_objects <- unclass(units(x))
  unique_units <- unique(unit_objects)
  unit_index <- match(unit_objects, unique_units)

  unit_labels <- vapply(
    unique_units,
    as.character,
    character(1)
  )

  formatted_values <- format(
    values,
    trim = TRUE,
    drop0trailing = TRUE
  )

  paste0(
    formatted_values,
    " [", unit_labels[unit_index], "]"
  )
}

#' Prepare a Data Frame for Interactive Viewing
#'
#' Replaces `mixed_units` columns with their formatted character display while
#' preserving all other columns and the data-frame class. Use this when an
#' interactive QMD notebook or data viewer displays mixed-unit values as
#' `<S3: mixed_units>` rather than their magnitudes and units.
#'
#' This is a display copy: the input data frame is not modified.
#'
#' @param x A data frame or tibble.
#'
#' @return A data frame or tibble with `mixed_units` columns converted to
#'   character display columns.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   analyte = c("payload", "ADC"),
#'   value = units::mixed_units(c(0.158, 2.24), c("ng/mL", "ug/mL"))
#' )
#' scicalc_view(df)
scicalc_view <- function(x) {
  stopifnot(is.data.frame(x))

  display <- x

  mixed_columns <- vapply(
    display,
    \(column) inherits(column, "mixed_units"),
    logical(1)
  )

  display[mixed_columns] <- lapply(
    display[mixed_columns],
    format_mixed_fast
  )

  display
}
