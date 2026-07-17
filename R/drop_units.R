#' Drop Units from Vectors and Data Frames
#'
#' A `scicalc` replacement for [units::drop_units()] that also supports
#' data frames containing `units` and `mixed_units` columns. It returns the
#' numeric magnitudes, preserves the data-frame class, and treats a `NULL`
#' element in a legacy `mixed_units` vector as `NA_real_`.
#'
#' @param x A vector, data frame, or tibble.
#'
#' @return `x` with units removed from every standard or mixed-units value.
#' @export
#'
#' @examples
#' drop_units(units::set_units(c(1, 2), "mg"))
#'
#' df <- data.frame(dose = units::set_units(c(1, 2), "mg"))
#' drop_units(df)
drop_units <- function(x) {
  if (is.data.frame(x)) {
    output <- x
    output[] <- lapply(x, drop_units)
    return(output)
  }

  if (inherits(x, "mixed_units")) {
    return(vapply(
      unclass(x),
      function(value) {
        if (is.null(value)) {
          NA_real_
        } else {
          as.numeric(units::drop_units(value))
        }
      },
      numeric(1)
    ))
  }

  if (inherits(x, "units")) {
    return(as.numeric(units::drop_units(x)))
  }

  x
}
