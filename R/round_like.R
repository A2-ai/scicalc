#' Round numeric values based on explicit or inferred significant digits
#'
#' Priority:
#' 1. If `sdig` is provided, round `x` to that many significant digits.
#' 2. If `sdig` is NULL but `ref` is provided, infer the number of significant digits from `ref`.
#' 3. If both are NULL, return `x` unchanged.
#'
#' @param x Numeric vector to round.
#' @param ref Optional numeric vector used to infer the number of significant digits.
#' @param sdig Optional integer specifying the number of significant digits to apply.
#'
#' @return A numeric vector rounded to the appropriate precision.
#'
#' @export
#'
#' @examples
#' round_like(123.456, sdig = 2)       # -> 120
#' round_like(123.456, ref = 12.3)     # -> 123
#' round_like(123.456, ref = 12.34)    # -> 123.5
#' round_like(123.456)                 # -> 123.456
round_like <- function(x, ref = NULL, sdig = NULL) {
  if (!is.null(sdig)) {
    return(signif(x, sdig))
  }
  if (!is.null(ref)) {
    # Helper to infer number of significant digits from numeric input
    infer_sigfigs <- function(val) {
      sapply(val, function(v) {
        if (is.na(v) || v == 0) return(0)
        str <- format(v, scientific = FALSE, trim = TRUE)
        # remove leading zeros, decimal, trailing zeros
        str <- sub("^0+", "", str)
        str <- sub("\\.", "", str)
        str <- sub("0+$", "", str)
        nchar(str)
      })
    }

    sdig_ref <- infer_sigfigs(ref)
    sdig_use <- ifelse(length(sdig_ref) == 0, 0, min(sdig_ref, na.rm = TRUE))
    return(signif(x, sdig_use))
  }
  if (is.null(sdig) && is.null(ref)) {
    return(x)
  }
}
