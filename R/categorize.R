#' Converts continuous variable into factor categories.
#'
#' @param continuous_var continuous variable data
#' @param nbins number of bins to break data into, default is 4
#' @param units string, optional units string to add to labels of categorized data
#' @param type type argument for stats::quantile, default is 7
#' @param digits number of digits to round quantile breaks to for labels, default is 1
#'
#' @return a vector of categorized data as factor
#' @export
#'
#' @examples
#' x <- rnorm(1000, mean = 10, sd = 5)
#' xc <- categorize(x, nbins = 5)
categorize <- function(
  continuous_var,
  nbins = 4,
  units = "",
  type = 7,
  digits = 1
) {
  checkmate::assertNumeric(continuous_var)

  if (
    continuous_var %>%
      stats::na.omit() %>%
      unique() %>%
      length() <
      nbins
  ) {
    stop("continuous_var contains less than nbins unique values.")
  }
  if (any(is.na(continuous_var))) {
    warning(
      "Input continuous_var has NA values, They will be removed in quantile calcuilation."
    )
  }

  if (nbins < 2) stop("The number of bins (nbins) must be at least 2.")

  quantiles <- stats::quantile(
    continuous_var,
    probs = seq(0, 1, length.out = nbins + 1),
    na.rm = TRUE,
    type = type
  )

  labels <- sapply(1:nbins, function(i) {
    if (i == 1) {
      lab <- paste0("< ", round(quantiles[i + 1], digits))
      if (units != "") {
        lab <- paste0(lab, " ", units)
      }
      lab
    } else if (i == nbins) {
      lab <- paste0("> ", round(quantiles[i], digits))
      if (units != "") {
        lab <- paste0(lab, " ", units)
      }
      lab
    } else {
      lab <- paste0(
        round(quantiles[i], digits),
        " - ",
        round(quantiles[i + 1], digits)
      )
      if (units != "") {
        lab <- paste0(lab, " ", units)
      }
      lab
    }
  })

  continuous_quantile <- factor(
    cut(
      continuous_var,
      breaks = quantiles,
      include.lowest = TRUE,
      labels = labels
    ),
    levels = labels
  )

  return(continuous_quantile)
}
