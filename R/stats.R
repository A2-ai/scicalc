#' Computes the coefficient of variation of input vector.
#'
#' @param x Input vector to compute CV for.
#' @param na.rm boolean to remove NA. default is FALSE
#'
#' @return CV of x. Standard deviation divided by mean. If you want % you'll need to multiply by 100
#' @export
#'
#' @examples
#' cv(c(1, 2, 1, 1, 2, 1, 2, 3))
cv <- function(x, na.rm = FALSE) {
  checkmate::assertNumeric(x)

  if (any(is.na(x))) {
    if (na.rm) {
      message("Your data contains NA and will be removed.")
    } else {
      warning("Your data contains NA. Please make sure na.rm is appropriately set.")
    }
  }

  return (stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm))
}


#' Computes the geometric mean of a vector.
#'
#' @param x vector to compute geometric mean of
#' @param na.rm boolean to remove NA from vector in calcualtion. Default is False
#'
#' @return geometric mean of input vector x
#' @export
#'
#' @examples
#' geom_mean(c(1, 2, 3, 2, 1))
geom_mean <- function(x, na.rm = FALSE) {
  checkmate::assertNumeric(x)

  if (any(is.na(x))) {
    if (na.rm) {
      message("Your data contains NA and will be removed.")
    } else {
      warning("Your data contains NA. Please make sure na.rm is appropriately set.")
    }
  }

  return (exp(mean(log(x), na.rm = na.rm)))
}

#' Computes the geometric standard deviation of a vector x.
#'
#' @param x The vector of data you want the geometric sd of.
#' @param na.rm a boolean to remove NA values. Default is False
#'
#' @return the geometric standard deviation of x
#' @export
#'
#' @examples
#' geom_sd(c(1, 2, 3, 2, 1))
geom_sd <- function(x, na.rm = FALSE) {
  checkmate::assertNumeric(x)

  if (any(is.na(x))) {
    if (na.rm) {
      message("Your data contains NA and will be removed.")
    } else {
      warning("Your data contains NA. Please make sure na.rm is appropriately set.")
    }
  }

  return (exp(stats::sd(log(x), na.rm = na.rm)))
}

#' Computes the geometric CV of a vector x
#'
#' @param x vector of data you want the geometric CV of.
#' @param na.rm boolean to remove NA from vector. Default is FALSE
#'
#' @return the geometric CV of the input vector x
#' @export
#'
#' @examples
#' geom_cv(c(1, 2, 3, 2, 1))
geom_cv <- function(x, na.rm = FALSE) {
  checkmate::assertNumeric(x)

  if (any(is.na(x))) {
    if (na.rm) {
      message("Your data contains NA and will be removed.")
    } else {
      warning("Your data contains NA. Please make sure na.rm is appropriately set.")
    }
  }

  return (sqrt(exp(stats::sd(log(x), na.rm = na.rm)**2) - 1))
}
