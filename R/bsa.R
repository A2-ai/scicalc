#' Calculate Body Surface Area
#' @param weight weight of a subject (kg)
#' @param height height of a subject (cm)
#' @param method String to dictate which equation to use. Dubois or Mosteller.
#'
#' @return bsa (m^2)
#'
#' @family body_composition
#' @export
#'
#' @examples
#' bsa(70, 170)
#' bsa(70, 170, method = "Mosteller")
#' bsa(70, 170, method = "Dubois")
bsa <- function(weight, height, method = "Dubois") {
  checkmate::assert_choice(tolower(method), c("dubois", "mosteller"))

  if (tolower(method) == "dubois") {
    bsa <- dubois_bsa(weight, height)
  } else if (tolower(method) == "mosteller") {
    bsa <- mosteller_bsa(weight, height)
  }
  return(bsa)
}


#' Calculate Body Surface Area Using Du Bois Equation
#'
#' @param weight weight of subject (kg)
#' @param height height of subject (cm)
#'
#' @details
#' The Du Bois equation for BSA:
#' \deqn{BSA = 0.007184 \cdot W^{0.425} \cdot H^{0.725}}{BSA = 0.007184 * W^0.425 * H^0.725}
#'
#' where:
#' \itemize{
#'   \item \eqn{W} = weight (kg)
#'   \item \eqn{H} = height (cm)
#' }
#'
#' @return the body surface area (m^2)
#'
#' @family body_composition
#' @export
#'
#' @examples
#' b <- dubois_bsa(80.56, 167)
#'
#' df <- data.frame(
#' "WT" = c(80.56, 71.53, 81.04, 70.17),
#' "HT" = c(167, 161, 163, 164)
#' )
#' df <- dplyr::mutate(df, bsa = dubois_bsa(WT, HT))
#' df
dubois_bsa <- function(weight, height) {
  # check that weight and height are numeric
  checkmate::assertNumeric(weight)
  checkmate::assertNumeric(height)

  input_lengths <- lengths(list(weight, height))
  if (length(unique(input_lengths)) != 1) {
    warning("Inputs have different lengths! Please check data.")
  }

  # give message if any NAs
  if (any(is.na(weight))) {
    message('weight contains missing values')
  }
  if (any(is.na(height))) {
    message('height contains missing values')
  }

  bsa <- (weight^0.425) * (height^0.725) * 0.007184
  attr(bsa, "units") <- "m^2"
  return(bsa)
} # bsa


#' Calculate Body Surface Area Using Mosteller Equation
#'
#' @param weight weight of subject (kg)
#' @param height height of subject (cm)
#'
#' @details
#' The Mosteller equation for BSA:
#' \deqn{BSA = \sqrt{\frac{W \cdot H}{3600}}}{BSA = sqrt(W * H / 3600)}
#'
#' where:
#' \itemize{
#'   \item \eqn{W} = weight (kg)
#'   \item \eqn{H} = height (cm)
#' }
#'
#' @return the body surface area (m^2)
#'
#' @family body_composition
#' @export
#'
#' @examples
#' mosteller_bsa(70, 170)
mosteller_bsa <- function(weight, height) {
  checkmate::assertNumeric(height)
  checkmate::assertNumeric(weight)

  input_lengths <- lengths(list(weight, height))
  if (length(unique(input_lengths)) != 1) {
    warning("Inputs have different lengths! Please check data.")
  }

  if (any(is.na(height))) {
    message("height contains missing values")
  }
  if (any(is.na(weight))) {
    message("weight contains missing values")
  }

  bsa <- sqrt(height * weight / 3600)
  attr(bsa, "units") <- "m^2"
  return(bsa)
}
