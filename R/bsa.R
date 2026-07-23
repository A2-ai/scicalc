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
    bsa <- .bsa_dubois(weight, height)
  } else if (tolower(method) == "mosteller") {
    bsa <- .bsa_mosteller(weight, height)
  }
  return(bsa)
}

#' @noRd
.bsa_dubois <- function(weight, height) {
  weight_input <- mask_missing_computation_input(weight, "weight")
  height_input <- mask_missing_computation_input(height, "height")
  weight <- weight_input$value
  height <- height_input$value

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
  bsa <- apply_mv_mask(bsa, weight_input$mask, height_input$mask)
  attr(bsa, "scicalc_units") <- "m^2"
  return(bsa)
}

#' @noRd
.bsa_mosteller <- function(weight, height) {
  weight_input <- mask_missing_computation_input(weight, "weight")
  height_input <- mask_missing_computation_input(height, "height")
  weight <- weight_input$value
  height <- height_input$value

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
  bsa <- apply_mv_mask(bsa, weight_input$mask, height_input$mask)
  attr(bsa, "scicalc_units") <- "m^2"
  return(bsa)
}


#' Calculate Body Surface Area Using Du Bois Equation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated; use [bsa()] with `method = "Dubois"`.
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
#' @keywords internal
#' @export
#'
#' @examples
#' b <- bsa(80.56, 167, method = "Dubois")
dubois_bsa <- function(weight, height) {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "dubois_bsa()",
    with = 'bsa(method = "Dubois")'
  )
  .bsa_dubois(weight, height)
}


#' Calculate Body Surface Area Using Mosteller Equation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated; use [bsa()] with `method = "Mosteller"`.
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
#' @keywords internal
#' @export
#'
#' @examples
#' mosteller_bsa <- bsa(70, 170, method = "Mosteller")
mosteller_bsa <- function(weight, height) {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "mosteller_bsa()",
    with = 'bsa(method = "Mosteller")'
  )
  .bsa_mosteller(weight, height)
}
