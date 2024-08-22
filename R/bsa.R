#' Calculates Body Surface Area based on Weight and Height using the method specified. Default is Dubois.
#' @param weight weight of a subject (kg)
#' @param height height of a subject (cm)
#' @param method String to dictate which equation to use. Dubois or Mosteller.
#'
#' @return bsa (m^2)
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
  return (bsa)
}



#' Calculates Body Surface Area based on Weight and Height using Dubois Dubois equation
#'
#' @param weight weight of subject (kg)
#' @param height height of subject (cm)
#'
#' @return the body surface area (m^2)
#' @export
#'
#' @examples
#' #' b <- dubois_bsa(80.56, 167)
#'
#' df <- data.frame(
#' "WT" = c(80.56, 71.53, 81.04, 70.17),
#' "HT" = c(167, 161, 163, 164)
#' )
#' df <- dplyr::mutate(df, bsa = dubois_bsa(WT, HT))
dubois_bsa <- function(weight, height) {
 # check that weight and height are numeric
 checkmate::assertNumeric(weight)
 checkmate::assertNumeric(height)

 # give message if any NAs
 if (any(is.na(weight))) {
   message('weight contains missing values')
 }
 if (any(is.na(height))) {
   message('height contains missing values')
 }

 bsa <- (weight^0.425) * (height^0.725) * 0.007184

 return(bsa)
} # bsa


#' Calculates Body Surface Area based on Weight and Height using Mosteller equation
#'
#' @param weight weight of subject (kg)
#' @param height height of subject (cm)
#'
#' @return the body surface area (m^2)
#' @export
#'
#' @examples
#' mosteller_bsa(70, 170)
mosteller_bsa <- function(weight, height) {
  checkmate::assertNumeric(height)
  checkmate::assertNumeric(weight)

  if (any(is.na(height))) {
    message("height contains missing values")
  }
  if (any(is.na(weight))) {
    message("weight contains missing values")
  }

  bsa <- sqrt(height * weight / 3600)
  return (bsa)
}
