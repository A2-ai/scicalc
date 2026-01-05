#' Convert Albumin concentration from g/L to g/dL
#'
#' @param alb albumin concentration (g/L)
#'
#' @return Albumin concentration (g/dL)
#' @export
#'
#' @examples
#' convert_alb(40)
#'
#' df <- data.frame(
#'   ID = c(1, 2, 3, 4),
#'   ALB = c(35, 40, 28, 45)
#' )
#'
#' df <- df %>%
#'   dplyr::group_by(ID) %>%
#'   dplyr::mutate(ALBBL = convert_alb(ALB))
#' df
convert_alb <- function(alb) {
  checkmate::assertNumeric(alb)

  if (any(is.na(alb))) {
    message("alb contains missing values")
  }

  alb_gdl <- alb / 10
  return(alb_gdl)
}

#' Convert Bilirubin concentration from µmol/L to mg/dL
#'
#' @param bili bilirubin concentration (µmol/L)
#'
#' @return Bilirubin concentration (mg/dL)
#' @export
#'
#' @examples
#' convert_bili(17.1) # ≈ 1 mg/dL
#'
#' df <- data.frame(
#'   ID = c(1, 2, 3, 4),
#'   BILI = c(10, 15, 25, 40)
#' )
#'
#' df <- df %>%
#'   dplyr::group_by(ID) %>%
#'   dplyr::mutate(BILIBL = convert_bili(BILI))
#' df
convert_bili <- function(bili) {
  checkmate::assertNumeric(bili)

  if (any(is.na(bili))) {
    message("bili contains missing values")
  }
  mol_weight_bili <- 584.673 # g/mol
  # convert umol/L to mg/dL
  # 1 umol/L * MW g/mol * mol / 10^6 umol * 10^3 mg /g * L / 10 dL
  conversion_factor <- mol_weight_bili / 10^4
  bili_mgdl <- bili * conversion_factor
  return(bili_mgdl)
}

#' Convert Serum Creatinine concentration from µmol/L to mg/dL
#'
#' @param creat serum creatinine concentration (µmol/L)
#'
#' @return Serum Creatinine concentration (mg/dL)
#' @export
#'
#' @examples
#' convert_creat(88.42) # ≈ 1 mg/dL
#'
#' df <- data.frame(
#'   ID = c(1, 2, 3, 4),
#'   CREAT = c(70, 90, 110, 130)
#' )
#'
#' df <- df %>%
#'   dplyr::group_by(ID) %>%
#'   dplyr::mutate(CREATBL = convert_creat(CREAT))
#' df
convert_creat <- function(creat) {
  checkmate::assertNumeric(creat)

  if (any(is.na(creat))) {
    message("creat contains missing values")
  }

  creat_mgdl <- creat / 88.42 # MediCalc conversion factor
  return(creat_mgdl)
}
