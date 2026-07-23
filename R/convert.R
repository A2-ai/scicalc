#' Convert Albumin Concentration Units
#'
#' @param alb albumin concentration (g/L)
#'
#' @return Albumin concentration (g/dL)
#'
#' @family unit_conversion
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
#' df <- df |>
#'   dplyr::group_by(ID) |>
#'   dplyr::mutate(ALBBL = convert_alb(ALB))
#' df
convert_alb <- function(alb) {
  alb_input <- mask_missing_computation_input(alb, "alb")
  alb <- alb_input$value
  checkmate::assertNumeric(alb)

  if (any(is.na(alb))) {
    message("alb contains missing values")
  }

  alb_gdl <- alb / 10
  alb_gdl <- apply_mv_mask(alb_gdl, alb_input$mask)
  attr(alb_gdl, "scicalc_units") <- "g/dL"
  return(alb_gdl)
}

#' Convert Bilirubin Concentration Units
#'
#' @param bili bilirubin concentration (µmol/L)
#'
#' @return Bilirubin concentration (mg/dL)
#'
#' @family unit_conversion
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
#' df <- df |>
#'   dplyr::group_by(ID) |>
#'   dplyr::mutate(BILIBL = convert_bili(BILI))
#' df
convert_bili <- function(bili) {
  bili_input <- mask_missing_computation_input(bili, "bili")
  bili <- bili_input$value
  checkmate::assertNumeric(bili)

  if (any(is.na(bili))) {
    message("bili contains missing values")
  }
  mol_weight_bili <- 584.673 # g/mol
  # convert umol/L to mg/dL
  # 1 umol/L * MW g/mol * mol / 10^6 umol * 10^3 mg /g * L / 10 dL
  conversion_factor <- mol_weight_bili / 10^4
  bili_mgdl <- bili * conversion_factor
  bili_mgdl <- apply_mv_mask(bili_mgdl, bili_input$mask)
  attr(bili_mgdl, "scicalc_units") <- "mg/dL"
  return(bili_mgdl)
}

#' Convert Serum Creatinine Concentration Units
#'
#' @param creat serum creatinine concentration (µmol/L)
#'
#' @return Serum Creatinine concentration (mg/dL)
#'
#' @family unit_conversion
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
#' df <- df |>
#'   dplyr::group_by(ID) |>
#'   dplyr::mutate(CREATBL = convert_creat(CREAT))
#' df
convert_creat <- function(creat) {
  creat_input <- mask_missing_computation_input(creat, "creat")
  creat <- creat_input$value
  checkmate::assertNumeric(creat)

  if (any(is.na(creat))) {
    message("creat contains missing values")
  }
  mol_weight_creat <- 113.12 # g/mol
  # convert umol/L to mg/dL
  # 1 umol/L * MW g/mol * mol / 10^6 umol * 10^3 mg /g * L / 10 dL
  conversion_factor <- mol_weight_creat / 10^4
  creat_mgdl <- creat * conversion_factor
  creat_mgdl <- apply_mv_mask(creat_mgdl, creat_input$mask)
  attr(creat_mgdl, "scicalc_units") <- "mg/dL"
  return(creat_mgdl)
}
