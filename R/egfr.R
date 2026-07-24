#' Calculate Estimated Glomerular Filtration Rate
#'
#' @param sexf a boolean representing if the patient is female.
#' @param raceb a boolean representing if the patient is black.
#' @param age the age of a patient in years.
#' @param creat the serum creatinine levels in mg/dL.
#' @param ... reserved; must be empty. Arguments after `creat` must be named.
#' @param cystc the cystatin C levels in mg/L - required for the "CKDEPI 2021 cystatin" method.
#' @param height the height of a patient in cm - required for the "Schwartz" method.
#' @param method equation to use, one of:
#'   [egfr(method = "CKDEPI 2009")][.egfr_ckdepi_2009],
#'   [egfr(method = "MDRD")][.egfr_mdrd],
#'   [egfr(method = "CKDEPI 2021")][.egfr_ckdepi_2021],
#'   [egfr(method = "CKDEPI 2021 cystatin")][.egfr_ckdepi_2021_cystatin],
#'   [egfr(method = "Schwartz")][.egfr_schwartz].
#'
#' @return the eGFR calculated based on method.
#'
#' @seealso The method implementations:
#'   [egfr(method = "CKDEPI 2009")][.egfr_ckdepi_2009],
#'   [egfr(method = "MDRD")][.egfr_mdrd],
#'   [egfr(method = "CKDEPI 2021")][.egfr_ckdepi_2021],
#'   [egfr(method = "CKDEPI 2021 cystatin")][.egfr_ckdepi_2021_cystatin],
#'   [egfr(method = "Schwartz")][.egfr_schwartz].
#'
#' @family renal_function
#' @export
#'
#' @examples
#' e <- egfr(TRUE, TRUE, 24, 1, method = "CKDEPI 2009")
#'
#' df <- data.frame(
#'   "SEXF" = c(TRUE, FALSE, TRUE, FALSE),
#'   "RACEB" = c(FALSE, FALSE, TRUE, FALSE),
#'   "AGE" = c(24, 24, 23, 24),
#'   "CREAT" = c(1, 1, 2, 1)
#' )
#' df <- dplyr::mutate(df, egfr = egfr(SEXF, RACEB, AGE, CREAT, method = "CKDEPI 2009"))
#' df
egfr <- function(
  sexf,
  raceb,
  age,
  creat,
  ...,
  cystc = NULL,
  height = NULL,
  method = "CKDEPI 2021"
) {
  rlang::check_dots_empty()

  checkmate::assert_string(method)
  checkmate::assert_choice(
    tolower(method),
    c("ckdepi 2009", "mdrd", "ckdepi 2021 cystatin", "ckdepi 2021", "schwartz")
  )

  if (!missing(sexf)) checkmate::assert_logical(sexf)
  if (!missing(raceb)) checkmate::assert_logical(raceb)
  if (!missing(age)) checkmate::assert_numeric(age)
  if (!missing(creat)) checkmate::assert_numeric(creat)
  if (!is.null(cystc)) checkmate::assert_numeric(cystc)
  if (!is.null(height)) checkmate::assert_numeric(height)

  method_low <- tolower(method)

  if (method_low == "ckdepi 2009") {
    egfr <- .egfr_ckdepi_2009(sexf, raceb, age, creat)
  } else if (method_low == "mdrd") {
    egfr <- .egfr_mdrd(sexf, raceb, age, creat)
  } else if (method_low == "ckdepi 2021") {
    egfr <- .egfr_ckdepi_2021(sexf, age, creat)
  } else if (method_low == "ckdepi 2021 cystatin") {
    if (is.null(cystc)) {
      rlang::abort('`cystc` is required when `method = "CKDEPI 2021 cystatin"`')
    }
    egfr <- .egfr_ckdepi_2021_cystatin(sexf, age, creat, cystc)
  } else if (method_low == "schwartz") {
    if (is.null(height)) {
      rlang::abort('`height` is required when `method = "Schwartz"`')
    }
    egfr <- .egfr_schwartz(height, creat)
  }
  return(egfr)
}

#' CKD-EPI 2009 creatinine eGFR equation
#'
#' The equation used by [egfr()] with `method = "CKDEPI 2009"`.
#'
#' @usage NULL
#'
#' @details
#' The CKD-EPI 2009 equation:
#' \deqn{eGFR = 141 \cdot \min(S_{cr}/\kappa, 1)^\alpha \cdot \max(S_{cr}/\kappa, 1)^{-1.209} \cdot 0.993^A \cdot 1.018^F \cdot 1.159^B}
#'
#' where:
#' \itemize{
#'   \item \eqn{S_{cr}} = serum creatinine (mg/dL)
#'   \item \eqn{\kappa} = 0.7 (female) or 0.9 (male)
#'   \item \eqn{\alpha} = -0.329 (female) or -0.411 (male)
#'   \item \eqn{A} = age (years)
#'   \item \eqn{F} = 1 (female) or 0 (male)
#'   \item \eqn{B} = 1 (Black) or 0 (other)
#' }
#'
#' @keywords internal
.egfr_ckdepi_2009 <- function(sexf, raceb, age, creat) {
  age_input <- mask_missing_computation_input(age, "age")
  creat_input <- mask_missing_computation_input(creat, "creat")
  age <- age_input$value
  creat <- creat_input$value

  checkmate::assert_logical(sexf)
  checkmate::assert_logical(raceb)
  checkmate::assertNumeric(age)
  checkmate::assertNumeric(creat)

  input_lengths <- lengths(list(sexf, raceb, age, creat))
  if (length(unique(input_lengths)) != 1) {
    warning("Inputs have different lengths! Please check data.")
  }

  if (any(is.na(sexf))) {
    message("sexf contains missing values")
  }
  if (any(is.na(raceb))) {
    message("raceb contains missing values")
  }
  if (any(is.na(age))) {
    message("age contains missing values")
  }
  if (any(is.na(creat))) {
    message("creat contains missing values")
  }

  if (any(stats::na.omit(age) < 18)) {
    message(
      "Ages less than 18 years old detected. You might want to calculate eGFR with method = 'Schwartz' for these subjects"
    )
  }

  k <- dplyr::if_else(sexf, 0.7, 0.9)
  alpha <- dplyr::if_else(sexf, -0.329, -0.411)
  sex_mult <- dplyr::if_else(sexf, 1.018, 1)
  race_mult <- dplyr::if_else(raceb, 1.159, 1)

  ratio <- creat / k
  scr_k_min <- dplyr::if_else(ratio < 1, ratio^alpha, 1)
  scr_k_max <- dplyr::if_else(ratio > 1, ratio^-1.209, 1)

  egfr <- 141 *
    scr_k_min *
    scr_k_max *
    (0.993^age) *
    sex_mult *
    race_mult

  egfr <- apply_mv_mask(egfr, age_input$mask, creat_input$mask)
  attr(egfr, "scicalc_units") <- "mL/min/1.73m^2"
  egfr
}

#' CKD-EPI 2021 creatinine eGFR equation
#'
#' The equation used by [egfr()] with `method = "CKDEPI 2021"`.
#'
#' @usage NULL
#'
#' @details
#' The CKD-EPI 2021 creatinine equation (race-free):
#' \deqn{eGFR = 142 \cdot \min(S_{cr}/\kappa, 1)^\alpha \cdot \max(S_{cr}/\kappa, 1)^{-1.2} \cdot 0.9938^A \cdot 1.012^F}
#'
#' where:
#' \itemize{
#'   \item \eqn{S_{cr}} = serum creatinine (mg/dL)
#'   \item \eqn{\kappa} = 0.7 (female) or 0.9 (male)
#'   \item \eqn{\alpha} = -0.241 (female) or -0.302 (male)
#'   \item \eqn{A} = age (years)
#'   \item \eqn{F} = 1 (female) or 0 (male)
#' }
#'
#' @keywords internal
.egfr_ckdepi_2021 <- function(sexf, age, creat) {
  age_input <- mask_missing_computation_input(age, "age")
  creat_input <- mask_missing_computation_input(creat, "creat")
  age <- age_input$value
  creat <- creat_input$value

  checkmate::assert_logical(sexf)
  checkmate::assertNumeric(age)
  checkmate::assertNumeric(creat)

  input_lengths <- lengths(list(sexf, age, creat))
  if (length(unique(input_lengths)) != 1) {
    warning("Inputs have different lengths! Please check data.")
  }

  if (any(is.na(sexf))) {
    message("sexf contains missing values")
  }
  if (any(is.na(age))) {
    message("age contains missing values")
  }
  if (any(is.na(creat))) {
    message("creat contains missing values")
  }

  if (any(stats::na.omit(age) < 18)) {
    message(
      "Ages less than 18 years old detected. You might want to calculate eGFR with method = 'Schwartz' for these subjects"
    )
  }

  k <- dplyr::if_else(sexf, 0.7, 0.9)
  alpha <- dplyr::if_else(sexf, -0.241, -0.302)
  sex_mult <- dplyr::if_else(sexf, 1.012, 1)

  ratio <- creat / k
  scr_k_min <- dplyr::if_else(ratio < 1, ratio^alpha, 1)
  scr_k_max <- dplyr::if_else(ratio > 1, ratio^-1.200, 1)

  egfr <- 142 *
    scr_k_min *
    scr_k_max *
    (0.9938^age) *
    sex_mult

  egfr <- apply_mv_mask(egfr, age_input$mask, creat_input$mask)
  attr(egfr, "scicalc_units") <- "mL/min/1.73m^2"
  egfr
}

#' CKD-EPI 2021 creatinine-cystatin C eGFR equation
#'
#' The equation used by [egfr()] with `method = "CKDEPI 2021 cystatin"`.
#'
#' @usage NULL
#'
#' @details
#' The CKD-EPI 2021 creatinine-cystatin equation:
#' \deqn{eGFR = 135 \cdot \min(S_{cr}/\kappa, 1)^\alpha \cdot \max(S_{cr}/\kappa, 1)^{-0.544} \cdot \min(S_{cys}/0.8, 1)^{-0.323} \\
#' \cdot \max(S_{cys}/0.8, 1)^{-0.778} \cdot 0.9961^A \cdot 0.963^F}
#'
#' where:
#' \itemize{
#'   \item \eqn{S_{cr}} = serum creatinine (mg/dL)
#'   \item \eqn{S_{cys}} = serum cystatin C (mg/L)
#'   \item \eqn{\kappa} = 0.7 (female) or 0.9 (male)
#'   \item \eqn{\alpha} = -0.219 (female) or -0.144 (male)
#'   \item \eqn{A} = age (years)
#'   \item \eqn{F} = 1 (female) or 0 (male)
#' }
#'
#' @keywords internal
.egfr_ckdepi_2021_cystatin <- function(sexf, age, creat, cystc) {
  age_input <- mask_missing_computation_input(age, "age")
  creat_input <- mask_missing_computation_input(creat, "creat")
  cystc_input <- mask_missing_computation_input(cystc, "cystc")
  age <- age_input$value
  creat <- creat_input$value
  cystc <- cystc_input$value

  checkmate::assert_logical(sexf)
  checkmate::assertNumeric(age)
  checkmate::assertNumeric(creat)
  checkmate::assertNumeric(cystc)

  input_lengths <- lengths(list(sexf, age, creat, cystc))
  if (length(unique(input_lengths)) != 1) {
    warning("Inputs have different lengths! Please check data.")
  }

  if (any(is.na(sexf))) {
    message("sexf contains missing values")
  }
  if (any(is.na(age))) {
    message("age contains missing values")
  }
  if (any(is.na(creat))) {
    message("creat contains missing values")
  }
  if (any(is.na(cystc))) {
    message("cystc contains missing values")
  }

  if (any(stats::na.omit(age) < 18)) {
    message(
      "Ages less than 18 years old detected. You might want to calculate eGFR with method = 'Schwartz' for these subjects"
    )
  }

  k <- dplyr::if_else(sexf, 0.7, 0.9)
  alpha <- dplyr::if_else(sexf, -0.219, -0.144)
  sex_mult <- dplyr::if_else(sexf, 0.963, 1)

  ratio <- creat / k
  cys_ratio <- cystc / 0.8

  scr_k_min <- dplyr::if_else(ratio < 1, ratio^alpha, 1)
  scr_k_max <- dplyr::if_else(ratio > 1, ratio^-0.544, 1)
  scys_k_min <- dplyr::if_else(cys_ratio < 1, cys_ratio^-0.323, 1)
  scys_k_max <- dplyr::if_else(cys_ratio > 1, cys_ratio^-0.778, 1)

  egfr <- 135 *
    scr_k_min *
    scr_k_max *
    scys_k_min *
    scys_k_max *
    (0.9961^age) *
    sex_mult

  egfr <- apply_mv_mask(egfr, age_input$mask, creat_input$mask, cystc_input$mask)
  attr(egfr, "scicalc_units") <- "mL/min/1.73m^2"
  egfr
}

#' MDRD (4-variable) eGFR equation
#'
#' The equation used by [egfr()] with `method = "MDRD"`.
#'
#' @usage NULL
#'
#' @details
#' The MDRD equation:
#' \deqn{eGFR = 175 \cdot S_{cr}^{-1.154} \cdot A^{-0.203} \cdot 0.742^F \cdot 1.212^B}{eGFR = 175 * Scr^-1.154 * A^-0.203 * 0.742^F * 1.212^B}
#'
#' where:
#' \itemize{
#'   \item \eqn{S_{cr}} = serum creatinine (mg/dL)
#'   \item \eqn{A} = age (years)
#'   \item \eqn{F} = 1 (female) or 0 (male)
#'   \item \eqn{B} = 1 (Black) or 0 (other)
#' }
#'
#' @keywords internal
.egfr_mdrd <- function(sexf, raceb, age, creat) {
  age_input <- mask_missing_computation_input(age, "age")
  creat_input <- mask_missing_computation_input(creat, "creat")
  age <- age_input$value
  creat <- creat_input$value

  checkmate::assert_logical(sexf)
  checkmate::assert_logical(raceb)
  checkmate::assertNumeric(age)
  checkmate::assertNumeric(creat)

  input_lengths <- lengths(list(sexf, raceb, age, creat))
  if (length(unique(input_lengths)) != 1) {
    warning("Inputs have different lengths! Please check data.")
  }

  if (any(is.na(sexf))) {
    message("sexf contains missing values")
  }
  if (any(is.na(raceb))) {
    message("raceb contains missing values")
  }
  if (any(is.na(age))) {
    message("age contains missing values")
  }
  if (any(is.na(creat))) {
    message("creat contains missing values")
  }

  if (any(stats::na.omit(age) < 18)) {
    message(
      "Ages less than 18 years old detected. You might want to calculate eGFR with method = 'Schwartz' for these subjects"
    )
  }

  sex_mult <- dplyr::if_else(sexf, 0.742, 1)
  race_mult <- dplyr::if_else(raceb, 1.212, 1)

  egfr <- 175 *
    creat^-1.154 *
    age^-0.203 *
    sex_mult *
    race_mult

  egfr <- apply_mv_mask(egfr, age_input$mask, creat_input$mask)
  attr(egfr, "scicalc_units") <- "mL/min/1.73m^2"
  egfr
}

#' Bedside Schwartz eGFR equation
#'
#' The equation used by [egfr()] with `method = "Schwartz"`.
#'
#' @usage NULL
#'
#' @details
#' The Schwartz equation for pediatric eGFR:
#' \deqn{eGFR = 0.413 \cdot \frac{H}{S_{cr}}}{eGFR = 0.413 * H / Scr}
#'
#' where:
#' \itemize{
#'   \item \eqn{H} = height (cm)
#'   \item \eqn{S_{cr}} = serum creatinine (mg/dL)
#' }
#'
#' @keywords internal
.egfr_schwartz <- function(height, creat) {
  height_input <- mask_missing_computation_input(height, "height")
  creat_input <- mask_missing_computation_input(creat, "creat")
  height <- height_input$value
  creat <- creat_input$value

  checkmate::assertNumeric(height)
  checkmate::assertNumeric(creat)

  input_lengths <- lengths(list(height, creat))
  if (length(unique(input_lengths)) != 1) {
    warning("Inputs have different lengths! Please check data.")
  }

  if (any(is.na(height))) {
    message("height contains missing values")
  }
  if (any(is.na(creat))) {
    message("creat contains missing values")
  }

  egfr <- 0.413 * height / creat

  egfr <- apply_mv_mask(egfr, height_input$mask, creat_input$mask)
  attr(egfr, "scicalc_units") <- "mL/min/1.73m^2"
  egfr
}

#' Calculate eGFR Using CKD-EPI 2009 Equation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated; use [egfr()] with `method = "CKDEPI 2009"`.
#'
#' @param sexf boolean value of sex Female: TRUE, Male: FALSE
#' @param raceb boolean value of Race == Black: Black: TRUE, Other: FALSE
#' @param age age of subject (years)
#' @param creat creatinine levels of subject (mg/dL)
#'
#' @details
#' The CKD-EPI 2009 equation:
#' \deqn{eGFR = 141 \cdot \min(S_{cr}/\kappa, 1)^\alpha \cdot \max(S_{cr}/\kappa, 1)^{-1.209} \cdot 0.993^A \cdot 1.018^F \cdot 1.159^B}
#'
#' where:
#' \itemize{
#'   \item \eqn{S_{cr}} = serum creatinine (mg/dL)
#'   \item \eqn{\kappa} = 0.7 (female) or 0.9 (male)
#'   \item \eqn{\alpha} = -0.329 (female) or -0.411 (male)
#'   \item \eqn{A} = age (years)
#'   \item \eqn{F} = 1 (female) or 0 (male)
#'   \item \eqn{B} = 1 (Black) or 0 (other)
#' }
#'
#' @return the eGFR value (mL/min/1.73m2)
#'
#' @keywords internal
#' @export
#'
#' @examples
#' e <- egfr(TRUE, TRUE, 24, 1, method = "CKDEPI 2009")
ckdepi_2009_egfr <- function(sexf, raceb, age, creat) {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "ckdepi_2009_egfr()",
    with = 'egfr(method = "CKDEPI 2009")'
  )
  .egfr_ckdepi_2009(sexf, raceb, age, creat)
}

#' Calculate eGFR Using CKD-EPI 2021 Creatinine Equation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated; use [egfr()] with `method = "CKDEPI 2021"`.
#'
#' @param sexf boolean value of sex Female: TRUE, Male: FALSE
#' @param age age of subject (years)
#' @param creat creatinine levels of subject (mg/dL)
#'
#' @details
#' The CKD-EPI 2021 creatinine equation (race-free):
#' \deqn{eGFR = 142 \cdot \min(S_{cr}/\kappa, 1)^\alpha \cdot \max(S_{cr}/\kappa, 1)^{-1.2} \cdot 0.9938^A \cdot 1.012^F}
#'
#' where:
#' \itemize{
#'   \item \eqn{S_{cr}} = serum creatinine (mg/dL)
#'   \item \eqn{\kappa} = 0.7 (female) or 0.9 (male)
#'   \item \eqn{\alpha} = -0.241 (female) or -0.302 (male)
#'   \item \eqn{A} = age (years)
#'   \item \eqn{F} = 1 (female) or 0 (male)
#' }
#'
#' @return the eGFR value (mL/min/1.73m2)
#'
#' @keywords internal
#' @export
#'
#' @examples
#' e <- egfr(TRUE, FALSE, 24, 1, method = "CKDEPI 2021")
ckdepi_2021_egfr <- function(sexf, age, creat) {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "ckdepi_2021_egfr()",
    with = 'egfr(method = "CKDEPI 2021")'
  )
  .egfr_ckdepi_2021(sexf, age, creat)
}

#' Calculate eGFR Using CKD-EPI 2021 Cystatin Equation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated; use [egfr()] with `method = "CKDEPI 2021 cystatin"`.
#'
#' @param sexf a boolean representing if the patient is female.
#' @param age age of patient in years
#' @param creat serum creatinine levels in mg/dL.
#' @param cystc serum cystatin C levels in mg/L.
#'
#' @details
#' The CKD-EPI 2021 creatinine-cystatin equation:
#' \deqn{eGFR = 135 \cdot \min(S_{cr}/\kappa, 1)^\alpha \cdot \max(S_{cr}/\kappa, 1)^{-0.544} \cdot \min(S_{cys}/0.8, 1)^{-0.323} \\
#' \cdot \max(S_{cys}/0.8, 1)^{-0.778} \cdot 0.9961^A \cdot 0.963^F}
#'
#' where:
#' \itemize{
#'   \item \eqn{S_{cr}} = serum creatinine (mg/dL)
#'   \item \eqn{S_{cys}} = serum cystatin C (mg/L)
#'   \item \eqn{\kappa} = 0.7 (female) or 0.9 (male)
#'   \item \eqn{\alpha} = -0.219 (female) or -0.144 (male)
#'   \item \eqn{A} = age (years)
#'   \item \eqn{F} = 1 (female) or 0 (male)
#' }
#'
#' @return eGFR in mL/min/1.73 m^2
#'
#' @keywords internal
#' @export
#'
#' @examples
#' e <- egfr(TRUE, FALSE, 24, 1, cystc = 2, method = "CKDEPI 2021 cystatin")
ckdepi_2021_egfr_cystatin <- function(sexf, age, creat, cystc) {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "ckdepi_2021_egfr_cystatin()",
    with = 'egfr(method = "CKDEPI 2021 cystatin")'
  )
  .egfr_ckdepi_2021_cystatin(sexf, age, creat, cystc)
}

#' Calculate eGFR Using MDRD Equation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated; use [egfr()] with `method = "MDRD"`.
#'
#' @param sexf a boolean representing if the patient is female.
#' @param raceb a boolean representing if the patient is black.
#' @param age the age of the patient in years
#' @param creat the serum creatinine levels in mg/dL
#'
#' @details
#' The MDRD equation:
#' \deqn{eGFR = 175 \cdot S_{cr}^{-1.154} \cdot A^{-0.203} \cdot 0.742^F \cdot 1.212^B}{eGFR = 175 * Scr^-1.154 * A^-0.203 * 0.742^F * 1.212^B}
#'
#' where:
#' \itemize{
#'   \item \eqn{S_{cr}} = serum creatinine (mg/dL)
#'   \item \eqn{A} = age (years)
#'   \item \eqn{F} = 1 (female) or 0 (male)
#'   \item \eqn{B} = 1 (Black) or 0 (other)
#' }
#'
#' @return the eGFR in mL/min/1.73 m^2
#'
#' @keywords internal
#' @export
#'
#' @examples
#' e <- egfr(TRUE, TRUE, 24, 1, method = "MDRD")
mdrd_egfr <- function(sexf, raceb, age, creat) {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "mdrd_egfr()",
    with = 'egfr(method = "MDRD")'
  )
  .egfr_mdrd(sexf, raceb, age, creat)
}

#' Calculate eGFR Using Schwartz Equation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated; use [egfr()] with `method = "Schwartz"`.
#'
#' @param height height of patients in cm.
#' @param creat Serum creatinine levels in mg/dL
#'
#' @details
#' The Schwartz equation for pediatric eGFR:
#' \deqn{eGFR = 0.413 \cdot \frac{H}{S_{cr}}}{eGFR = 0.413 * H / Scr}
#'
#' where:
#' \itemize{
#'   \item \eqn{H} = height (cm)
#'   \item \eqn{S_{cr}} = serum creatinine (mg/dL)
#' }
#'
#' @return eGFR in mL/min/1.73m^2
#'
#' @keywords internal
#' @export
#'
#' @examples
#' egfr(FALSE, FALSE, 10, 1, height = 100, method = "Schwartz")
schwartz_egfr <- function(height, creat) {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "schwartz_egfr()",
    with = 'egfr(method = "Schwartz")'
  )
  .egfr_schwartz(height, creat)
}
