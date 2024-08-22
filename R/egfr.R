#' Calculates eGFR based on the method specified
#'
#' @param sexf a boolean representing if the patient is female.
#' @param raceb a boolean representing if the patient is black.
#' @param age the age of a patient in years.
#' @param creat the serum creatinine levels in mg/dL.
#' @param cystc the cystain C levels in mg/L - only used in CKDEPI 2021 method
#' @param height the height of a patient in cm.
#' @param method a string specifying the method to use. Available options are "CKDEPI 2009", "MDRD", "CKDEPI 2021", "Schwartz".
#'
#' @return the eGFR calculated based on method.
#' @export
#'
#' @examples
#' e <- egfr(TRUE, TRUE, 24, 1, "CKDEPI 2009")
#'
#' df <- data.frame(
#'    "SEXF" = c(TRUE, FALSE, TRUE, FALSE),
#'    "RACEB" = c(FALSE, FALSE, TRUE, FALSE),
#'    "AGE" = c(24, 24, 23, 24),
#'    "CREAT" = c(1, 1, 2, 1)
#'    )
#' df <- dplyr::mutate(df, egfr = egfr(SEXF, RACEB, AGE, CREAT, "CKDEPI 2009"))
egfr <- function(sexf, raceb, age, creat, cystc, height, method = "CKDEPI 2009") {
  checkmate::assert_choice(tolower(method), c("ckdepi 2009", "mdrd", "ckdepi 2021", "schwartz"))

  method_low = tolower(method)

  if (method_low == "ckdepi 2009") {
    egfr <- ckdepi_2009_egfr(sexf, raceb, age, creat)
  } else if (method_low == "mdrd") {
    egfr <- mdrd_egfr(sexf, raceb, age, creat)
  } else if (method_low == "ckdepi 2021") {
    egfr <- ckdepi_2021_egfr(sexf, age, creat, cystc)
  } else if (method_low == "schwartz") {
    egfr <- schwartz_egfr(height, creat)
  }
  return(egfr)
}
#' Calculates Estimated Glomerular Filtration Rate based on Sex, Race, Age, and Creatinine levels
#' based on the CKDEPI 2009 equation
#' @param sexf boolean value of sex Female: TRUE, Male: FALSE
#' @param raceb boolean value of Race == Black: Black: TRUE, Other: FALSE
#' @param age age of subject (years)
#' @param creat creatinine levels of subject (mg/dL)
#'
#' @return the eGFR value (mL/min/1.73m2)
#' @export
#'
#' @examples
#' e <- ckdepi_2009_egfr(TRUE, TRUE, 24, 1)
#'
#' df <- data.frame(
#'    "SEXF" = c(TRUE, FALSE, TRUE, FALSE),
#'    "RACEB" = c(FALSE, FALSE, TRUE, FALSE),
#'    "AGE" = c(24, 24, 23, 24),
#'    "CREAT" = c(1, 1, 2, 1)
#'    )
#' df <- dplyr::mutate(df, egfr = ckdepi_2009_egfr(SEXF, RACEB, AGE, CREAT))
ckdepi_2009_egfr <- function(sexf, raceb, age, creat) {
  checkmate::assert_logical(sexf)
  checkmate::assert_logical(raceb)
  checkmate::assertNumeric(age)
  checkmate::assertNumeric(creat)

  if (any(is.na(sexf))) {
    message('sexf contains missing values')
  }
  if (any(is.na(raceb))) {
    message('raceb contains missing values')
  }
  if (any(is.na(age))) {
    message('age contains missing values')
  }
  if (any(is.na(creat))) {
    message('creat contains missing values')
  }

  if (any(stats::na.omit(age) < 18)) {
    message("Ages less than 18 years old detected. You might want to calculate eGFR with method = 'Schwartz' for these subjects")
  }

  K <- dplyr::if_else(sexf, 0.7, 0.9)
  alpha <- dplyr::if_else(sexf, -0.329, -0.411)
  sex_mult <- dplyr::if_else(sexf, 1.018, 1)
  race_mult <- dplyr::if_else(raceb, 1.159, 1)

  ratio <- creat/K
  scr_k_min <- dplyr::if_else(ratio < 1, ratio^alpha, 1)
  scr_k_max <- dplyr::if_else(ratio > 1, ratio^-1.209, 1)

  egfr <- 141 *
    scr_k_min *
    scr_k_max *
    (0.993^age) *
    sex_mult *
    race_mult

  return (egfr)
}

#' Calculates eGFR with CKDEPI 2021 equation
#'
#' @param sexf a boolean representing if the patient is female.
#' @param age age of patient in years
#' @param creat serum creatinine levels in mg/dL.
#' @param cystc serum cystain C levels in mg/L.
#'
#' @return eGFR in mL/min/1.73 m^2
#' @export
#'
#' @examples
#' e <- ckdepi_2021_egfr(TRUE, 24, 1, 2)
#'
#' df <- data.frame(
#'    "SEXF" = c(TRUE, FALSE, TRUE, FALSE),
#'    "RACEB" = c(FALSE, FALSE, TRUE, FALSE),
#'    "AGE" = c(24, 24, 23, 24),
#'    "CREAT" = c(1, 1, 2, 1),
#'    "CYSTC" = c(0.4, 0.8, 1, 2)
#'    )
#' df <- dplyr::mutate(df, egfr = ckdepi_2021_egfr(SEXF, AGE, CREAT, CYSTC))
ckdepi_2021_egfr <- function(sexf, age, creat, cystc) {
  checkmate::assert_logical(sexf)
  checkmate::assertNumeric(age)
  checkmate::assertNumeric(creat)
  checkmate::assertNumeric(cystc)

  if (any(is.na(sexf))) {
    message('sexf contains missing values')
  }
  if (any(is.na(age))) {
    message('age contains missing values')
  }
  if (any(is.na(creat))) {
    message('creat contains missing values')
  }
  if (any(is.na(cystc))) {
    message('cystc contains missing values')
  }

  if (any(stats::na.omit(age) < 18)) {
    message("Ages less than 18 years old detected. You might want to calculate eGFR with method = 'Schwartz' for these subjects")
  }

  K <- dplyr::if_else(sexf, 0.7, 0.9)
  alpha <- dplyr::if_else(sexf, -0.219, -0.144)
  sex_mult <- dplyr::if_else(sexf, 0.963, 1)

  ratio <- creat/K
  cys_ratio <- cystc/0.8

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

  return (egfr)
}

#' Modification of Diet in Renal Disease eGFR calculation
#'
#' @param sexf a boolean representing if the patient is female.
#' @param raceb a boolean representing if the patient is black.
#' @param age the age of the patient in years
#' @param creat the serum creatinine levels in mg/dL
#'
#' @return the eGFR in mL/min/1.73 m^2
#' @export
#'
#' @examples
#' e <- mdrd_egfr(TRUE, TRUE, 24, 1)
#'
#' df <- data.frame(
#'    "SEXF" = c(TRUE, FALSE, TRUE, FALSE),
#'    "RACEB" = c(FALSE, FALSE, TRUE, FALSE),
#'    "AGE" = c(24, 24, 23, 24),
#'    "CREAT" = c(1, 1, 2, 1)
#'    )
#' df <- dplyr::mutate(df, egfr = mdrd_egfr(SEXF, RACEB, AGE, CREAT))
mdrd_egfr <- function(sexf, raceb, age, creat) {
  checkmate::assert_logical(sexf)
  checkmate::assert_logical(raceb)
  checkmate::assertNumeric(age)
  checkmate::assertNumeric(creat)


  if (any(is.na(sexf))) {
    message('sexf contains missing values')
  }
  if (any(is.na(raceb))) {
    message('raceb contains missing values')
  }
  if (any(is.na(age))) {
    message('age contains missing values')
  }
  if (any(is.na(creat))) {
    message('creat contains missing values')
  }

  if (any(stats::na.omit(age) < 18)) {
    message("Ages less than 18 years old detected. You might want to calculate eGFR with method = 'Schwartz' for these subjects")
  }

  sex_mult <- dplyr::if_else(sexf, 0.742, 1)
  race_mult <- dplyr::if_else(raceb, 1.212, 1)

  egfr <- 175 *
    creat ^ -1.154 *
    age ^ -0.203 *
    sex_mult *
    race_mult

  return(egfr)
}

#' Calculates eGFR based on Schwartz' equation
#'
#' @param height height of patients in cm.
#' @param creat Serum creatinine levels in mg/dL
#'
#' @return eGFR in mL/min/1.73m^2
#' @export
#'
#' @examples
#' schwartz_egfr(100, 1)
schwartz_egfr <- function(height, creat) {
  checkmate::assertNumeric(height)
  checkmate::assertNumeric(creat)

  if (any(is.na(height))) {
    message("height contains missing values")
  }
  if (any(is.na(creat))) {
    message("creat contains missing values")
  }

  egfr <- 0.413 * height / creat
  return(egfr)
}
