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
#' df <- df %>%
#'   dplyr::group_by(ID) %>%
#'   dplyr::mutate(ALBBL = convert_alb(ALB))
#' df
convert_alb <- function(alb) {
  input_name <- deparse1(substitute(alb))
  alb <- assert_and_strip_units(alb, "g/L")

  checkmate::assertNumeric(alb)

  if (any(is.na(alb))) {
    message("alb contains missing values")
  }

  alb_gdl <- alb / 10
  alb_gdl <- units::set_units(alb_gdl, "g/dL", mode = "standard")
  log_audit_event(
    "unit", fn = "convert_alb", input = input_name, from = "g/L", to = "g/dL",
    transform = "convert", detail = "x0.1", n = sum(!is.na(alb))
  )
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
#' df <- df %>%
#'   dplyr::group_by(ID) %>%
#'   dplyr::mutate(BILIBL = convert_bili(BILI))
#' df
convert_bili <- function(bili) {
  input_name <- deparse1(substitute(bili))
  bili <- assert_and_strip_units(bili, "umol/L")

  checkmate::assertNumeric(bili)

  if (any(is.na(bili))) {
    message("bili contains missing values")
  }
  mol_weight_bili <- 584.673 # g/mol
  # convert umol/L to mg/dL
  # 1 umol/L * MW g/mol * mol / 10^6 umol * 10^3 mg /g * L / 10 dL
  conversion_factor <- mol_weight_bili / 10^4
  bili_mgdl <- bili * conversion_factor
  bili_mgdl <- units::set_units(bili_mgdl, "mg/dL", mode = "standard")
  log_audit_event(
    "unit", fn = "convert_bili", input = input_name, from = "umol/L", to = "mg/dL",
    transform = "convert", detail = paste0("x", signif(conversion_factor, 4)),
    n = sum(!is.na(bili))
  )
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
#' df <- df %>%
#'   dplyr::group_by(ID) %>%
#'   dplyr::mutate(CREATBL = convert_creat(CREAT))
#' df
convert_creat <- function(creat) {
  input_name <- deparse1(substitute(creat))
  creat <- assert_and_strip_units(creat, "umol/L")

  checkmate::assertNumeric(creat)

  if (any(is.na(creat))) {
    message("creat contains missing values")
  }
  mol_weight_creat <- 113.12 # g/mol
  # convert umol/L to mg/dL
  # 1 umol/L * MW g/mol * mol / 10^6 umol * 10^3 mg /g * L / 10 dL
  conversion_factor <- mol_weight_creat / 10^4
  creat_mgdl <- creat * conversion_factor
  creat_mgdl <- units::set_units(creat_mgdl, "mg/dL", mode = "standard")
  log_audit_event(
    "unit", fn = "convert_creat", input = input_name, from = "umol/L", to = "mg/dL",
    transform = "convert", detail = paste0("x", signif(conversion_factor, 4)),
    n = sum(!is.na(creat))
  )
  return(creat_mgdl)
}

#' Convert Mass to Molar Amounts or Concentrations
#'
#' Converts a mass quantity or mass concentration to the corresponding molar
#' quantity or concentration using a molecular weight. Works for plain amounts
#' (e.g. `mg` -> `umol`) and concentrations (e.g. `mg/dL` -> `umol/L`) alike;
#' the `units` machinery handles the volume dimension.
#'
#' @param values numeric or `units` vector interpreted in `mass_units`.
#' @param mass_units the mass unit of `values` (e.g. `"mg"`, `"mg/dL"`).
#' @param mol_units the target molar unit (e.g. `"umol"`, `"umol/L"`).
#' @param mol_weight molecular weight in g/mol.
#'
#' @return a `units` vector in `mol_units`.
#'
#' @family unit_conversion
#' @export
#'
#' @examples
#' convert_mass_to_mol(1, "mg/dL", "umol/L", mol_weight = 113.12) # creatinine
convert_mass_to_mol <- function(values, mass_units, mol_units, mol_weight) {
  input_name <- deparse1(substitute(values))
  checkmate::assert_string(mass_units)
  checkmate::assert_string(mol_units)
  checkmate::assert_number(mol_weight, lower = 0)

  mass <- units::set_units(values, mass_units, mode = "standard")
  mw <- units::set_units(mol_weight, "g/mol", mode = "standard")
  result <- units::set_units(mass / mw, mol_units, mode = "standard")

  log_audit_event(
    "unit", fn = "convert_mass_to_mol", input = input_name,
    from = mass_units, to = mol_units, transform = "convert",
    detail = paste0("MW=", mol_weight, " g/mol"),
    n = sum(!is.na(as.numeric(result)))
  )
  result
}

#' Convert Molar to Mass Amounts or Concentrations
#'
#' Converts a molar quantity or molar concentration to the corresponding mass
#' quantity or concentration using a molecular weight. Works for plain amounts
#' (e.g. `umol` -> `mg`) and concentrations (e.g. `umol/L` -> `mg/dL`) alike.
#'
#' @param values numeric or `units` vector interpreted in `mol_units`.
#' @param mass_units the target mass unit (e.g. `"mg"`, `"mg/dL"`).
#' @param mol_units the molar unit of `values` (e.g. `"umol"`, `"umol/L"`).
#' @param mol_weight molecular weight in g/mol.
#'
#' @return a `units` vector in `mass_units`.
#'
#' @family unit_conversion
#' @export
#'
#' @examples
#' convert_mol_to_mass(88.42, "mg/dL", "umol/L", mol_weight = 113.12) # creatinine
convert_mol_to_mass <- function(values, mass_units, mol_units, mol_weight) {
  input_name <- deparse1(substitute(values))
  checkmate::assert_string(mass_units)
  checkmate::assert_string(mol_units)
  checkmate::assert_number(mol_weight, lower = 0)

  mol <- units::set_units(values, mol_units, mode = "standard")
  mw <- units::set_units(mol_weight, "g/mol", mode = "standard")
  result <- units::set_units(mol * mw, mass_units, mode = "standard")

  log_audit_event(
    "unit", fn = "convert_mol_to_mass", input = input_name,
    from = mol_units, to = mass_units, transform = "convert",
    detail = paste0("MW=", mol_weight, " g/mol"),
    n = sum(!is.na(as.numeric(result)))
  )
  result
}
