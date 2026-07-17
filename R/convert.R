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
  input_name <- deparse1(substitute(alb))
  alb <- assert_and_strip_units(alb, "g/L")
  alb_input <- mask_missing_computation_input(alb, "alb")
  alb <- alb_input$value

  checkmate::assertNumeric(alb)

  if (any(is.na(alb))) {
    message("alb contains missing values")
  }

  alb_gdl <- alb / 10
  alb_gdl <- units::set_units(alb_gdl, "g/dL", mode = "standard")
  alb_gdl <- apply_mv_mask(alb_gdl, alb_input$mask)
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
#' df <- df |>
#'   dplyr::group_by(ID) |>
#'   dplyr::mutate(BILIBL = convert_bili(BILI))
#' df
convert_bili <- function(bili) {
  input_name <- deparse1(substitute(bili))
  bili <- assert_and_strip_units(bili, "umol/L")
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
  bili_mgdl <- units::set_units(bili_mgdl, "mg/dL", mode = "standard")
  bili_mgdl <- apply_mv_mask(bili_mgdl, bili_input$mask)
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
#' df <- df |>
#'   dplyr::group_by(ID) |>
#'   dplyr::mutate(CREATBL = convert_creat(CREAT))
#' df
convert_creat <- function(creat) {
  input_name <- deparse1(substitute(creat))
  creat <- assert_and_strip_units(creat, "umol/L")
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
  creat_mgdl <- units::set_units(creat_mgdl, "mg/dL", mode = "standard")
  creat_mgdl <- apply_mv_mask(creat_mgdl, creat_input$mask)
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
#' @param x A numeric, `units`, or `mixed_units` vector.
#' @param mol_weight Molecular weight as a numeric or `units` vector. Numeric
#'   values are assumed to be in g/mol with a warning.
#' @param mol_units Optional target molar unit.
#' @param ... Arguments passed to a class method.
#' @param mass_units Source mass unit required by the numeric method.
#'
#' @return A `units` or `mixed_units` vector.
#'
#' @family unit_conversion
#' @export
#'
#' @examples
#' mass <- units::set_units(1, "mg/dL", mode = "standard")
#' mw <- units::set_units(113.12, "g/mol", mode = "standard")
#' convert_mass_to_mol(mass, mw, mol_units = "umol/L")
convert_mass_to_mol <- function(x, mol_weight, mol_units = NULL, ...) {
  UseMethod("convert_mass_to_mol")
}

#' @rdname convert_mass_to_mol
#' @export
convert_mass_to_mol.numeric <- function(
  x, mol_weight, mol_units = NULL, mass_units, ...
) {
  rlang::check_dots_empty()
  if (missing(mass_units)) {
    rlang::abort("`mass_units` is required for numeric `x`.")
  }
  checkmate::assert_string(mass_units)
  input_name <- deparse1(substitute(x))
  x_input <- mask_missing_computation_input(x, "x")
  x <- x_input$value
  x <- units::set_units(x, normalize_unit_string(mass_units), mode = "standard")
  convert_with_molecular_weight(
    x, mol_weight, mol_units, "/", "convert_mass_to_mol", input_name,
    input_mask = x_input$mask
  )
}

#' @rdname convert_mass_to_mol
#' @export
convert_mass_to_mol.units <- function(x, mol_weight, mol_units = NULL, ...) {
  rlang::check_dots_empty()
  x_input <- mask_missing_computation_input(x, "x")
  x <- x_input$value
  convert_with_molecular_weight(
    x, mol_weight, mol_units, "/", "convert_mass_to_mol",
    deparse1(substitute(x)), input_mask = x_input$mask
  )
}

#' @rdname convert_mass_to_mol
#' @export
convert_mass_to_mol.mixed_units <- function(x, mol_weight, mol_units = NULL, ...) {
  rlang::check_dots_empty()
  x_input <- mask_missing_computation_input(x, "x")
  x <- x_input$value
  convert_with_molecular_weight(
    x, mol_weight, mol_units, "/", "convert_mass_to_mol",
    deparse1(substitute(x)), input_mask = x_input$mask
  )
}

#' @rdname convert_mass_to_mol
#' @export
convert_mass_to_mol.default <- function(x, mol_weight, mol_units = NULL, ...) {
  rlang::abort(paste0(
    "No `convert_mass_to_mol()` method for class <",
    paste(class(x), collapse = "/"), ">."
  ))
}

#' Convert Molar to Mass Amounts or Concentrations
#'
#' Converts a molar quantity or molar concentration to the corresponding mass
#' quantity or concentration using a molecular weight. Works for plain amounts
#' (e.g. `umol` -> `mg`) and concentrations (e.g. `umol/L` -> `mg/dL`) alike.
#'
#' @param x A numeric, `units`, or `mixed_units` vector.
#' @param mol_weight Molecular weight as a numeric or `units` vector. Numeric
#'   values are assumed to be in g/mol with a warning.
#' @param mass_units Optional target mass unit.
#' @param ... Arguments passed to a class method.
#' @param mol_units Source molar unit required by the numeric method.
#'
#' @return A `units` or `mixed_units` vector.
#'
#' @family unit_conversion
#' @export
#'
#' @examples
#' mol <- units::set_units(88.4017, "umol/L", mode = "standard")
#' mw <- units::set_units(113.12, "g/mol", mode = "standard")
#' convert_mol_to_mass(mol, mw, mass_units = "mg/dL")
convert_mol_to_mass <- function(x, mol_weight, mass_units = NULL, ...) {
  UseMethod("convert_mol_to_mass")
}

#' @rdname convert_mol_to_mass
#' @export
convert_mol_to_mass.numeric <- function(
  x, mol_weight, mass_units = NULL, mol_units, ...
) {
  rlang::check_dots_empty()
  if (missing(mol_units)) {
    rlang::abort("`mol_units` is required for numeric `x`.")
  }
  checkmate::assert_string(mol_units)
  input_name <- deparse1(substitute(x))
  x_input <- mask_missing_computation_input(x, "x")
  x <- x_input$value
  x <- units::set_units(x, normalize_unit_string(mol_units), mode = "standard")
  convert_with_molecular_weight(
    x, mol_weight, mass_units, "*", "convert_mol_to_mass", input_name,
    input_mask = x_input$mask
  )
}

#' @rdname convert_mol_to_mass
#' @export
convert_mol_to_mass.units <- function(x, mol_weight, mass_units = NULL, ...) {
  rlang::check_dots_empty()
  x_input <- mask_missing_computation_input(x, "x")
  x <- x_input$value
  convert_with_molecular_weight(
    x, mol_weight, mass_units, "*", "convert_mol_to_mass",
    deparse1(substitute(x)), input_mask = x_input$mask
  )
}

#' @rdname convert_mol_to_mass
#' @export
convert_mol_to_mass.mixed_units <- function(x, mol_weight, mass_units = NULL, ...) {
  rlang::check_dots_empty()
  x_input <- mask_missing_computation_input(x, "x")
  x <- x_input$value
  convert_with_molecular_weight(
    x, mol_weight, mass_units, "*", "convert_mol_to_mass",
    deparse1(substitute(x)), input_mask = x_input$mask
  )
}

#' @rdname convert_mol_to_mass
#' @export
convert_mol_to_mass.default <- function(x, mol_weight, mass_units = NULL, ...) {
  rlang::abort(paste0(
    "No `convert_mol_to_mass()` method for class <",
    paste(class(x), collapse = "/"), ">."
  ))
}

# Prepare molecular weight in g/mol.
#' @noRd
prepare_molecular_weight <- function(mol_weight, n) {
  if (inherits(mol_weight, "mixed_units")) {
    rlang::abort("`mol_weight` must be numeric or a standard `units` vector.")
  }
  if (inherits(mol_weight, "units")) {
    mw <- units::set_units(mol_weight, "g/mol", mode = "standard")
  } else if (is.numeric(mol_weight)) {
    rlang::warn(
      "Numeric `mol_weight` has no units; assuming [g/mol].",
      class = "scicalc_assumed_molecular_weight_units"
    )
    mw <- units::set_units(mol_weight, "g/mol", mode = "standard")
  } else {
    rlang::abort("`mol_weight` must be numeric or inherit from `units`.")
  }
  if (!length(mw) %in% c(1L, n)) {
    rlang::abort("`mol_weight` must have length 1 or the same length as `x`.")
  }
  if (length(mw) == 1L) {
    mw <- rep(mw, length.out = n)
  }
  if (any(!is.na(mw) & (!is.finite(as.numeric(mw)) | as.numeric(mw) <= 0))) {
    rlang::abort("Non-missing `mol_weight` values must be finite and greater than zero.")
  }
  mw
}

# Apply molecular weight and optionally convert to one target unit.
#' @noRd
convert_with_molecular_weight <- function(
  x, mol_weight, target, operator, fn, input_name, input_mask = NULL
) {
  mw <- prepare_molecular_weight(mol_weight, length(x))
  operand <- if (inherits(x, "mixed_units")) {
    units::mixed_units(as.numeric(mw), rep("g/mol", length(mw)))
  } else {
    mw
  }
  result <- if (operator == "/") x / operand else x * operand

  if (!is.null(target)) {
    checkmate::assert_string(target)
    target <- normalize_unit_string(target)
    result <- units::set_units(result, target, mode = "standard")
    if (inherits(result, "mixed_units")) {
      result <- units::set_units(
        units::drop_units(result), target, mode = "standard"
      )
    }
  }

  result <- apply_mv_mask(result, input_mask)

  log_audit_event(
    "unit", fn = fn, input = input_name,
    from = paste(unique(as.character(units(x))), collapse = ","),
    to = paste(unique(as.character(units(result))), collapse = ","),
    transform = "convert",
    detail = paste0("MW=", paste(unique(signif(as.numeric(mw), 8)), collapse = ","), " g/mol"),
    n = sum(!is.na(units::drop_units(result)))
  )
  result
}
