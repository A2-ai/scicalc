test_that("convert_mass_to_mol converts a mass concentration to molar", {
  mass <- units::set_units(1, "mg/dL", mode = "standard")
  mw <- units::set_units(113.12, "g/mol", mode = "standard")
  x <- convert_mass_to_mol(mass, mw, mol_units = "umol/L")
  expect_s3_class(x, "units")
  expect_equal(as.character(units(x)), "umol/L")
  expect_equal(as.numeric(x), 88.4017, tolerance = 1e-3)
})

test_that("convert_mol_to_mass is the inverse", {
  mol <- units::set_units(88.4017, "umol/L", mode = "standard")
  mw <- units::set_units(113.12, "g/mol", mode = "standard")
  x <- convert_mol_to_mass(mol, mw, mass_units = "mg/dL")
  expect_equal(as.character(units(x)), "mg/dL")
  expect_equal(as.numeric(x), 1, tolerance = 1e-4)
})

test_that("molar conversions work for plain amounts (no volume)", {
  mw <- units::set_units(113.12, "g/mol", mode = "standard")
  x <- convert_mass_to_mol(5, mw, mol_units = "umol", mass_units = "mg")
  expect_equal(as.character(units(x)), "umol")
  # 5 mg / 113.12 g/mol = 44.2 umol
  expect_equal(as.numeric(x), 5 / 113.12 * 1e3, tolerance = 1e-1)
})

test_that("numeric methods require only their class-specific source unit", {
  mw <- units::set_units(113.12, "g/mol", mode = "standard")

  expect_error(convert_mass_to_mol(1, mw), "mass_units.*required")
  expect_error(convert_mol_to_mass(88.4017, mw), "mol_units.*required")

  mass <- units::set_units(1, "mg/dL", mode = "standard")
  mol <- units::set_units(88.4017, "umol/L", mode = "standard")
  expect_error(
    convert_mass_to_mol(mass, mw, mass_units = "mg/dL"),
    "must be empty"
  )
  expect_error(
    convert_mol_to_mass(mol, mw, mol_units = "umol/L"),
    "must be empty"
  )
})

test_that("numeric molecular weight warns and assumes g/mol", {
  mass <- units::set_units(1, "mg/dL", mode = "standard")
  expect_warning(
    x <- convert_mass_to_mol(mass, 113.12, mol_units = "umol/L"),
    "assuming \\[g/mol\\]",
    class = "scicalc_assumed_molecular_weight_units"
  )
  expect_equal(as.numeric(x), 88.4017, tolerance = 1e-3)
})

test_that("molecular weight units are converted to g/mol", {
  mass <- units::set_units(1, "mg/dL", mode = "standard")
  mw <- units::set_units(0.11312, "kg/mol", mode = "standard")
  x <- convert_mass_to_mol(mass, mw, mol_units = "umol/L")
  expect_equal(as.numeric(x), 88.4017, tolerance = 1e-3)
})

test_that("mixed mass units support row-level molecular weights", {
  mass <- units::mixed_units(c(1, 500), c("ug/mL", "ng/mL"))
  mw <- units::set_units(c(500, 250), "g/mol", mode = "standard")

  natural <- convert_mass_to_mol(mass, mw)
  expect_s3_class(natural, "mixed_units")

  mol <- convert_mass_to_mol(mass, mw, mol_units = "nmol/L")
  expect_s3_class(mol, "units")
  expect_equal(as.character(units(mol)), "nmol/L")
  expect_equal(as.numeric(mol), c(2000, 2000))
})

test_that("mixed molar units convert back to one mass unit", {
  mol <- units::mixed_units(c(2, 3), c("nmol/L", "umol/L"))
  mw <- units::set_units(c(500, 250), "g/mol", mode = "standard")

  natural <- convert_mol_to_mass(mol, mw)
  expect_s3_class(natural, "mixed_units")

  mass <- convert_mol_to_mass(mol, mw, mass_units = "ng/mL")
  expect_s3_class(mass, "units")
  expect_equal(as.character(units(mass)), "ng/mL")
  expect_equal(as.numeric(mass), c(1, 750))
})

test_that("molecular weight length and values are validated", {
  mass <- units::set_units(1:3, "mg", mode = "standard")

  expect_error(
    suppressWarnings(convert_mass_to_mol(mass, c(100, 200))),
    "length 1 or the same length"
  )
  expect_error(
    suppressWarnings(convert_mass_to_mol(mass, 0)),
    "greater than zero"
  )
  expect_error(
    convert_mass_to_mol(mass, units::mixed_units(100, "g/mol")),
    "standard `units` vector"
  )
})

test_that("incompatible target units and unsupported inputs error", {
  mw <- units::set_units(113.12, "g/mol", mode = "standard")
  mass <- units::set_units(1, "mg/dL", mode = "standard")

  expect_error(convert_mass_to_mol(mass, mw, mol_units = "L"))
  expect_error(
    convert_mass_to_mol("one", mw),
    "No `convert_mass_to_mol\\(\\)` method"
  )
})
