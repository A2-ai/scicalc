test_that("convert_mass_to_mol converts a mass concentration to molar", {
  x <- convert_mass_to_mol(1, "mg/dL", "umol/L", mol_weight = 113.12)
  expect_s3_class(x, "units")
  expect_equal(as.character(units(x)), "umol/L")
  expect_equal(as.numeric(x), 88.4017, tolerance = 1e-3)
})

test_that("convert_mol_to_mass is the inverse", {
  x <- convert_mol_to_mass(88.4017, "mg/dL", "umol/L", mol_weight = 113.12)
  expect_equal(as.character(units(x)), "mg/dL")
  expect_equal(as.numeric(x), 1, tolerance = 1e-4)
})

test_that("molar conversions work for plain amounts (no volume)", {
  x <- convert_mass_to_mol(5, "mg", "umol", mol_weight = 113.12)
  expect_equal(as.character(units(x)), "umol")
  # 5 mg / 113.12 g/mol = 44.2 umol
  expect_equal(as.numeric(x), 5 / 113.12 * 1e3, tolerance = 1e-1)
})

test_that("molar conversions accept a units input and error on bad units", {
  m <- units::set_units(1, "mg/dL", mode = "standard")
  expect_equal(
    as.numeric(convert_mass_to_mol(m, "mg/dL", "umol/L", 113.12)),
    88.4017,
    tolerance = 1e-3
  )
  # incompatible target dimension errors
  expect_error(convert_mass_to_mol(1, "mg/dL", "L", 113.12))
})
