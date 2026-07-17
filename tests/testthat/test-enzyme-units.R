test_that("enzyme units convert to katal units", {
  enzyme_unit <- units::set_units(1, "U", mode = "standard")

  expect_equal(
    as.numeric(units::set_units(enzyme_unit, "nkat", mode = "standard")),
    1e3 / 60,
    tolerance = 1e-12
  )
  expect_equal(
    as.numeric(units::set_units(enzyme_unit, "kat", mode = "standard")),
    1e-6 / 60,
    tolerance = 1e-18
  )
})

test_that("enzyme activity concentrations retain the U/L shorthand", {
  activity <- units::set_units(10, "U/L", mode = "standard")

  expect_equal(
    as.numeric(units::set_units(activity, "nkat/L", mode = "standard")),
    10 * 1e3 / 60,
    tolerance = 1e-10
  )
})
