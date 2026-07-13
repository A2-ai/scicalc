test_that("assert_and_strip_units passes plain numerics through", {
  expect_equal(assert_and_strip_units(70, "kg"), 70)
})

test_that("assert_and_strip_units converts compatible units and informs", {
  x <- units::set_units(150, "lb")
  expect_message(
    result <- assert_and_strip_units(x, "kg"),
    "converted from \\[lb\\] to \\[kg\\]"
  )
  expect_equal(result, 150 * 0.453592, tolerance = 0.01)
})

test_that("assert_and_strip_units does not inform when units already match", {
  x <- units::set_units(70, "kg")
  expect_no_message(result <- assert_and_strip_units(x, "kg"))
  expect_equal(result, 70)
})

test_that("assert_and_strip_units errors on incompatible units", {
  x <- units::set_units(70, "kg")
  expect_error(assert_and_strip_units(x, "cm"), "cannot be converted")
})

test_that("bmi auto-converts units on input", {
  # 150 lb ≈ 68.04 kg, 67 in ≈ 170.18 cm
  expect_message(
    result <- bmi(units::set_units(150, "lb"), units::set_units(67, "inches")),
    "converted from"
  )
  expect_equal(
    as.numeric(result),
    as.numeric(bmi(68.04, 170.18)),
    tolerance = 0.1
  )
})

test_that("bmi errors on incompatible input units", {
  expect_error(bmi(units::set_units(70, "m"), 170), "cannot be converted")
})

test_that("bsa auto-converts units on input", {
  expect_message(
    result <- .bsa_dubois(units::set_units(150, "lb"), units::set_units(67, "inches")),
    "converted from"
  )
  expect_equal(
    as.numeric(result),
    as.numeric(.bsa_dubois(68.04, 170.18)),
    tolerance = 0.01
  )
})

test_that("crcl auto-converts units on input", {
  expect_message(
    result <- crcl(TRUE, units::set_units(24, "years"), units::set_units(1, "mg/dL"), units::set_units(150, "lb")),
    "converted from \\[lb\\] to \\[kg\\]"
  )
  expect_equal(
    as.numeric(result),
    as.numeric(crcl(TRUE, 24, 1, 68.04)),
    tolerance = 0.1
  )
})
