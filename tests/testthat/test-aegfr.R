test_that("aegfr correctly converts relative to absolute eGFR", {
  # With BSA = 1.73, result should equal input (1.73/1.73 = 1)
  expect_equal(aegfr(90, 1.73), 90, ignore_attr = TRUE)

  # With BSA = 2.0, result should be higher
  result <- aegfr(90, 2.0)
  expect_equal(result, 90 * (2.0 / 1.73), tolerance = 0.01, ignore_attr = TRUE)

  # With BSA = 1.5, result should be lower
  result <- aegfr(90, 1.5)
  expect_equal(result, 90 * (1.5 / 1.73), tolerance = 0.01, ignore_attr = TRUE)
})

test_that("aegfr handles vectorized input", {
  egfr_vals <- c(60, 90, 120)
  bsa_vals <- c(1.8, 1.73, 2.0)
  result <- aegfr(egfr_vals, bsa_vals)

  expected <- egfr_vals * (bsa_vals / 1.73)
  expect_equal(as.numeric(result), expected, tolerance = 0.01)
})

test_that("aegfr sets units attribute to mL/min", {
  result <- aegfr(90, 1.8)
  expect_equal(as.character(units(result)), "mL/min")
})

test_that("aegfr warns and returns unchanged if input already has absolute units", {
  # Create input with absolute units (units object)
  input <- units::set_units(90, "mL/min", mode = "standard")

  expect_warning(
    result <- aegfr(input, 1.8),
    "already has absolute units"
  )

  # Should return unchanged
  expect_equal(as.numeric(result), 90)
  expect_equal(as.character(units(result)), "mL/min")
})

test_that("aegfr warns about legacy attr units", {
  # Legacy attr fallback triggers deprecation warning
  input <- 90
  attr(input, "units") <- "mL/min"

  lifecycle::expect_deprecated(
    expect_warning(
      result <- aegfr(input, 1.8),
      "already has absolute units"
    )
  )
  expect_equal(as.numeric(result), 90)
})

test_that("aegfr handles missing values", {
  expect_message(aegfr(c(90, NA), c(1.8, 1.8)), "egfr contains missing values")
  expect_message(aegfr(c(90, 90), c(1.8, NA)), "bsa contains missing values")

  result <- suppressMessages(aegfr(c(90, NA), c(1.8, 1.8)))
  expect_true(is.na(result[2]))
})

test_that("aegfr preserves units attribute through pipeline from egfr()", {
  # Simulate pipeline: egfr() -> aegfr()
  egfr_result <- .egfr_ckdepi_2021(TRUE, 30, 1.0)
  expect_equal(as.character(units(egfr_result)), "mL/(min*bsa_ref)")

  aegfr_result <- aegfr(egfr_result, 1.8)
  expect_equal(as.character(units(aegfr_result)), "mL/min")
})
