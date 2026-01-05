test_that("rfc correctly calculates regulatory categories with absolute estimator", {
  expect_equal(rfc(estimator = 0, absolute_units = TRUE), 4)
  expect_equal(rfc(estimator = 30, absolute_units = TRUE), 3)
  expect_equal(rfc(estimator = 60, absolute_units = TRUE), 2)
  expect_equal(rfc(estimator = 90, absolute_units = TRUE), 1)
  expect_equal(rfc(estimator = 100, absolute_units = TRUE), 1)
  expect_equal(rfc(estimator = NA, absolute_units = TRUE), -999)
})

test_that("rfc correctly calculates regulatory categories with relative estimator and bsa", {
  expect_equal(rfc(estimator = 0, absolute_units = FALSE, bsa = 1.73), 4)
  expect_equal(rfc(estimator = 30, absolute_units = FALSE, bsa = 1.73), 3)
  expect_equal(rfc(estimator = 60, absolute_units = FALSE, bsa = 1.73), 2)
  expect_equal(rfc(estimator = 90, absolute_units = FALSE, bsa = 1.73), 1)
  expect_equal(rfc(estimator = 100, absolute_units = FALSE, bsa = 1.73), 1)
  expect_equal(rfc(estimator = NA, absolute_units = FALSE, bsa = 1.73), -999)
})

test_that("rfc correctly calculates clinical categories with absolute estimator", {
  expect_equal(
    rfc(
      estimator = 0,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    5
  )
  expect_equal(
    rfc(
      estimator = 10,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    5
  )
  expect_equal(
    rfc(
      estimator = 15,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    4
  )
  expect_equal(
    rfc(
      estimator = 30,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    3
  )
  expect_equal(
    rfc(
      estimator = 60,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    2
  )
  expect_equal(
    rfc(
      estimator = 90,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    1
  )
  expect_equal(
    rfc(
      estimator = 100,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    1
  )
})

test_that("rfc correctly calculates clinical categories with relative estimator", {
  expect_equal(
    rfc(estimator = 0, absolute_units = FALSE, category_standard = "clinical"),
    5
  )
  expect_equal(
    rfc(estimator = 15, absolute_units = FALSE, category_standard = "clinical"),
    4
  )
  expect_equal(
    rfc(estimator = 30, absolute_units = FALSE, category_standard = "clinical"),
    3
  )
  expect_equal(
    rfc(estimator = 60, absolute_units = FALSE, category_standard = "clinical"),
    2
  )
  expect_equal(
    rfc(estimator = 90, absolute_units = FALSE, category_standard = "clinical"),
    1
  )
  expect_equal(
    rfc(
      estimator = 100,
      absolute_units = FALSE,
      category_standard = "clinical"
    ),
    1
  )
  expect_equal(
    rfc(estimator = NA, absolute_units = FALSE, category_standard = "clinical"),
    -999
  )
})


test_that("rfc validates required parameters", {
  expect_error(rfc())
  expect_error(
    rfc(estimator = 60),
    "Must supply absolute_units when input has no units attribute"
  )
  expect_error(rfc(estimator = 60, absolute_units = FALSE)) #regulatory category needs bsa for relative estimator
  expect_error(rfc(
    estimator = 60,
    absolute_units = TRUE,
    category_standard = "clinical"
  )) #clinical category needs bsa for absolute estimator
})

test_that("rfc infers absolute_units from input attribute", {
  # Create input with relative units attribute (from egfr())
  relative_input <- 60
  attr(relative_input, "units") <- "mL/min/1.73m^2"

  # Should infer absolute_units = FALSE and use clinical pathway without bsa
  expect_equal(
    rfc(estimator = relative_input, category_standard = "clinical"),
    2
  )

  # Create input with absolute units attribute (from aegfr())
  absolute_input <- 60
  attr(absolute_input, "units") <- "mL/min"

  # Should infer absolute_units = TRUE and use regulatory pathway

  expect_equal(rfc(estimator = absolute_input), 2)
})

test_that("rfc warns when absolute_units conflicts with attribute", {
  # Create input with relative units
  relative_input <- 60
  attr(relative_input, "units") <- "mL/min/1.73m^2"

  # Providing absolute_units = TRUE should warn and use attribute
  expect_warning(
    rfc(estimator = relative_input, absolute_units = TRUE, bsa = 1.73),
    "conflicts with input units attribute"
  )

  # Create input with absolute units
  absolute_input <- 60
  attr(absolute_input, "units") <- "mL/min"

  # Providing absolute_units = FALSE should warn and use attribute
  expect_warning(
    rfc(estimator = absolute_input, absolute_units = FALSE, bsa = 1.73),
    "conflicts with input units attribute"
  )
})

test_that("rfc works with egfr() output directly", {
  # Simulate pipeline: egfr() -> rfc()
  egfr_result <- ckdepi_2021_egfr(TRUE, 30, 1.0)
  expect_equal(attr(egfr_result, "units"), "mL/min/1.73m^2")

  # rfc should infer units from attribute
  rfc_result <- rfc(egfr_result, category_standard = "clinical")
  expect_true(rfc_result %in% 1:5)
})

test_that("rfc works with aegfr() output directly", {
  # Simulate pipeline: egfr() -> aegfr() -> rfc()
  egfr_result <- ckdepi_2021_egfr(TRUE, 30, 1.0)
  aegfr_result <- aegfr(egfr_result, 1.8)
  expect_equal(attr(aegfr_result, "units"), "mL/min")

  # rfc should infer units from attribute
  rfc_result <- rfc(aegfr_result)
  expect_true(rfc_result %in% 1:4)
})

test_that("rfc handles missing values correctly", {
  # Test NA handling with messages
  expect_message(
    rfc(estimator = NA, absolute_units = TRUE),
    "Estimator input has missing values"
  )
  expect_message(
    rfc(estimator = NA, absolute_units = FALSE, category_standard = "clinical"),
    "Estimator input has missing values"
  )

  # Test mixed NA vectors
  expect_equal(
    rfc(estimator = c(60, NA, 90), absolute_units = TRUE),
    c(2, -999, 1)
  )
  expect_message(
    rfc(estimator = c(60, NA, 90), absolute_units = TRUE),
    "Estimator input has missing values"
  )
})

test_that("rfc conversion functions handle BSA validation correctly", {
  # Test that conversion functions catch BSA issues
  expect_error(
    rfc(estimator = c(60, 70), absolute_units = FALSE, bsa = c(1.8, NA)),
    "bsa cannot be missing when relative_est has values"
  )
  expect_error(
    rfc(
      estimator = c(60, 70),
      absolute_units = TRUE,
      bsa = c(1.8, NA),
      category_standard = "clinical"
    ),
    "bsa cannot be missing when absolute_est has values"
  )

  # But NA estimator with NA BSA should be fine
  expect_equal(
    rfc(estimator = c(60, NA), absolute_units = FALSE, bsa = c(1.8, NA)),
    c(2, -999)
  )
})
