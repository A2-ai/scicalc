test_that("rfc sets category_standard attribute", {
  result_regulatory <- rfc(estimator = 60, absolute_units = TRUE)
  expect_equal(attr(result_regulatory, "category_standard"), "FDA")

  result_clinical <- rfc(estimator = 60, absolute_units = TRUE, bsa = 1.73, category_standard = "clinical")
  expect_equal(attr(result_clinical, "category_standard"), "KDIGO")
})

test_that("rfc correctly calculates regulatory categories with absolute estimator", {
  expect_equal(rfc(estimator = 0, absolute_units = TRUE), 4, ignore_attr = TRUE)
  expect_equal(rfc(estimator = 30, absolute_units = TRUE), 3, ignore_attr = TRUE)
  expect_equal(rfc(estimator = 60, absolute_units = TRUE), 2, ignore_attr = TRUE)
  expect_equal(rfc(estimator = 90, absolute_units = TRUE), 1, ignore_attr = TRUE)
  expect_equal(rfc(estimator = 100, absolute_units = TRUE), 1, ignore_attr = TRUE)
  expect_equal(rfc(estimator = NA, absolute_units = TRUE), -999, ignore_attr = TRUE)
})

test_that("rfc correctly calculates regulatory categories with relative estimator and bsa", {
  expect_equal(rfc(estimator = 0, absolute_units = FALSE, bsa = 1.73), 4, ignore_attr = TRUE)
  expect_equal(rfc(estimator = 30, absolute_units = FALSE, bsa = 1.73), 3, ignore_attr = TRUE)
  expect_equal(rfc(estimator = 60, absolute_units = FALSE, bsa = 1.73), 2, ignore_attr = TRUE)
  expect_equal(rfc(estimator = 90, absolute_units = FALSE, bsa = 1.73), 1, ignore_attr = TRUE)
  expect_equal(rfc(estimator = 100, absolute_units = FALSE, bsa = 1.73), 1, ignore_attr = TRUE)
  expect_equal(rfc(estimator = NA, absolute_units = FALSE, bsa = 1.73), -999, ignore_attr = TRUE)
})

test_that("rfc correctly calculates clinical categories with absolute estimator", {
  expect_equal(
    rfc(
      estimator = 0,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    5,
    ignore_attr = TRUE
  )
  expect_equal(
    rfc(
      estimator = 10,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    5,
    ignore_attr = TRUE
  )
  expect_equal(
    rfc(
      estimator = 15,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    4,
    ignore_attr = TRUE
  )
  expect_equal(
    rfc(
      estimator = 30,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    3,
    ignore_attr = TRUE
  )
  expect_equal(
    rfc(
      estimator = 60,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    2,
    ignore_attr = TRUE
  )
  expect_equal(
    rfc(
      estimator = 90,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    1,
    ignore_attr = TRUE
  )
  expect_equal(
    rfc(
      estimator = 100,
      absolute_units = TRUE,
      bsa = 1.73,
      category_standard = "clinical"
    ),
    1,
    ignore_attr = TRUE
  )
})

test_that("rfc correctly calculates clinical categories with relative estimator", {
  expect_equal(
    rfc(estimator = 0, absolute_units = FALSE, category_standard = "clinical"),
    5,
    ignore_attr = TRUE
  )
  expect_equal(
    rfc(estimator = 15, absolute_units = FALSE, category_standard = "clinical"),
    4,
    ignore_attr = TRUE
  )
  expect_equal(
    rfc(estimator = 30, absolute_units = FALSE, category_standard = "clinical"),
    3,
    ignore_attr = TRUE
  )
  expect_equal(
    rfc(estimator = 60, absolute_units = FALSE, category_standard = "clinical"),
    2,
    ignore_attr = TRUE
  )
  expect_equal(
    rfc(estimator = 90, absolute_units = FALSE, category_standard = "clinical"),
    1,
    ignore_attr = TRUE
  )
  expect_equal(
    rfc(
      estimator = 100,
      absolute_units = FALSE,
      category_standard = "clinical"
    ),
    1,
    ignore_attr = TRUE
  )
  expect_equal(
    rfc(estimator = NA, absolute_units = FALSE, category_standard = "clinical"),
    -999,
    ignore_attr = TRUE
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
  # Create input with relative units (from egfr())
  relative_input <- structure(60, scicalc_units = "mL/min/1.73m^2")

  # Should infer absolute_units = FALSE and use clinical pathway without bsa
  expect_equal(
    rfc(estimator = relative_input, category_standard = "clinical"),
    2,
    ignore_attr = TRUE
  )

  # Create input with absolute units (from aegfr())
  absolute_input <- structure(60, scicalc_units = "mL/min")

  # Should infer absolute_units = TRUE and use regulatory pathway

  expect_equal(rfc(estimator = absolute_input), 2, ignore_attr = TRUE)
})

test_that("an explicit absolute_units wins over the units attribute", {
  # Attribute says relative, but the caller explicitly declares absolute: the
  # explicit flag is used, no warning, no attribute override.
  tagged <- structure(60, scicalc_units = "mL/min/1.73m^2")
  expect_no_warning(
    result <- rfc(estimator = tagged, absolute_units = TRUE)
  )
  expect_equal(result, rfc(estimator = 60, absolute_units = TRUE), ignore_attr = TRUE)
})

test_that("rfc works with egfr() output directly", {
  # Simulate pipeline: egfr() -> rfc()
  egfr_result <- .egfr_ckdepi_2021(TRUE, 30, 1.0)
  expect_equal(attr(egfr_result, "scicalc_units"), "mL/min/1.73m^2")

  # rfc should infer units from attribute
  rfc_result <- rfc(egfr_result, category_standard = "clinical")
  expect_true(rfc_result %in% 1:5)
})

test_that("rfc works with aegfr() output directly", {
  # Simulate pipeline: egfr() -> aegfr() -> rfc()
  egfr_result <- .egfr_ckdepi_2021(TRUE, 30, 1.0)
  aegfr_result <- aegfr(egfr_result, 1.8)
  expect_equal(attr(aegfr_result, "scicalc_units"), "mL/min")

  # rfc should infer units from attribute
  rfc_result <- rfc(aegfr_result)
  expect_true(rfc_result %in% 1:4)
})

test_that("rfc warns and masks when estimator contains missing value sentinel", {
  expect_warning(
    result <- rfc(estimator = c(90, -999), absolute_units = TRUE),
    "estimator contains missing value indicator"
  )
  expect_equal(result, c(1, -999), ignore_attr = TRUE)
})

test_that("rfc warns and masks when bsa contains missing value sentinel and bsa is used", {
  # regulatory + relative units → bsa IS used for conversion
  expect_warning(
    result <- rfc(estimator = c(90, 60), absolute_units = FALSE, bsa = c(1.73, -999)),
    "bsa contains missing value indicator"
  )
  expect_equal(result[2], -999, ignore_attr = TRUE)
})

test_that("rfc ignores bsa sentinel when bsa is not used for conversion", {
  # regulatory + absolute units → bsa is not used, sentinel should not mask
  result <- rfc(estimator = c(90, 60), absolute_units = TRUE, bsa = c(1.73, -999))
  expect_equal(result, c(1, 2), ignore_attr = TRUE)

  # clinical + relative units → bsa is not used, sentinel should not mask
  result <- rfc(estimator = c(90, 60), absolute_units = FALSE,
                bsa = c(1.73, -999), category_standard = "clinical")
  expect_equal(result, c(1, 2), ignore_attr = TRUE)
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
    c(2, -999, 1),
    ignore_attr = TRUE
  )
  expect_message(
    rfc(estimator = c(60, NA, 90), absolute_units = TRUE),
    "Estimator input has missing values"
  )
})

test_that("a foreign units attribute is ignored by rfc", {
  # A non-scicalc `units` attribute (e.g. m^2 inherited through arithmetic from
  # a BSA column) is not read by rfc, so it cannot flip the pathway.
  stray <- structure(90, units = "m^2")
  expect_no_warning(
    result <- rfc(estimator = stray, absolute_units = TRUE)
  )
  expect_equal(result, rfc(estimator = 90, absolute_units = TRUE), ignore_attr = TRUE)
  # and with no explicit flag, the foreign attribute does not satisfy inference
  expect_error(rfc(estimator = stray), "Must supply absolute_units")
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
    c(2, -999),
    ignore_attr = TRUE
  )
})
