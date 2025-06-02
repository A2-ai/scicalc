test_that("brfc correctly calculates regulatory categories with absolute estimator", {
  expect_equal(brfc(estimator = 0, absolute_units = TRUE), 4)
  expect_equal(brfc(estimator = 30, absolute_units = TRUE), 3)
  expect_equal(brfc(estimator = 60, absolute_units = TRUE), 2)
  expect_equal(brfc(estimator = 90, absolute_units = TRUE), 1)
  expect_equal(brfc(estimator = 100, absolute_units = TRUE), 1)
  expect_equal(brfc(estimator = NA, absolute_units = TRUE), -999)
})

test_that("brfc correctly calculates regulatory categories with relative estimator and bsa", {
  expect_equal(brfc(estimator = 0, absolute_units = FALSE, bsa = 1.73), 4)
  expect_equal(brfc(estimator = 30, absolute_units = FALSE, bsa = 1.73), 3)
  expect_equal(brfc(estimator = 60, absolute_units = FALSE, bsa = 1.73), 2)
  expect_equal(brfc(estimator = 90, absolute_units = FALSE, bsa = 1.73), 1)
  expect_equal(brfc(estimator = 100, absolute_units = FALSE, bsa = 1.73), 1)
  expect_equal(brfc(estimator = NA, absolute_units = FALSE, bsa = 1.73), -999)
})

test_that("brfc correctly calculates clinical categories with absolute estimator", {
  expect_equal(brfc(estimator = 0, absolute_units = TRUE, bsa = 1.73, category_standard = "clinical"), 5)
  expect_equal(brfc(estimator = 10, absolute_units = TRUE, bsa = 1.73, category_standard = "clinical"), 5)
  expect_equal(brfc(estimator = 15, absolute_units = TRUE, bsa = 1.73, category_standard = "clinical"), 4)
  expect_equal(brfc(estimator = 30, absolute_units = TRUE, bsa = 1.73, category_standard = "clinical"), 3)
  expect_equal(brfc(estimator = 60, absolute_units = TRUE, bsa = 1.73, category_standard = "clinical"), 2)
  expect_equal(brfc(estimator = 90, absolute_units = TRUE, bsa = 1.73, category_standard = "clinical"), 1)
  expect_equal(brfc(estimator = 100, absolute_units = TRUE, bsa = 1.73, category_standard = "clinical"), 1)
})

test_that("brfc correctly calculates clinical categories with relative estimator", {
  expect_equal(brfc(estimator = 0, absolute_units = FALSE, category_standard = "clinical"), 5)
  expect_equal(brfc(estimator = 15, absolute_units = FALSE, category_standard = "clinical"), 4)
  expect_equal(brfc(estimator = 30, absolute_units = FALSE, category_standard = "clinical"), 3)
  expect_equal(brfc(estimator = 60, absolute_units = FALSE, category_standard = "clinical"), 2)
  expect_equal(brfc(estimator = 90, absolute_units = FALSE, category_standard = "clinical"), 1)
  expect_equal(brfc(estimator = 100, absolute_units = FALSE, category_standard = "clinical"), 1)
  expect_equal(brfc(estimator = NA, absolute_units = FALSE, category_standard = "clinical"), -999)
})


test_that("brfc validates required parameters", {
  expect_error(brfc())
  expect_error(brfc(estimator = 60), "Must supply absolute flag to describe units")
  expect_error(brfc(estimator = 60, absolute_units = FALSE)) #regulatory category needs bsa for relative estimator
  expect_error(brfc(estimator = 60, absolute_units = TRUE, category_standard = "clinical"))#clinical category needs bsa for absolute estimator
})

test_that("brfc handles missing values correctly", {
  # Test NA handling with messages
  expect_message(brfc(estimator = NA, absolute_units = TRUE), "Estimator input has missing values")
  expect_message(brfc(estimator = NA, absolute_units = FALSE, category_standard = "clinical"),
                 "Estimator input has missing values")

  # Test mixed NA vectors
  expect_equal(brfc(estimator = c(60, NA, 90), absolute_units = TRUE), c(2, -999, 1))
  expect_message(brfc(estimator = c(60, NA, 90), absolute_units = TRUE), "Estimator input has missing values")
})

test_that("brfc conversion functions handle BSA validation correctly", {
  # Test that conversion functions catch BSA issues
  expect_error(brfc(estimator = c(60, 70), absolute_units = FALSE, bsa = c(1.8, NA)),
               "bsa cannot be missing when relative_est has values")
  expect_error(brfc(estimator = c(60, 70), absolute_units = TRUE, bsa = c(1.8, NA), category_standard = "clinical"),
               "bsa cannot be missing when absolute_est has values")

  # But NA estimator with NA BSA should be fine
  expect_equal(brfc(estimator = c(60, NA), absolute_units = FALSE, bsa = c(1.8, NA)), c(2, -999))
})
