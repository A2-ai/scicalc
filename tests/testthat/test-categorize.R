test_that("categorize creates correct bins", {
  vec <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  result <- categorize(vec, nbins = 2)
  expect_equal(levels(result), c("< 5.5", "> 5.5"))
  expect_equal(
    as.character(result),
    c(
      "< 5.5",
      "< 5.5",
      "< 5.5",
      "< 5.5",
      "< 5.5",
      "> 5.5",
      "> 5.5",
      "> 5.5",
      "> 5.5",
      "> 5.5"
    )
  )
})

test_that("categorize handles NA values with a warning", {
  vec <- c(1, 2, 3, NA, 5, 6, 7, 8, 9, 10)
  expect_warning(
    result <- categorize(vec, nbins = 2),
    "NA values, They will be removed"
  )
  expect_equal(levels(result), c("< 6", "> 6"))
  expect_true(is.na(result[4]))
})

test_that("categorize throws an error for invalid nbins", {
  vec <- c(1, 2, 3, 4, 5)
  expect_error(
    categorize(vec, nbins = 1),
    "The number of bins \\(nbins\\) must be at least 2\\."
  )
})

test_that("categorize includes units in labels", {
  vec <- c(1, 2, 3, 4, 5)
  result <- categorize(vec, nbins = 2, units = "mg")
  expect_equal(levels(result), c("< 3 mg", "> 3 mg"))
})

test_that("categorize respects digits parameter", {
  vec <- c(1.00, 2.00, 3.01, 4.00, 5.00)
  result <- categorize(vec, nbins = 2, digits = 2)
  expect_equal(levels(result), c("< 3.01", "> 3.01"))
})

test_that("categorize handles edge cases", {
  vec <- c(5, 5, 5, 5)
  expect_error(
    categorize(vec, nbins = 2),
    "continuous_var contains less than nbins unique values."
  )
})

test_that("categorize returns a factor", {
  vec <- c(1, 2, 3, 4, 5)
  result <- categorize(vec, nbins = 3)
  expect_true(is.factor(result))
})
