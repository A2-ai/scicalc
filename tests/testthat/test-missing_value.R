test_that("is_missing_value flags the sentinel in plain numeric vectors", {
  expect_equal(is_missing_value(c(1, -999, 3)), c(FALSE, TRUE, FALSE))
})

test_that("is_missing_value treats NA as not-missing-sentinel", {
  expect_equal(is_missing_value(c(1, NA, -999)), c(FALSE, FALSE, TRUE))
})

test_that("is_missing_value honors the scicalc.missing_value option", {
  withr::local_options(scicalc.missing_value = -1)
  expect_equal(is_missing_value(c(1, -1, -999)), c(FALSE, TRUE, FALSE))
})

test_that("is_missing_value accepts an explicit missing_value", {
  expect_equal(is_missing_value(c(1, 9999, 3), missing_value = 9999), c(FALSE, TRUE, FALSE))
})

test_that("is_missing_value returns all FALSE when the sentinel is NA", {
  withr::local_options(scicalc.missing_value = NA)
  expect_equal(is_missing_value(c(1, NA, 3)), c(FALSE, FALSE, FALSE))
})
