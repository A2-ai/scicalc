test_that("is_pacific_islander works for single entries", {
  expect_equal(
    is_pacific_islander("NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"),
    TRUE
  )
  expect_equal(is_pacific_islander("Pacific Islander"), TRUE)
  expect_equal(is_pacific_islander("native hawaiian"), TRUE)
  expect_equal(is_pacific_islander("WHITE"), FALSE)
})

test_that("is_pacific_islander works for vectors", {
  expect_equal(
    is_pacific_islander(c("Pacific Islander", "White", "Native Hawaiian")),
    c(TRUE, FALSE, TRUE)
  )
})

test_that("is_pacific_islander handles numeric input", {
  expect_message(res <- is_pacific_islander(c(5, 1, 4)), "Numeric input detected")
  expect_equal(res, c(TRUE, FALSE, FALSE))
})
