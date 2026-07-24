test_that("is_american_native works for single entries", {
  expect_equal(is_american_native("AMERICAN INDIAN OR ALASKA NATIVE"), TRUE)
  expect_equal(is_american_native("Native American"), TRUE)
  expect_equal(is_american_native("alaska native"), TRUE)
  expect_equal(is_american_native("WHITE"), FALSE)
})

test_that("is_american_native works for vectors", {
  expect_equal(
    is_american_native(c("American Native", "White", "Native American")),
    c(TRUE, FALSE, TRUE)
  )
})

test_that("is_american_native handles numeric input", {
  expect_message(res <- is_american_native(c(4, 1, 5)), "Numeric input detected")
  expect_equal(res, c(TRUE, FALSE, FALSE))
})
