test_that("is_unspecified is TRUE for unrecognized values and warns", {
  expect_warning(
    res <- is_unspecified(c("ROMAN LATIN", "WHITE")),
    "Unspecified race value.*ROMAN LATIN"
  )
  expect_equal(res, c(TRUE, FALSE))
})

test_that("is_unspecified treats unknown and NA as specified", {
  expect_equal(is_unspecified("UNKNOWN"), FALSE)
  expect_equal(is_unspecified(NA_character_), FALSE)
})

test_that("is_unspecified honors the known argument", {
  expect_silent(res <- is_unspecified("JAPANESE", known = "japanese"))
  expect_equal(res, FALSE)
})

test_that("is_unspecified handles numeric input", {
  expect_message(res <- is_unspecified(c(7, 1, -999)), "Numeric input detected")
  expect_equal(res, c(TRUE, FALSE, FALSE))
})
