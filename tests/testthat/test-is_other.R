test_that("is_other works for single entries", {
  expect_equal(is_other("OTHER"), TRUE)
  expect_equal(is_other("MULTIPLE"), TRUE)
  expect_equal(is_other("White"), FALSE)
})

test_that("is_other works for vectors", {
  expect_equal(
    is_other(c("Other", "Multiple", "White", "Asian")),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})
