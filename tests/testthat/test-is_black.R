test_that("is_black works for single entries", {
  expect_equal(is_black("BLACK"), TRUE)
  expect_equal(is_black("AFRICAN AMERICAN"), TRUE)
  expect_equal(is_black("White"), FALSE)
})

test_that("is_black works for vectors", {
  expect_equal(is_black(c("Black", "White", "Black", "Asian")), c(TRUE, FALSE, TRUE, FALSE))
})
