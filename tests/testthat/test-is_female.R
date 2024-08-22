test_that("is_female works for single entries", {
  expect_equal(is_female("F"), TRUE)
})

test_that("is_female works for full FEMALE/MALE", {
  expect_equal(is_female("MALE"), FALSE)
})

test_that("is_female works for vectors", {
  expect_equal(is_female(c("male", "FEMALE", "Male", "Female")), c(FALSE, TRUE, FALSE, TRUE))
})
