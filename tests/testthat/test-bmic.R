test_that("bmic works for basic BMI categorization and boundaries", {
  # Basic categories
  expect_equal(
    bmic(c(17.0, 22.0, 27.0, 35.0), rep(25, 4)),
    c(1, 2, 3, 4)
  )

  # Boundary values
  expect_equal(bmic(18.49, 25), 1) # Just under normal
  expect_equal(bmic(18.5, 25), 2) # Exactly normal
  expect_equal(bmic(24.99, 25), 2) # Just under overweight
  expect_equal(bmic(25.0, 25), 3) # Exactly overweight
  expect_equal(bmic(29.99, 25), 3) # Just under obese
  expect_equal(bmic(30.0, 25), 4) # Exactly obese
})

test_that("bmic handles missing values correctly", {
  # Messages about missing values
  expect_message(
    bmic(c(22.0, NA), c(25, 30)),
    "BMI contains missing values"
  )
  expect_message(
    bmic(c(22.0, 25.0), c(25, NA)),
    "age contains missing values"
  )

  # Returns -999 for missing BMI
  result <- suppressMessages(bmic(c(22.0, NA, 30.0), c(25, 30, 35)))
  expect_equal(result, c(2, -999, 4))
})

test_that("bmic warns about age < 18", {
  expect_warning(
    bmic(c(22.0, 25.0), c(17, 25)),
    "Age contains values less than 18 years"
  )
})

test_that("bmic handles negative and zero BMI correctly", {
  # Negative BMI message and -999 return
  expect_message(
    bmic(c(-1.0, 22.0), c(25, 30)),
    "BMI contains negative values"
  )
  result_neg <- suppressMessages(bmic(c(-1.0, 22.0), c(25, 30)))
  expect_equal(result_neg, c(-999, 2))

  # Zero BMI returns -999
  result_zero <- suppressMessages(bmic(c(0, 22.0), c(25, 30)))
  expect_equal(result_zero, c(-999, 2))
})
