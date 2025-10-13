test_that("obesity_category works for basic BMI categorization and boundaries", {
  # Basic categories
  expect_equal(
    obesity_category(c(17.0, 22.0, 27.0, 35.0), rep(25, 4)),
    c(1, 2, 3, 4)
  )

  # Boundary values
  expect_equal(obesity_category(18.49, 25), 1) # Just under normal
  expect_equal(obesity_category(18.5, 25), 2)  # Exactly normal
  expect_equal(obesity_category(24.99, 25), 2) # Just under overweight
  expect_equal(obesity_category(25.0, 25), 3)  # Exactly overweight
  expect_equal(obesity_category(29.99, 25), 3) # Just under obese
  expect_equal(obesity_category(30.0, 25), 4)  # Exactly obese
})

test_that("obesity_category handles missing values correctly", {
  # Messages about missing values
  expect_message(
    obesity_category(c(22.0, NA), c(25, 30)),
    "BMI contains missing values"
  )
  expect_message(
    obesity_category(c(22.0, 25.0), c(25, NA)),
    "age contains missing values"
  )

  # Returns -999 for missing BMI
  result <- suppressMessages(obesity_category(c(22.0, NA, 30.0), c(25, 30, 35)))
  expect_equal(result, c(2, -999, 4))
})

test_that("obesity_category warns about age < 18", {
  expect_warning(
    obesity_category(c(22.0, 25.0), c(17, 25)),
    "Age contains values less than 18 years"
  )
})

test_that("obesity_category handles negative and zero BMI correctly", {
  # Negative BMI message and -999 return
  expect_message(
    obesity_category(c(-1.0, 22.0), c(25, 30)),
    "BMI contains negative values"
  )
  result_neg <- suppressMessages(obesity_category(c(-1.0, 22.0), c(25, 30)))
  expect_equal(result_neg, c(-999, 2))

  # Zero BMI returns -999
  result_zero <- suppressMessages(obesity_category(c(0, 22.0), c(25, 30)))
  expect_equal(result_zero, c(-999, 2))
})
