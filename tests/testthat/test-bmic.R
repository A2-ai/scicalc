test_that("bmic sets category_standard attribute", {
  result <- bmic(22.0, 25)
  expect_equal(attr(result, "category_standard"), "WHO")
})

test_that("bmic works for basic BMI categorization and boundaries", {
  # Basic categories
  expect_equal(
    bmic(c(17.0, 22.0, 27.0, 32.0, 37.0, 42.0), rep(25, 6)),
    c(1, 2, 3, 4, 5, 6),
    ignore_attr = TRUE
  )

  # Boundary values
  expect_equal(bmic(18.49, 25), 1, ignore_attr = TRUE) # Just under normal
  expect_equal(bmic(18.5, 25), 2, ignore_attr = TRUE) # Exactly normal
  expect_equal(bmic(24.99, 25), 2, ignore_attr = TRUE) # Just under overweight
  expect_equal(bmic(25.0, 25), 3, ignore_attr = TRUE) # Exactly overweight
  expect_equal(bmic(29.99, 25), 3, ignore_attr = TRUE) # Just under obese class 1
  expect_equal(bmic(30.0, 25), 4, ignore_attr = TRUE) # Exactly obese class 1
  expect_equal(bmic(34.99, 25), 4, ignore_attr = TRUE) # Just under obese class 2
  expect_equal(bmic(35.0, 25), 5, ignore_attr = TRUE) # Exactly obese class 2
  expect_equal(bmic(39.99, 25), 5, ignore_attr = TRUE) # Just under obese class 3
  expect_equal(bmic(40.0, 25), 6, ignore_attr = TRUE) # Exactly obese class 3
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
  expect_equal(result, c(2, -999, 4), ignore_attr = TRUE)
})

test_that("bmic warns about age < 18", {
  expect_warning(
    bmic(c(22.0, 25.0), c(17, 25)),
    "Age contains values less than 18 years"
  )
})

test_that("bmic warns and masks when bmi contains missing value sentinel", {
  expect_warning(
    result <- bmic(c(22, -999), c(25, 30)),
    "bmi contains missing value indicator"
  )
  expect_equal(result, c(2, -999), ignore_attr = TRUE)
})

test_that("bmic propagates an age missing-value sentinel", {
  expect_warning(
    result <- bmic(c(22, 25), c(25, -999)),
    "age contains missing value indicator"
  )
  expect_equal(result, c(2, -999), ignore_attr = TRUE)
})

test_that("bmic handles negative and zero BMI correctly", {
  # Negative BMI message and -999 return
  expect_message(
    bmic(c(-1.0, 22.0), c(25, 30)),
    "BMI contains negative values"
  )
  result_neg <- suppressMessages(bmic(c(-1.0, 22.0), c(25, 30)))
  expect_equal(result_neg, c(-999, 2), ignore_attr = TRUE)

  # Zero BMI returns -999
  result_zero <- suppressMessages(bmic(c(0, 22.0), c(25, 30)))
  expect_equal(result_zero, c(-999, 2), ignore_attr = TRUE)
})

test_that("bmic uses a custom band config when scicalc.bmic_config is set", {
  withr::local_options(scicalc.bmic_config = data.frame(
    label = c("lean", "high"),
    min = c(0, 27),
    code = c(1, 2)
  ))
  result <- bmic(c(22, 30), c(40, 40))
  expect_equal(as.numeric(result), c(1, 2))
  expect_equal(attr(result, "category_standard"), "custom")
})
