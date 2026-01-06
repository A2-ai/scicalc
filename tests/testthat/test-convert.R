test_that("convert_alb sets units attribute", {
  result <- convert_alb(40)
  expect_equal(attr(result, "units"), "g/dL")
})

test_that("convert_alb converts g/L to g/dL", {
  # 40 g/L = 4 g/dL

  expect_equal(convert_alb(40), 4, ignore_attr = TRUE)
  expect_equal(convert_alb(35), 3.5, ignore_attr = TRUE)
  expect_equal(convert_alb(0), 0, ignore_attr = TRUE)

  # vectorized

  expect_equal(convert_alb(c(30, 40, 50)), c(3, 4, 5), ignore_attr = TRUE)
})

test_that("convert_alb handles missing values", {
  expect_message(convert_alb(c(40, NA)), "alb contains missing values")
  result <- suppressMessages(convert_alb(c(40, NA)))
  expect_equal(result, c(4, NA), ignore_attr = TRUE)
})

test_that("convert_bili sets units attribute", {
  result <- convert_bili(17.1)
  expect_equal(attr(result, "units"), "mg/dL")
})

test_that("convert_bili converts umol/L to mg/dL", {
  # 17.1 umol/L ≈ 1 mg/dL
  expect_equal(convert_bili(17.1), 17.1 * 584.673 / 10000, tolerance = 1e-6, ignore_attr = TRUE)

  # verify known conversion: 1 mg/dL = 17.1 umol/L
  result <- convert_bili(17.1)
  expect_equal(result, 1, tolerance = 0.01, ignore_attr = TRUE)

  expect_equal(convert_bili(0), 0, ignore_attr = TRUE)

  # vectorized
  expect_length(convert_bili(c(10, 20, 30)), 3)
})

test_that("convert_bili handles missing values", {
  expect_message(convert_bili(c(17.1, NA)), "bili contains missing values")
  result <- suppressMessages(convert_bili(c(17.1, NA)))
  expect_equal(result[1], 1, tolerance = 0.01, ignore_attr = TRUE)
  expect_true(is.na(result[2]))
})

test_that("convert_creat sets units attribute", {
  result <- convert_creat(88.4)
  expect_equal(attr(result, "units"), "mg/dL")
})

test_that("convert_creat converts umol/L to mg/dL", {
  # 88.42 umol/L ≈ 1 mg/dL
  expect_equal(convert_creat(88.42), 88.42 * 113.12 / 10000, tolerance = 1e-6, ignore_attr = TRUE)

  # verify known conversion: 1 mg/dL ≈ 88.4 umol/L
  result <- convert_creat(88.4)
  expect_equal(result, 1, tolerance = 0.01, ignore_attr = TRUE)

  expect_equal(convert_creat(0), 0, ignore_attr = TRUE)

  # vectorized
  expect_length(convert_creat(c(70, 90, 110)), 3)
})

test_that("convert_creat handles missing values", {
  expect_message(convert_creat(c(88.4, NA)), "creat contains missing values")
  result <- suppressMessages(convert_creat(c(88.4, NA)))
  expect_equal(result[1], 1, tolerance = 0.01, ignore_attr = TRUE)
  expect_true(is.na(result[2]))
})
