test_that("bmi works for numerical input", {
  expect_equal(bmi(weight = 60, height = 170) %>% round(3), 20.761, ignore_attr = TRUE)
})

test_that("bmi sets units attribute", {
  result <- bmi(weight = 60, height = 170)
  expect_equal(attr(result, "units"), "kg/m^2")
})

test_that("bmi works for dataframe columns", {
  df <- data.frame(
    "WT" = c(80.56, 71.53, 81.04, 70.17),
    "HT" = c(167, 161, 163, 164)
  )
  expect_equal(
    bmi(weight = df$WT, height = df$HT) %>% round(3),
    c(28.886, 27.595, 30.502, 26.089),
    ignore_attr = TRUE
  )
})

test_that("bmi can be used in a mutate", {
  df <- data.frame(
    "WT" = c(80.56, 71.53, 81.04, 70.17),
    "HT" = c(167, 161, 163, 164)
  )

  df <- df %>%
    dplyr::mutate(bmi = bmi(WT, HT))

  expect_equal(
    bmi(weight = df$WT, height = df$HT) %>% round(3),
    c(28.886, 27.595, 30.502, 26.089),
    ignore_attr = TRUE
  )
})

test_that("bmi can be used in a mutate after a group_by", {
  df <- data.frame(
    "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
    "WT" = c(80.56, 80.56, 80.56, 80.56, 71.53, 71.53, 71.53, 71.53),
    "HT" = c(167, 167, 167, 167, 161, 161, 161, 161)
  )

  df <- df %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(
      bmi = bmi(WT, HT)
    )
  expect_equal(
    df$bmi %>% round(3),
    c(28.886, 28.886, 28.886, 28.886, 27.595, 27.595, 27.595, 27.595),
    ignore_attr = TRUE
  )
})

test_that("bmi messages about missing values", {
  expect_message(bmi(NA, 167), "weight contains ")
  expect_message(bmi(80.56, NA), "height contains ")
})

test_that("bmi warns about recycling", {
  weights <- c(60, 70, 80)
  expect_warning(
    bmi(weights, 170),
    "Inputs have different lengths! Please check data."
  )
  expect_warning(
    bmi(70, c(160, 170)),
    "Inputs have different lengths! Please check data."
  )
})
