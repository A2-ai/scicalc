test_that("bsa works for numerical input", {
  expect_equal(bsa(67.2, 173) %>% round(3), 1.801, ignore_attr = TRUE)
  expect_equal(bsa(67.2, 173, method = "Mosteller") %>% round(3), 1.797, ignore_attr = TRUE)
})

test_that("bsa sets units attribute", {
  result <- bsa(67.2, 173)
  expect_equal(attr(result, "units"), "m^2")

  result_mosteller <- bsa(67.2, 173, method = "Mosteller")
  expect_equal(attr(result_mosteller, "units"), "m^2")
})

test_that("bsa works for dataframe columns", {
  df <- data.frame(
    "WT" = c(80.56, 71.53, 81.04, 70.17),
    "HT" = c(167, 161, 163, 164)
  )
  expect_equal(
    bsa(weight = df$WT, height = df$HT) %>% round(3),
    c(1.896, 1.756, 1.868, 1.765),
    ignore_attr = TRUE
  )
  expect_equal(
    bsa(weight = df$WT, height = df$HT, method = "mosteller") %>% round(3),
    c(1.933, 1.789, 1.916, 1.788),
    ignore_attr = TRUE
  )
})

test_that("bsa can be used in a mutate", {
  df <- data.frame(
    "WT" = c(80.56, 71.53, 81.04, 70.17),
    "HT" = c(167, 161, 163, 164)
  )

  df <- df %>%
    dplyr::mutate(
      dubois_bsa = bsa(WT, HT),
      mosteller_bsa = bsa(WT, HT, method = "Mosteller")
    )

  expect_equal(
    df$dubois_bsa %>% round(3),
    c(1.896, 1.756, 1.868, 1.765),
    ignore_attr = TRUE
  )
  expect_equal(
    df$mosteller_bsa %>% round(3),
    c(1.933, 1.789, 1.916, 1.788),
    ignore_attr = TRUE
  )
})

test_that("bsa can be used in a mutate after a group_by", {
  df <- data.frame(
    "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
    "WT" = c(80.56, 80.56, 80.56, 80.56, 71.53, 71.53, 71.53, 71.53),
    "HT" = c(167, 167, 167, 167, 161, 161, 161, 161)
  )

  df <- df %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(
      dubois_bsa = bsa(WT, HT),
      mosteller_bsa = bsa(WT, HT, method = "Mosteller")
    )

  expect_equal(
    df$dubois_bsa %>% round(3),
    c(1.896, 1.896, 1.896, 1.896, 1.756, 1.756, 1.756, 1.756),
    ignore_attr = TRUE
  )
  expect_equal(
    df$mosteller_bsa %>% round(3),
    c(1.933, 1.933, 1.933, 1.933, 1.789, 1.789, 1.789, 1.789),
    ignore_attr = TRUE
  )
})

test_that("bsa messages about missing values", {
  expect_message(bsa(NA, 167), "weight contains ")
  expect_message(bsa(80.56, NA), "height contains ")
  expect_message(bsa(NA, 167, method = "Mosteller"), "weight contains ")
  expect_message(bsa(80.56, NA, method = "Mosteller"), "height contains ")
})

test_that("bsa warns about recycling", {
  weights <- c(60, 70, 80)
  expect_warning(
    bsa(weights, 170),
    "Inputs have different lengths! Please check data."
  )
  expect_warning(
    bsa(70, c(160, 170)),
    "Inputs have different lengths! Please check data."
  )

  # Test with method parameter
  expect_warning(
    bsa(weights, 170, method = "Mosteller"),
    "Inputs have different lengths! Please check data."
  )
})
