test_that("bbmi works for numerical input", {
  expect_equal(bbmi(weight = 60, height = 170) %>% round(3), 20.761)
})

test_that("bbmi works for dataframe columns", {
  df <- data.frame(
    "WT" = c(80.56, 71.53, 81.04, 70.17),
    "HT" = c(167, 161, 163, 164)
  )
  expect_equal(
    bbmi(weight = df$WT, height = df$HT) %>% round(3),
    c(28.886, 27.595, 30.502, 26.089)
    )
})

test_that("bbmi can be used in a mutate", {
  df <- data.frame(
    "WT" = c(80.56, 71.53, 81.04, 70.17),
    "HT" = c(167, 161, 163, 164)
  )

  df <- df %>%
    dplyr::mutate(bbmi = bbmi(WT, HT))

  expect_equal(
    bbmi(weight = df$WT, height = df$HT) %>% round(3),
    c(28.886, 27.595, 30.502, 26.089)
  )
})

test_that("bbmi can be used in a mutate after a group_by", {
  df <- data.frame(
    "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
    "WT" = c(80.56, 80.56, 80.56, 80.56, 71.53, 71.53, 71.53, 71.53),
    "HT" = c(167, 167, 167, 167, 161, 161, 161, 161)
  )

  df <- df %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(
      bbmi = bbmi(WT, HT)
    )
  expect_equal(df$bbmi %>% round(3),
               c(28.886, 28.886, 28.886, 28.886,
                 27.595, 27.595, 27.595, 27.595))
})

test_that("bbmi messages about missing values", {
  expect_message(bbmi(NA, 167), "weight contains ")
  expect_message(bbmi(80.56, NA), "height contains ")
})


