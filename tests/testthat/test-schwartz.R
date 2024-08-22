#(height, creat)
METHOD = "Schwartz"
test_that("schwartz_egfr works for numerical input", {
  expect_equal(schwartz_egfr(height = 174, creat = 1) %>% round(3), 71.862)
})

test_that("schwartz_egfr works for vector input", {
  expect_equal(
    schwartz_egfr(c(174, 202, 186, 179), c(0.4, 0.8, 1, 2)) %>% round(3),
    c(179.655, 104.282, 76.818, 36.963))
})

test_that("schwartz_egfr works for dataframe columns", {
  df <- data.frame(
    "SEXN" = c(FALSE, TRUE, FALSE, TRUE),
    "RACEN" = c(FALSE, FALSE, TRUE, FALSE),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1),
    "CYSTC" = c(0.4, 0.8, 1, 2),
    "HEIGHT" = c(174, 202, 186, 179)
  )
  expect_equal(
    schwartz_egfr(df$HEIGHT, df$CREAT) %>% round(3),
    c(71.862, 83.426, 38.409, 73.927))
})

test_that("schwartz_egfr can be used in a mutate", {
  df <- data.frame(
    "SEXN" = c(FALSE, TRUE, FALSE, TRUE),
    "RACEN" = c(FALSE, FALSE, TRUE, FALSE),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1),
    "CYSTC" = c(0.4, 0.8, 1, 2),
    "HEIGHT" = c(174, 202, 186, 179)
  )
  df <- df %>%
    dplyr::mutate(schwartz_egfr = schwartz_egfr(HEIGHT, CREAT))

  expect_equal(df$schwartz_egfr %>% round(3), c(71.862, 83.426, 38.409, 73.927))
})

test_that("schwartz_egfr can be used within mutate after a group_by", {
  df <- data.frame(
    "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
    "SEXN" = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    "RACEN" = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    "AGE" = c(24, 24, 24, 24, 22, 22, 22, 22),
    "CREAT" = c(1, 1, 1, 1, 4, 4, 4, 4),
    "CYSTC" = c(0.4, 0.4, 0.4, 0.4, 0.9, 0.9, 0.9, 0.9),
    "HEIGHT" = c(174 ,174, 174, 174, 201, 201, 201, 201)
  )

  df <- df %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(
      schwartz_egfr = schwartz_egfr(HEIGHT, CREAT)
    )
  expect_equal(df$schwartz_egfr %>% round(3), c(71.862, 71.862, 71.862, 71.862, 20.753, 20.753, 20.753, 20.753))
})

test_that("schwartz_egfr messages about missing values", {
  expect_message(schwartz_egfr(NA, 0.9), "height contains ")
  expect_message(schwartz_egfr(174, NA), "creat contains ")
})
