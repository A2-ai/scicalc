METHOD = "CKDEPI 2009"
test_that("ckdepi_2009_egfr works for numerical input", {
  expect_equal(ckdepi_2009_egfr(sexf = FALSE, raceb = TRUE, age = 24, creat = 1) %>% round(3), 121.552)
})

test_that("ckdepi_2009_egfr works for vector input", {
  expect_equal(
    ckdepi_2009_egfr(c(FALSE, TRUE, FALSE, TRUE), c(FALSE, FALSE, TRUE, FALSE), c(24, 24, 23, 24), c(1, 1, 2, 1) ) %>% round(3),
    c(104.877, 78.790, 52.950, 78.790))
})

test_that("ckdepi_2009_egfr works for dataframe columns", {
  df <- data.frame(
    "SEXN" = c(FALSE, TRUE, FALSE, TRUE),
    "RACEN" = c(FALSE, FALSE, TRUE, FALSE),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1)
  )
  expect_equal(
    ckdepi_2009_egfr(df$SEXN, df$RACEN, df$AGE, df$CREAT) %>% round(3),
    c(104.877, 78.790, 52.950, 78.790))
})

test_that("ckdepi_2009_egfr can be used in a mutate", {
  df <- data.frame(
    "SEXN" = c(FALSE, TRUE, FALSE, TRUE),
    "RACEN" = c(FALSE, FALSE, TRUE, FALSE),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1)
  )
  df <- df %>%
    dplyr::mutate(ckdepi_2009_egfr = ckdepi_2009_egfr(SEXN, RACEN, AGE, CREAT))

  expect_equal(df$ckdepi_2009_egfr %>% round(3), c(104.877, 78.790, 52.950, 78.790))
})

test_that("ckdepi_2009_egfr can be used within mutate after a group_by", {
  df <- data.frame(
    "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
    "SEXN" = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    "RACEN" = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    "AGE" = c(24, 24, 24, 24, 22, 22, 22, 22),
    "CREAT" = c(1, 1, 1, 1, 4, 4, 4, 4)
  )

  df <- df %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(
      ckdepi_2009_egfr = ckdepi_2009_egfr(SEXN, RACEN, AGE, CREAT)
    )
  expect_equal(df$ckdepi_2009_egfr %>% round(3), c(78.790, 78.790, 78.790, 78.790, 23.066, 23.066, 23.066, 23.066))
})

test_that("ckdepi_2009_egfr won't work for character Sex", {
  df <- data.frame(
    "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
    "SEX" = c("MALE", "MALE", "MALE", "MALE", "FEMALE", "FEMALE", "FEMALE", "FEMALE"),
    "RACEN" = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    "AGE" = c(24, 24, 24, 24, 22, 22, 22, 22),
    "CREAT" = c(1, 1, 1, 1, 4, 4, 4, 4)
  )
  expect_error(
    df <- df %>%
      dplyr::group_by(ID) %>%
      dplyr::mutate(
        ckdepi_2009_egfr = ckdepi_2009_egfr(SEX, RACEN, AGE, CREAT) #error here due to non numerical sex
      )
  )
})

test_that("ckdepi_2009_egfr won't work for character Race", {
  df <- data.frame(
    "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
    "SEXN" = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    "RACE" = c("WHITE", "WHITE", "WHITE", "WHITE", "BLACK", "BLACK", "BLACK", "BLACK"),
    "AGE" = c(24, 24, 24, 24, 22, 22, 22, 22),
    "CREAT" = c(1, 1, 1, 1, 4, 4, 4, 4)
  )
  expect_error(
    df <- df %>%
      dplyr::group_by(ID) %>%
      dplyr::mutate(
        ckdepi_2009_egfr = ckdepi_2009_egfr(SEXN, RACE, AGE, CREAT) #error here due to non numerical sex
      )
  )
})

test_that("ckdepi_2009_egfr messages about missing values", {
  expect_message(ckdepi_2009_egfr(NA, TRUE, 24, 1 ), "sexf contains ")
  expect_message(ckdepi_2009_egfr(FALSE, NA, 24, 1 ), "raceb contains ")
  expect_message(ckdepi_2009_egfr(FALSE, TRUE, NA, 1 ), "age contains ")
  expect_message(ckdepi_2009_egfr(FALSE, TRUE, 24, NA ), "creat contains ")
})
