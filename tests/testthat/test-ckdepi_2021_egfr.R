#(sexf, age, creat, cystc)
METHOD = "CKDEPI 2021"
test_that("ckdepi_2021_egfr works for numerical input", {
  expect_equal(ckdepi_2021_egfr(sexf = FALSE, age = 24, creat = 1, cystc = 1) %>% round(3), 97.570)
})

test_that("ckdepi_2021_egfr works for vector input", {
  expect_equal(
    ckdepi_2021_egfr(c(FALSE, TRUE, FALSE, TRUE), c(24, 24, 23, 24), c(1, 1, 2, 1), c(0.4, 0.8, 1, 2)) %>% round(3),
    c(145.193, 97.491, 67.182, 47.793))
})

test_that("ckdepi_2021_egfr works for dataframe columns", {
  df <- data.frame(
    "SEXN" = c(FALSE, TRUE, FALSE, TRUE),
    "RACEN" = c(FALSE, FALSE, TRUE, FALSE),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1),
    "CYSTC" = c(0.4, 0.8, 1, 2)
  )
  expect_equal(
    ckdepi_2021_egfr(df$SEXN, df$AGE, df$CREAT, df$CYSTC) %>% round(3),
    c(145.193, 97.491, 67.182, 47.793))
})

test_that("ckdepi_2021_egfr can be used in a mutate", {
  df <- data.frame(
    "SEXN" = c(FALSE, TRUE, FALSE, TRUE),
    "RACEN" = c(FALSE, FALSE, TRUE, FALSE),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1),
    "CYSTC" = c(0.4, 0.8, 1, 2)
  )
  df <- df %>%
    dplyr::mutate(ckdepi_2021_egfr = ckdepi_2021_egfr(SEXN, AGE, CREAT, CYSTC))

  expect_equal(df$ckdepi_2021_egfr %>% round(3), c(145.193, 97.491, 67.182, 47.793))
})

test_that("ckdepi_2021_egfr can be used within mutate after a group_by", {
  df <- data.frame(
    "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
    "SEXN" = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    "RACEN" = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    "AGE" = c(24, 24, 24, 24, 22, 22, 22, 22),
    "CREAT" = c(1, 1, 1, 1, 4, 4, 4, 4),
    "CYSTC" = c(0.4, 0.4, 0.4, 0.4, 0.9, 0.9, 0.9, 0.9)
  )

  df <- df %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(
      ckdepi_2021_egfr = ckdepi_2021_egfr(SEXN, AGE, CREAT, CYSTC)
    )
  expect_equal(df$ckdepi_2021_egfr %>% round(3), c(121.954, 121.954, 121.954, 121.954, 50.210 , 50.210 , 50.210 , 50.210 ))
})

test_that("ckdepi_2021_egfr won't work for character Sex", {
  df <- data.frame(
    "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
    "SEX" = c("MALE", "MALE", "MALE", "MALE", "FEMALE", "FEMALE", "FEMALE", "FEMALE"),
    "RACEN" = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    "AGE" = c(24, 24, 24, 24, 22, 22, 22, 22),
    "CREAT" = c(1, 1, 1, 1, 4, 4, 4, 4),
    "CYSTC" = c(0.4, 0.4, 0.4, 0.4, 0.9, 0.9, 0.9, 0.9)
  )
  expect_error(
    df <- df %>%
      dplyr::group_by(ID) %>%
      dplyr::mutate(
        ckdepi_2021_egfr = ckdepi_2021_egfr(SEX, AGE, CREAT, CYSTC) #error here due to non numerical sex
      )
  )
})

test_that("ckdepi_2021_egfr messages about missing values", {
  expect_message(ckdepi_2021_egfr(NA, 24, 1, 0.9), "sexf contains ")
  expect_message(ckdepi_2021_egfr(FALSE, NA, 1, 0.9), "age contains ")
  expect_message(ckdepi_2021_egfr(FALSE, 24, NA, 0.9), "creat contains ")
  expect_message(ckdepi_2021_egfr(FALSE, 24, 1, NA), "cystc contains ")
})
