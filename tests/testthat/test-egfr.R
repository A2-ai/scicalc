#(sexf, raceb, age, creat, cystc, height, method = "ckdepi 2009"

test_that("egfr with ckdepi 2009 method works and can be used in a mutate", {
  df <- data.frame(
    "SEXN" = c(FALSE, TRUE, FALSE, TRUE),
    "RACEN" = c(FALSE, FALSE, TRUE, FALSE),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1)
  )
  df <- df %>%
    dplyr::mutate(ckdepi_2009_egfr = egfr(SEXN, RACEN, AGE, CREAT, method = "CKDEPI 2009"))

  expect_equal(df$ckdepi_2009_egfr %>% round(3), c(104.877, 78.790, 52.950, 78.790))
})

test_that("egfr with MDRD method works and can be used in a mutate", {
  df <- data.frame(
    "SEXN" = c(FALSE, TRUE, FALSE, TRUE),
    "RACEN" = c(FALSE, FALSE, TRUE, FALSE),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1)
  )
  df <- df %>%
    dplyr::mutate(mdrd_egfr = egfr(sexf = SEXN, raceb = RACEN, age = AGE, creat = CREAT, method = "MDRD"))

  expect_equal(df$mdrd_egfr %>% round(3), c(91.803, 68.118, 50.434, 68.118))
})

test_that("egfr with ckdepi_2021_egfr can be used in a mutate", {
  df <- data.frame(
    "SEXN" = c(FALSE, TRUE, FALSE, TRUE),
    "RACEN" = c(FALSE, FALSE, TRUE, FALSE),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1),
    "CYSTC" = c(0.4, 0.8, 1, 2)
  )
  df <- df %>%
    dplyr::mutate(ckdepi_2021_egfr = egfr(sexf = SEXN, age = AGE, creat = CREAT, cystc = CYSTC, method = "CKDEPI 2021"))

  expect_equal(df$ckdepi_2021_egfr %>% round(3), c(145.193, 97.491, 67.182, 47.793))
})

test_that("egfr with schwartz_egfr can be used in a mutate", {
  df <- data.frame(
    "SEXN" = c(FALSE, TRUE, FALSE, TRUE),
    "RACEN" = c(FALSE, FALSE, TRUE, FALSE),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1),
    "CYSTC" = c(0.4, 0.8, 1, 2),
    "HEIGHT" = c(174, 186, 201, 193)
  )
  df <- df %>%
    dplyr::mutate(schwartz_egfr = egfr(sexf = SEXN, raceb = RACEN, age = AGE, creat = CREAT, cystc = CYSTC, height = HEIGHT, method = "Schwartz"))

  expect_equal(df$schwartz_egfr %>% round(3), c(71.862, 76.818, 41.506, 79.709))
})


test_that("egfr messages about <18 being used in non schwartz methods", {
  df <- data.frame(
    "SEXN" = c(FALSE, TRUE, FALSE, TRUE),
    "RACEN" = c(FALSE, FALSE, TRUE, FALSE),
    "AGE" = c(24, 17, 23, 24),
    "CREAT" = c(1, 1, 2, 1),
    "CYSTC" = c(0.4, 0.8, 1, 2),
    "HEIGHT" = c(174, 186, 201, 193)
  )
  expect_message(egfr(sexf = df$SEXN, raceb = df$RACEN, age = df$AGE, creat = df$CREAT, cystc = df$CYSTC, height = df$HEIGHT, method = "ckdepi 2009"), "Ages less than 18 years")
  expect_message(egfr(sexf = df$SEXN, raceb = df$RACEN, age = df$AGE, creat = df$CREAT, cystc = df$CYSTC, height = df$HEIGHT, method = "ckdepi 2021"), "Ages less than 18 years")
  expect_message(egfr(sexf = df$SEXN, raceb = df$RACEN, age = df$AGE, creat = df$CREAT, cystc = df$CYSTC, height = df$HEIGHT, method = "mdrd"), "Ages less than 18 years")
})
