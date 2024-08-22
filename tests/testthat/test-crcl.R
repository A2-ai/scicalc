test_that("crcl works for single entries", {
  expect_equal(
    crcl(TRUE, 30, 10, 70) %>% round(3),
    9.090
  )
})

test_that("crcl works within mutates", {
  df <- data.frame(
    "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
    "SEX" = c("F", "F", "F", "F", "M", "M", "M", "M"),
    "RACE" = c("WHITE", "WHITE", "WHITE", "WHITE", "BLACK", "BLACK", "BLACK", "BLACK"),
    "AGE" = c(24, 24, 24, 24, 22, 22, 22, 22),
    "CREAT" = c(1, 1, 1, 1, 4, 4, 4, 4),
    "WEIGHT" = c(70, 70, 70, 70, 65, 65, 65, 65)
  )

  df <- df %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(CRCL = crcl(is_female(SEX), AGE, CREAT, WEIGHT))

  expect_equal(
    df$CRCL %>% round(3),
    c(95.861, 95.861, 95.861, 95.861, 26.632, 26.632, 26.632, 26.632)
  )
})

test_that("crcl messages about NA values", {
  expect_message(crcl(NA, 30, 10, 70), "sexf contains")
  expect_message(crcl(TRUE, NA, 10, 70), "age contains")
  expect_message(crcl(TRUE, 30, NA, 70), "creat contains")
  expect_message(crcl(TRUE, 30, 10, NA), "weight contains")
})
