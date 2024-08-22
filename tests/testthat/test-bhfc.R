test_that("bhfc computes all levels of bhfc", {
  expect_equal(bhfc(15, 33, 0.6, 1.2), 1)
  expect_equal(bhfc(33, 33, 0.6, 1.2), 1)
  expect_equal(bhfc(30, 33, 1.2, 1.2), 1)
  expect_equal(bhfc(33, 33, 1.2, 1.2), 1)
  expect_equal(bhfc(15, 33, 1.5, 1.2), 2)
  expect_equal(bhfc(35, 33, 0.6, 1.2), 2)
  expect_equal(bhfc(10, 33, 1.8, 1.2), 2)
  expect_equal(bhfc(100, 33, 1.8, 1.2), 2)
  expect_equal(bhfc(4, 33, 3.6, 1.2), 3)
  expect_equal(bhfc(4, 33, 3.8, 1.2), 4)
  expect_equal(bhfc(NA, 33, 0.6, 1.2), -999)
})

test_that("bhfc messages about NA values", {
  expect_message(bhfc(NA, 33, 0.6, 1.2), "AST contains")
  expect_message(bhfc(10, NA, 0.6, 1.2), "ULNAST contains")
  expect_message(bhfc(10, 33, NA, 1.2), "BILI contains")
  expect_message(bhfc(10, 33, 0.6, NA), "ULNBILI contains")
})

test_that("bhfc works within dpylr pipes", {
  df <- data.frame(
    "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
    "SEX" = c("F", "F", "F", "F", "M", "M", "M", "M"),
    "RACE" = c("WHITE", "WHITE", "WHITE", "WHITE", "BLACK", "BLACK", "BLACK", "BLACK"),
    "AGE" = c(24, 24, 24, 24, 22, 22, 22, 22),
    "CREAT" = c(1, 1, 1, 1, 4, 4, 4, 4),
    "WEIGHT" = c(70, 70, 70, 70, 65, 65, 65, 65),
    "AST" = c(15, 15, 15, 15, 23, 23, 23, 23),
    "ULNAST" = c(33, 33, 33, 33, 33, 33, 33, 33),
    "BILI" = c(10, 10, 10, 10, 0.4, 0.4, 0.4, 0.4),
    "ULNBILI" = c(1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2)
  )

  df <- df %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(BHFC = bhfc(AST, ULNAST, BILI, ULNBILI))
  expect_equal(df$BHFC, c(4, 4, 4, 4, 1, 1, 1 ,1))
})
