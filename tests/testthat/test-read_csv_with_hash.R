test_that("read_csv_with_hash prints hash", {
  expect_output(read_csv_with_hash("testdata/test_data.csv"), "test_data.csv: 682d81e77c35093aaacf8a181740f89a")
})

test_that('read_csv_with_hash gives data', {
  df <- read_csv_with_hash("testdata/test_data.csv")
  expected_df <- data.frame(
    "SEX" = c("FEMALE", "FEMALE", "MALE", "MALE"),
    "RACE" = c("ASIAN", "BLACK", "WHITE", "OTHER"),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1),
    "CYSTC" = c(0.4, 0.8, 1, 2),
    "HEIGHT" = c(174, 186, 201, 193)
  )
  expect_equal(df %>% as.data.frame(), expected_df)
})

test_that("read_csv_with_hash fails for non csv file", {
  expect_error(read_csv_with_hash("testdata/test_data.parquet"))
})

test_that("read_csv_with_hash can replace '.' with NA", {
  df <- read_csv_with_hash("testdata/test_data_missing.csv", na = ".")
  expected_df <- data.frame(
    "SEX" = c("FEMALE", "FEMALE", "MALE", "MALE"),
    "RACE" = c("ASIAN", "BLACK", "WHITE", "OTHER"),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1),
    "CYSTC" = c(0.4, 0.8, 1, 2),
    "HEIGHT" = c(NA, 186, 201, 193)
  )
  expect_equal(df %>% as.data.frame(), expected_df)
})

test_that("read_csv_with_hash will not replace '.' by default", {
  df <- read_csv_with_hash("testdata/test_data_missing.csv")
  expected_df <- data.frame(
    "SEX" = c("FEMALE", "FEMALE", "MALE", "MALE"),
    "RACE" = c("ASIAN", "BLACK", "WHITE", "OTHER"),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1),
    "CYSTC" = c(0.4, 0.8, 1, 2),
    "HEIGHT" = c('.', 186, 201, 193)
  )
  expect_equal(df %>% as.data.frame(), expected_df)
})

test_that("read_csv_with_hash can hide column types", {
  expect_output(read_csv_with_hash("testdata/test_data_missing.csv", show_col_types = FALSE), "test_data_missing.csv: 7366d2af6972e7bbda399c8fcbc2760b")
})
