test_that("read_csv_with_hash prints hash", {
  hash <- digest::digest(file = "testdata/test_data.csv")
  expect_output(read_csv_with_hash("testdata/test_data.csv"), paste0("test_data.csv: ", hash))
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
  hash <- digest::digest(file = "testdata/test_data_missing.csv")
  expect_output(read_csv_with_hash("testdata/test_data_missing.csv", show_col_types = FALSE), paste0("test_data_missing.csv: ", hash))
})

test_that("read_csv_with_hash can use different digest algorithms", {
  expected_df <- data.frame(
    "SEX" = c("FEMALE", "FEMALE", "MALE", "MALE"),
    "RACE" = c("ASIAN", "BLACK", "WHITE", "OTHER"),
    "AGE" = c(24, 24, 23, 24),
    "CREAT" = c(1, 1, 2, 1),
    "CYSTC" = c(0.4, 0.8, 1, 2),
    "HEIGHT" = c(NA, 186, 201, 193)
  )
  df <- read_csv_with_hash("testdata/test_data_missing.csv", na = ".", algo = "blake3")
  hash <- digest::digest(file = "testdata/test_data_missing.csv", algo = "blake3")

  expect_output(read_csv_with_hash("testdata/test_data_missing.csv", na = ".", algo = "blake3"), paste0("test_data_missing.csv: ", hash))
  expect_equal(df %>% as.data.frame(), expected_df)
})
