test_that("read_parquet_with_hash prints hash", {
  hash <- digest::digest(file = "testdata/test_data.parquet")
  expect_output(
    read_parquet_with_hash("testdata/test_data.parquet"),
    paste0("test_data.parquet: ", hash)
  )
})

test_that('read_parquet_with_hash gives data', {
  df <- read_parquet_with_hash("testdata/test_data.parquet")
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

test_that("read_parquet_with_hash fails for non parquet file", {
  expect_error(read_parquet_with_hash("testdata/test_data.csv"))
})
