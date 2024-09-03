test_that("read_file_with_hash prints hash for parquet file", {
  hash <- digest::digest(file = "testdata/test_data.parquet")
  expect_output(read_file_with_hash("testdata/test_data.parquet"), paste0("test_data.parquet: ", hash))
})

test_that("read_file_with_hash prints hash for csv file", {
  hash <- digest::digest(file = "testdata/test_data.csv")
  expect_output(read_file_with_hash("testdata/test_data.csv"), paste0("test_data.csv: ", hash))
})

test_that("read_file_with_hash won't work for non-existant files", {
  expect_error(read_file_with_hash("testdata/data_dne.parquet"))
})

test_that("read_file_with_hash warns about non-supported file type", {
  expect_warning(read_file_with_hash("testdata/test_data.txt"))
})

test_that("read_file_with_hash prints for sas file", {
  hash <- digest::digest(file = "testdata/test_data.sas7bdat")
  expect_output(read_file_with_hash("testdata/test_data.sas7bdat"), paste0("test_data.sas7bdat: ", hash))
})

test_that("read_file_with_hash prints for pzxf file", {
  hash <- digest::digest(file = "testdata/test_data.pzfx")
  expect_output(read_file_with_hash("testdata/test_data.pzfx", table = "Data 1"), paste0("test_data.pzfx: ", hash))
})

test_that("read_file_with_hash can replace '.' with NA", {
  df <- read_file_with_hash("testdata/test_data_missing.csv", na = ".")
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

test_that("read_file_with_hash will not replace '.' by default", {
  df <- read_file_with_hash("testdata/test_data_missing.csv")
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
  expect_output(read_file_with_hash("testdata/test_data_missing.csv", show_col_types = FALSE), paste0("test_data_missing.csv: ", hash))
})

test_that("read_file_with_hash can use different algos", {
  hash <- digest::digest(file = "testdata/test_data.parquet", algo = "blake3")
  expect_output(read_file_with_hash("testdata/test_data.parquet", algo = "blake3"),  paste0("test_data.parquet: ", hash))
})
