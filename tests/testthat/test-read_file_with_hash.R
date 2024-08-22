test_that("read_file_with_hash prints hash for parquet file", {
  expect_output(read_file_with_hash("testdata/test_data.parquet"), "test_data.parquet: 8cf6b17fcae5b5673a28045a41f622b7")
})

test_that("read_file_with_hash prints hash for csv file", {
  expect_output(read_file_with_hash("testdata/test_data.csv"), "test_data.csv: 682d81e77c35093aaacf8a181740f89a")
})

test_that("read_file_with_hash won't work for non-existant files", {
  expect_error(read_file_with_hash("testdata/data_dne.parquet"))
})

test_that("read_file_with_hash warns about non-supported file type", {
  expect_warning(read_file_with_hash("testdata/test_data.txt"))
})

test_that("read_file_with_hash prints for sas file", {
  expect_output(read_file_with_hash("testdata/test_data.sas7bdat"), "test_data.sas7bdat: b7fc331371df02c2038455c2099f633a")
})

test_that("read_file_with_hash prints for pzxf file", {
  expect_output(read_file_with_hash("testdata/test_data.pzfx", table = "Data 1"), "test_data.pzfx: 2277208799b6c53733a1c9b2f871fb75")
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
  expect_output(read_file_with_hash("testdata/test_data_missing.csv", show_col_types = FALSE), "test_data_missing.csv: 7366d2af6972e7bbda399c8fcbc2760b")
})
