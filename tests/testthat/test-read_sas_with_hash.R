test_that("read_sas_with_hash prints hash", {
  expect_output(read_sas_with_hash("testdata/test_data.sas7bdat"), "test_data.sas7bdat: b7fc331371df02c2038455c2099f633a")
})

test_that('read_sas_with_hash gives data', {
  df <- read_sas_with_hash("testdata/test_data.sas7bdat")
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

test_that("read_sas_with_hash fails for non sas file", {
  expect_error(read_sas_with_hash("testdata/test_data.csv"))
})
