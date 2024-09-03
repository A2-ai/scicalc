test_that("read_hashed_file works for file with matching hash", {
  file_path <- "testdata/test_data.parquet"
  hash <- digest::digest(file = file_path)
  expect_no_condition(read_hashed_file(file_path, hash))
})

test_that("read_hashed_file fails for mismatching hash", {
  hash <- "8cf6b17fcae5b5673a28045a41f622b9" #last digit is different
  file_path <- "testdata/test_data.parquet"
  expect_error(read_hashed_file(file_path, hash))
})

test_that("read_hashed_file fails for unsupported file type", {
  file_path <- "testdata/test_data.txt"
  hash <- digest::digest(file = file_path)
  expect_warning(read_hashed_file(file_path, hash))
})

test_that("read_hashed_file reads csvs too", {
  file_path <- "testdata/test_data.csv"
  hash <-  digest::digest(file = file_path)

  expect_no_error(read_hashed_file(file_path, hash))
})

test_that("read_hashed_file reads sas file", {
  file_path <- "testdata/test_data.sas7bdat"
  hash <-  digest::digest(file = file_path)

  expect_no_error(read_hashed_file(file_path, hash))
})
