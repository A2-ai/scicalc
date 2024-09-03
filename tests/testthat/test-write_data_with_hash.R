test_that("write_file_with_hash creates csv file", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test.csv"
  expect_equal(!file.exists(path), TRUE)
  write_file_with_hash(df, path)
  expect_equal(file.exists(path), TRUE)
  unlink(path, recursive = TRUE)
})

test_that("write_file_with_hash creates parquet file", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test.parquet"
  expect_equal(!file.exists(path), TRUE)
  write_file_with_hash(df, path)
  expect_equal(file.exists(path), TRUE)
  unlink(path, recursive = TRUE)
})

test_that("write_file_with_hash won't overwrite unless specified", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test.parquet"
  write_file_with_hash(df, path)
  expect_equal(file.exists(path), TRUE)
  expect_error(write_file_with_hash(df, path))
  unlink(path, recursive = TRUE)
})

test_that("write_file_with_hash won't overwrite unless specified", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test.parquet"
  write_file_with_hash(df, path)
  expect_equal(file.exists(path), TRUE)
  expect_no_condition(write_file_with_hash(df, path, overwrite = TRUE))
  unlink(path, recursive = TRUE)
})

test_that("write_file_with_hash will create a dir if it doesn't exist", {
  path <- "export/test.parquet"
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  expect_equal(dir.exists(dirname(path)), FALSE)
  expect_equal(file.exists(path), FALSE)

  write_file_with_hash(df, path)

  expect_equal(dir.exists(dirname(path)), TRUE)
  expect_equal(file.exists(path), TRUE)

  unlink(dirname(path), recursive = TRUE)
})

test_that("write_file_with_hash will warn about non supported file types", {
  path <- "export/test.test"
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  expect_warning(write_file_with_hash(df, path))
})

test_that("write_file_with_hash can use different digest algorithms", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test_2.parquet"
  write_parquet_with_hash(df, path) #Generating file to digest it for hash to test output
  md5_hash <- digest::digest(file = path)
  blake3_hash <- digest::digest(file = path, algo = "blake3")

  expect_output(write_file_with_hash(df, path, overwrite = TRUE), paste0("test_2.parquet: ", md5_hash))
  unlink(path, recursive = TRUE)
  expect_output(write_file_with_hash(df, path, overwrite = TRUE, algo = "blake3"), paste0("test_2.parquet: ", blake3_hash))
  unlink(path, recursive = TRUE)
})
