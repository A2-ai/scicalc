test_that("write_parquet_with_hash creates a csv file", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test.parquet"
  expect_equal(!file.exists(path), TRUE)
  .write_parquet_with_hash(df, path)
  expect_equal(file.exists(path), TRUE)
  unlink(path, recursive = TRUE)
})

test_that("write_parquet_with_hash prints a hash", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test.parquet"
  .write_parquet_with_hash(df, path) #Generating file to digest it for hash to test output
  md5_hash <- digest::digest(file = path)
  blake3_hash <- digest::digest(file = path, algo = "blake3")

  # blake3 is the default, matching the read functions
  expect_output(
    .write_parquet_with_hash(df, path),
    paste0("test.parquet: ", blake3_hash)
  )
  unlink(path, recursive = TRUE)
  expect_output(
    .write_parquet_with_hash(df, path, algo = "md5"),
    paste0("test.parquet: ", md5_hash)
  )
  unlink(path, recursive = TRUE)
})

test_that("write_parquet_with_hash forwards write_parquet arguments", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test.parquet"
  # a valid compression value is forwarded and works
  .write_parquet_with_hash(df, path, compression = "uncompressed")
  expect_true(file.exists(path))
  unlink(path, recursive = TRUE)
  # an invalid value errors, proving the argument reaches write_parquet
  expect_error(.write_parquet_with_hash(df, path, compression = "bogus"))
  unlink(path, recursive = TRUE)
})

test_that("write_parquet_with_hash fails for wrong file type", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test.csv"
  expect_error(.write_parquet_with_hash(df, path))
})
