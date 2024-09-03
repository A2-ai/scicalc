test_that("write_parquet_with_hash creates a csv file", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test.parquet"
  expect_equal(!file.exists(path), TRUE)
  write_parquet_with_hash(df, path)
  expect_equal(file.exists(path), TRUE)
  unlink(path, recursive = TRUE)
})

test_that("write_parquet_with_hash prints a hash", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test.parquet"
  write_parquet_with_hash(df, path) #Generating file to digest it for hash to test output
  md5_hash <- digest::digest(file = path)
  blake3_hash <- digest::digest(file = path, algo = "blake3")

  expect_output(write_parquet_with_hash(df, path), paste0("test.parquet: ", md5_hash))
  unlink(path, recursive = TRUE)
  expect_output(write_parquet_with_hash(df, path, algo = "blake3"), paste0("test.parquet: ", blake3_hash))
  unlink(path, recursive = TRUE)
})

test_that("write_parquet_with_hash fails for wrong file type", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test.csv"
  expect_error(write_parquet_with_hash(df, path))
})
