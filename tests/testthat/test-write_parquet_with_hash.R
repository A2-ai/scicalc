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
  expect_output(write_parquet_with_hash(df, path), "test.parquet: fd45cca2b6e96f8f58ec492c98de340d")
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
