test_that("write_csv_with_hash creates a csv file", {
  df <- data.frame(
     "a" = c(1, 2, 3, 4),
     "b" = c("A", "B", "C", "D")
  )
  path <- "test.csv"
  expect_equal(!file.exists(path), TRUE)
  write_csv_with_hash(df, path)
  expect_equal(file.exists(path), TRUE)
  unlink(path, recursive = TRUE)
})

test_that("write_csv_with_hash prints a hash", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test.csv"
  expect_output(write_csv_with_hash(df, path), "test.csv: 0cfd6da55e6c1e198effe1e584c26d79")
  unlink(path, recursive = TRUE)
})

test_that("write_csv_with_hash fails for wrong file type", {
  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c("A", "B", "C", "D")
  )
  path <- "test.parquet"
  expect_error(write_csv_with_hash(df, path))
})
