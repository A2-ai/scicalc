test_that("read_hashed_file works for file with matching hash", {
  file_path <- "testdata/test_data.parquet"
  hash <- digest::digest(file = file_path, algo = "blake3")
  expect_no_condition(read_hashed_file(file_path, hash))
})

test_that("read_hashed_file fails for mismatching hash", {
  hash <- "8cf6b17fcae5b5673a28045a41f622b9" #last digit is different
  file_path <- "testdata/test_data.parquet"
  expect_error(read_hashed_file(file_path, hash))
})

test_that("read_hashed_file fails for unsupported file type", {
  file_path <- "testdata/test_data.txt"
  hash <- digest::digest(file = file_path, algo = "blake3")
  expect_warning(read_hashed_file(file_path, hash))
})

test_that("read_hashed_file reads csvs too", {
  file_path <- "testdata/test_data.csv"
  hash <- digest::digest(file = file_path, algo = "blake3")

  expect_no_error(read_hashed_file(file_path, hash))
})

test_that("read_hashed_file reads sas file", {
  file_path <- "testdata/test_data.sas7bdat"
  hash <- digest::digest(file = file_path, algo = "blake3")

  expect_no_error(read_hashed_file(file_path, hash))
})

test_that("read_hashed_file uses a supplied reader for an unknown extension", {
  file_path <- "testdata/test_data.txt"
  hash <- digest::digest(file = file_path, algo = "blake3")

  df <- read_hashed_file(
    file_path,
    hash,
    reader = function(path, ...) readr::read_csv(path, ...)
  )
  expect_true(nrow(df) > 0)
})

test_that("read_hashed_file warns and ignores reader for a known extension without force", {
  file_path <- "testdata/test_data.csv"
  hash <- digest::digest(file = file_path, algo = "blake3")

  expect_warning(
    df <- read_hashed_file(
      file_path,
      hash,
      reader = function(path, ...) stop("should not be called")
    ),
    "Supplied `reader` ignored"
  )
  expect_true(nrow(df) > 0)
})

test_that("read_hashed_file uses reader for a known extension when forced", {
  file_path <- "testdata/test_data.csv"
  hash <- digest::digest(file = file_path, algo = "blake3")

  called <- FALSE
  df <- read_hashed_file(
    file_path,
    hash,
    reader = function(path, ...) {
      called <<- TRUE
      readr::read_csv(path, ...)
    },
    force = TRUE
  )
  expect_true(called)
  expect_true(nrow(df) > 0)
})

test_that("read_hashed_file does not use reader when the hash doesn't match", {
  file_path <- "testdata/test_data.txt"
  bad_hash <- "8cf6b17fcae5b5673a28045a41f622b9"

  called <- FALSE
  expect_error(
    read_hashed_file(
      file_path,
      bad_hash,
      reader = function(path, ...) {
        called <<- TRUE
      }
    )
  )
  expect_false(called)
})

test_that("read_hashed_file errors when force is TRUE without a reader", {
  file_path <- "testdata/test_data.csv"
  hash <- digest::digest(file = file_path, algo = "blake3")

  expect_error(
    read_hashed_file(file_path, hash, force = TRUE),
    "requires `reader`"
  )
})
