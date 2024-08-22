test_that("create_dir creates a directory from relative path", {
  create_dir("test_dir")
  expect_equal(dir.exists("test_dir"), TRUE)
  unlink("test_dir", recursive = TRUE)
})

test_that("create_dir creates a directory from absolute path", {
  absolute_path <- file.path(normalizePath(".", mustWork = FALSE), "abs_test_dir")
  create_dir(absolute_path)
  expect_equal(dir.exists(absolute_path), TRUE)
  unlink(absolute_path, recursive = TRUE)
})

test_that("create_dir creates all nonexistant directories along the path", {
  base_dir = file.path("create")
  path <- file.path(base_dir, "dir", "test")
  create_dir(path)
  expect_equal(dir.exists(path), TRUE)
  expect_equal(dir.exists(dirname(path)), TRUE)
  expect_equal(dir.exists(dirname(dirname(path))), TRUE)
  unlink(base_dir, recursive = TRUE)
})
