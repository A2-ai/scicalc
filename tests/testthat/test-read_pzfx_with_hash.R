test_that('read_pzfx_with_hash gives data for pzfx file', {
  df <- read_pzfx_with_hash("testdata/test_data.pzfx", "Data 1")
  expected_df <- data.frame(
    ID = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
    AGE = c(24, 24, 24, 24, 22, 22, 22, 22, 35, 35, 35, 35),
    CREAT = c(1, 1, 1, 1, 4, 4, 4, 4, 3, 3, 3, 3),
    CYSTC = c(0.4, 0.4, 0.4, 0.4, 0.9, 0.9, 0.9, 0.9, 0.7, 0.7, 0.7, 0.7),
    AST = c(15, 15, 15, 15, 29, 29, 29, 29, 38, 38, 38, 38),
    ULNAST = c(33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33),
    BILI = c(0.8, 0.8, 0.8, 0.8, 1.9, 1.9, 1.9, 1.9, 1.1, 1.1, 1.1, 1.1),
    ULNBILI = c(1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2),
    HEIGHT = c(174, 174, 174, 174, 201, 201, 201, 201, 190, 190, 190, 190),
    WEIGHT = c(70, 70, 70, 70, 80, 80, 80, 80, 75, 75, 75, 75),
    DV = c(10, 150, 70, 9, 7, 140, 60, 7, 11, 156, 70, 7),
    NTLD = c(0.00, 0.25, 1.00, 8.00, 0.00, 0.25, 1.00, 8.00, 0.00, 0.25, 1.00, 8.00)
  )
  expect_equal(df %>% as.data.frame(), expected_df)
})

test_that("read_pzfx_with_hash errors for unsupplied table", {
  expect_error(read_pzfx_with_hash("testdata/test_data.pzfx"))
})
