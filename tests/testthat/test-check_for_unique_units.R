test_that("check_for_unique_units gives false for non-unique units", {
  df <- data.frame(
  PARAM = c(
    "ALB","ALT","AST","CR","TBIL","ALB","CR","TBIL","ALT","AST"),
  UNIT = c(
    "g/L","U/L","U/L","umol/L","umol/L","U/L","μmol/L","μmol/L","IU/L","IU/L")
  )
  expect_equal(check_for_unique_units(df$PARAM, df$UNIT), FALSE)
})

test_that("check_for_unique_units gives True for unique units", {
  df <- data.frame(
    PARAM = c(
      "ALB","ALT","AST","CR","TBIL","ALB","CR","TBIL","ALT","AST"),
    UNIT = c(
      "g/L","U/L","U/L","umol/L","umol/L","g/L","μmol/L","μmol/L","IU/L","IU/L")
  )
  expect_equal(check_for_unique_units(df$PARAM, df$UNIT), TRUE)
})

test_that("check_for_unique_units failes for non character input", {
  df <- data.frame(
    PARAM = c(
      10,1,1.5,1,13,6,2,13,9,1),
    UNIT = c(
      "g/L","U/L","U/L","umol/L","umol/L","g/L","μmol/L","μmol/L","IU/L","IU/L")
  )
  expect_error(check_for_unique_units(df$PARAM, df$UNIT))
})
