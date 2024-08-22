test_that("brfc correctly calculates categories", {
  expect_equal(brfc(0), 3)
  expect_equal(brfc(30), 2)
  expect_equal(brfc(60), 1)
  expect_equal(brfc(90), 0)
  expect_equal(brfc(100), 0)
  expect_equal(brfc(NA), -999)
})

test_that("brfc gives message about NA", {
  expect_message(brfc(NA), "creatinine clearance input has missing values")
})

test_that("brfc works within piped functions", {
  df <- data.frame(
    "ID" = c(1, 1, 1, 1, 2, 2, 2, 2),
    "SEX" = c("F", "F", "F", "F", "M", "M", "M", "M"),
    "RACE" = c("WHITE", "WHITE", "WHITE", "WHITE", "BLACK", "BLACK", "BLACK", "BLACK"),
    "AGE" = c(24, 24, 24, 24, 22, 22, 22, 22),
    "CREAT" = c(1, 1, 1, 1, 4, 4, 4, 4),
    "WEIGHT" = c(70, 70, 70, 70, 65, 65, 65, 65)
  )

  df <- df %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(
      CRCL = crcl(is_female(SEX), AGE, CREAT, WEIGHT),
      BRFC = brfc(CRCL)
    )

  expect_equal(df$BRFC, c(0, 0, 0, 0, 3, 3, 3, 3))

})
