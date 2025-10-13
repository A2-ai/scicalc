test_that("ibw works for basic calculations", {
  # Male at 5 feet (152.4 cm) should be 50 kg base weight
  expect_equal(ibw(152.4, 0), 50)

  # Female at 5 feet (152.4 cm) should be 45.5 kg base weight
  expect_equal(ibw(152.4, 1), 45.5)

  # Male at 5'10" (177.8 cm): 50 + 23 kg = 73 kg
  expect_equal(ibw(177.8, 0), 73, tolerance = 0.1)

  # Female at 5'4" (162.56 cm): 45.5 + 9.2 kg = 54.7 kg
  expect_equal(ibw(162.56, 1), 54.7, tolerance = 0.1)

  expect_equal(
    ibw(c(152.4, 152.4, 177.8, 162.56), c(0, 1, 0, 1)),
    c(50, 45.5, 73, 54.7)
  )
})

test_that("ibw handles intercept_for_short parameter correctly", {
  # Short height without intercept: calculates negative adjustment
  short_male <- ibw(141., 0, intercept_for_short = FALSE)
  expect_lt(short_male, 50) # Should be less than base weight
})

test_that("ibw can be used in a mutate", {
  df <- data.frame(
    HEIGHT = c(152.4, 162.56, 177.8),
    SEX = c(0, 1, 0)
  )

  df <- df %>%
    dplyr::mutate(IBW = ibw(HEIGHT, SEX))

  expect_equal(df$IBW, c(50, 54.7, 73))
})

test_that("ibw fails with non-numeric inputs", {
  expect_error(ibw("not_numeric", 0), "Assertion on 'height' failed")
  expect_error(ibw(170, "not_numeric"), "Assertion on 'sexf' failed")
})
