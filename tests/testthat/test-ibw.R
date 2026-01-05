test_that("ibw works for basic calculations", {
  # Male at 5 feet (152.4 cm) should be 50 kg base weight
  expect_equal(ibw(152.4, 0, 25), 50)

  # Female at 5 feet (152.4 cm) should be 45.5 kg base weight
  expect_equal(ibw(152.4, 1, 30), 45.5)

  # Male at 5'10" (177.8 cm): 50 + 23 kg = 73 kg
  expect_equal(ibw(177.8, 0, 35), 73, tolerance = 0.1)

  # Female at 5'4" (162.56 cm): 45.5 + 9.2 kg = 54.7 kg
  expect_equal(ibw(162.56, 1, 28), 54.7, tolerance = 0.1)

  expect_equal(
    ibw(c(152.4, 152.4, 177.8, 162.56), c(0, 1, 0, 1), c(25, 30, 35, 28)),
    c(50, 45.5, 73, 54.7)
  )
})

test_that("ibw handles allow_ibw_lt_intercept parameter correctly", {
  # Test both parameter settings for short height

  # When allow_ibw_lt_intercept = TRUE (default), IBW can be less than intercept
  short_male_allow <- ibw(141., 0, 25, allow_ibw_lt_intercept = TRUE)
  expect_lt(short_male_allow, 50) # Should be less than base weight

  # When allow_ibw_lt_intercept = FALSE, IBW gets clamped to intercept
  short_male_clamp <- ibw(141., 0, 25, allow_ibw_lt_intercept = FALSE)
  expect_equal(short_male_clamp, 50) # Should equal base weight

  # Results should be different
  expect_false(short_male_allow == short_male_clamp)
})

test_that("ibw can be used in a mutate", {
  df <- data.frame(
    HEIGHT = c(152.4, 162.56, 177.8),
    SEX = c(0, 1, 0),
    AGE = c(25, 30, 35)
  )

  df <- df %>%
    dplyr::mutate(IBW = ibw(HEIGHT, SEX, AGE))

  expect_equal(df$IBW, c(50, 54.7, 73))
})

test_that("ibw fails with non-numeric inputs", {
  expect_error(ibw("not_numeric", 0, 25), "Assertion on 'height' failed")
  expect_error(ibw(170, "not_numeric", 25), "Assertion on 'sexf' failed")
  expect_error(ibw(170, 0, "not_numeric"), "Assertion on 'age' failed")
})

test_that("ibw handles age parameter correctly", {
  # Test normal adult ages work without warnings
  expect_silent(ibw(170, 0, 25))
  expect_silent(ibw(160, 1, 30))

  # Test age exactly at 18
  expect_silent(ibw(170, 0, 18))

  # Test with vector of ages
  expect_silent(ibw(c(170, 160), c(0, 1), c(25, 30)))
})

test_that("ibw warns for pediatric ages", {
  # Test warning for age less than 18
  expect_warning(
    ibw(170, 0, 17),
    "Age contains values less than 18 years. Ideal body weight may not be appropriate for pediatric populations."
  )

  # Test warning with multiple ages, some pediatric
  expect_warning(
    ibw(c(170, 160, 150), c(0, 1, 0), c(17, 25, 16)),
    "Age contains values less than 18 years. Ideal body weight may not be appropriate for pediatric populations."
  )

  # Ensure calculation still works despite warning
  expect_warning(result <- ibw(170, 0, 17))
  expect_equal(result, ibw(170, 0, 25)) # Should give same result as adult
})

test_that("ibw handles missing age values", {
  # Test message for missing age values
  expect_message(
    ibw(170, 0, NA),
    "age contains missing values"
  )

  # Test with mixed missing and valid ages
  expect_message(
    ibw(c(170, 160), c(0, 1), c(25, NA)),
    "age contains missing values"
  )

  # Ensure calculation still works with NA ages
  expect_message(result <- ibw(170, 0, NA))
  expect_equal(result, ibw(170, 0, 25)) # Should give same result
})

test_that("ibw warns about recycling", {
  # Test with different length vectors
  heights <- c(160, 170, 175)
  expect_warning(
    ibw(heights, 0, 30),
    "Inputs have different lengths! Please check data."
  )

  # Test another combination
  expect_warning(
    ibw(170, c(0, 1), 25),
    "Inputs have different lengths! Please check data."
  )
})
