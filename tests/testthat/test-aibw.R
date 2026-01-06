test_that("aibw sets units attribute", {
  result <- aibw(70, 170, 0, 25)
  expect_equal(attr(result, "units"), "kg")
})

test_that("aibw works for basic calculations", {
  # Test with realistic clinical data similar to function example
  weights <- c(53, 71, 78, 55, 72, 43)
  heights <- c(160, 170, 175, 165, 180, 150)
  sexes <- c(1, 0, 0, 1, 0, 1) # female, male, male, female, male, female
  ages <- c(18, 27, 34, 33, 29, 30)

  results <- aibw(weights, heights, sexes, ages)

  # Should return vector of same length
  expect_length(results, 6)
  expect_true(all(!is.na(results)))

  # Verify mathematical correctness: AIBW = IBW + 0.4 * (weight - IBW)
  ibw_values <- ibw(heights, sexes, ages)
  expected <- ibw_values + 0.4 * (weights - ibw_values)
  expect_true(all(dplyr::near(results, expected)))
})

test_that("aibw handles allow_tbw_lt_ibw parameter correctly", {
  # Test with underweight patients where parameter makes a difference
  weights <- c(45, 38, 42) # All underweight
  heights <- c(170, 165, 180)
  sexes <- c(0, 1, 0) # male, female, male
  ages <- c(25, 30, 35)

  # Default behavior: allow_tbw_lt_ibw = TRUE (AIBW can be < IBW)
  results_allow <- aibw(weights, heights, sexes, ages, allow_tbw_lt_ibw = TRUE)

  # Conservative behavior: allow_tbw_lt_ibw = FALSE (AIBW cannot be < IBW)
  results_conservative <- aibw(
    weights,
    heights,
    sexes,
    ages,
    allow_tbw_lt_ibw = FALSE
  )

  # Get corresponding IBW values for comparison
  ibw_values <- ibw(heights, sexes, ages)

  # With allow_tbw_lt_ibw = TRUE, AIBW should be less than IBW for underweight patients
  expect_true(all(results_allow < ibw_values))

  # With allow_tbw_lt_ibw = FALSE, AIBW should equal IBW for underweight patients
  expect_equal(results_conservative, ibw_values, ignore_attr = TRUE)

  # Results should be different
  expect_false(all(results_allow == results_conservative))
})

test_that("aibw handles allow_ibw_lt_intercept parameter correctly", {
  # Test with short patients where this parameter affects IBW calculation
  weights <- c(45, 40, 48)
  heights <- c(140, 135, 145) # All below 152.4cm
  sexes <- c(0, 1, 0) # male, female, male
  ages <- c(25, 30, 28)

  # Allow IBW to be less than intercept
  results_allow <- aibw(
    weights,
    heights,
    sexes,
    ages,
    allow_ibw_lt_intercept = TRUE
  )

  # Clamp IBW to intercept
  results_clamp <- aibw(
    weights,
    heights,
    sexes,
    ages,
    allow_ibw_lt_intercept = FALSE
  )

  # Results should be different (underlying IBW calculation differs)
  expect_false(all(results_allow == results_clamp))

  # Verify the difference comes from IBW calculation
  ibw_allow <- ibw(heights, sexes, ages, allow_ibw_lt_intercept = TRUE)
  ibw_clamp <- ibw(heights, sexes, ages, allow_ibw_lt_intercept = FALSE)
  expect_false(all(ibw_allow == ibw_clamp))
})

test_that("aibw can be used in a mutate", {
  # Create realistic clinical dataset
  df <- data.frame(
    PATIENT_ID = 1:8,
    WEIGHT = c(53, 71, 78, 55, 72, 43, 95, 68),
    HEIGHT = c(160, 170, 175, 165, 180, 150, 168, 172),
    SEX = c(1, 0, 0, 1, 0, 1, 0, 1),
    AGE = c(18, 27, 34, 33, 29, 30, 45, 28)
  )

  # Add AIBW using dplyr mutate (real-world usage)
  df <- df %>%
    dplyr::mutate(AIBW = aibw(WEIGHT, HEIGHT, SEX, AGE))

  expect_length(df$AIBW, 8)
  expect_true(all(!is.na(df$AIBW)))

  # Verify a few manual calculations
  expect_equal(df$AIBW[1], aibw(53, 160, 1, 18), ignore_attr = TRUE)
  expect_equal(df$AIBW[7], aibw(95, 168, 0, 45), ignore_attr = TRUE) # Overweight case

  # Test with both parameter variations in mutate
  df_conservative <- df %>%
    dplyr::mutate(
      AIBW_CONSERVATIVE = aibw(
        WEIGHT,
        HEIGHT,
        SEX,
        AGE,
        allow_tbw_lt_ibw = FALSE
      )
    )

  expect_length(df_conservative$AIBW_CONSERVATIVE, 8)
  expect_true(all(!is.na(df_conservative$AIBW_CONSERVATIVE)))
})

test_that("aibw fails with non-numeric inputs", {
  expect_error(aibw("not_numeric", 170, 0, 25), "Assertion on 'weight' failed")
  expect_error(aibw(70, "not_numeric", 0, 25), "Assertion on 'height' failed")
  expect_error(aibw(70, 170, "not_numeric", 25), "Assertion on 'sexf' failed")
  expect_error(aibw(70, 170, 0, "not_numeric"), "Assertion on 'age' failed")
})

test_that("aibw handles missing weight values", {
  weights <- c(70, NA, 80)
  heights <- c(170, 160, 180)
  sexes <- c(0, 1, 0)
  ages <- c(25, 30, 35)

  # Test message for missing weight values
  expect_message(
    aibw(weights, heights, sexes, ages),
    "weight contains missing values."
  )

  # Ensure calculation continues with NA weights
  expect_message(results <- aibw(weights, heights, sexes, ages))
  expect_length(results, 3)
  expect_true(is.na(results[2])) # NA weight should give NA result
  expect_false(is.na(results[1])) # Valid weights should give valid results
  expect_false(is.na(results[3]))
})

test_that("aibw handles missing values in other parameters", {
  # Should inherit missing value handling from ibw function
  weights <- c(70, 65, 75)

  expect_message(
    aibw(weights, c(170, NA, 180), c(0, 1, 0), c(25, 30, 35)),
    "height contains missing values"
  )
  expect_message(
    aibw(weights, c(170, 160, 180), c(0, NA, 0), c(25, 30, 35)),
    "sexf contains missing values"
  )
  expect_message(
    aibw(weights, c(170, 160, 180), c(0, 1, 0), c(25, NA, 35)),
    "age contains missing values"
  )
})

test_that("aibw warns for pediatric ages", {
  # Should inherit age warnings from ibw function
  weights <- c(45, 50, 48)
  heights <- c(160, 170, 165)
  sexes <- c(1, 0, 1)
  ages <- c(17, 16, 15) # All pediatric

  expect_warning(
    aibw(weights, heights, sexes, ages),
    "Age contains values less than 18 years. Ideal body weight may not be appropriate for pediatric populations."
  )

  # Mixed ages with some pediatric
  ages_mixed <- c(25, 16, 30)
  expect_warning(
    aibw(weights, heights, sexes, ages_mixed),
    "Age contains values less than 18 years. Ideal body weight may not be appropriate for pediatric populations."
  )
})

test_that("aibw warns about recycling", {
  w <- c(60, 80, 100)
  # Shorter height ⇒ will recycle if no checks exist
  expect_warning(aibw(w, 170, 0, 30))
})
