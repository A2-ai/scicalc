test_that("agec sets category_standard attribute", {
  result <- agec(25)
  expect_equal(attr(result, "category_standard"), "FDA")
})

test_that("agec correctly classifies all age categories using roxygen example", {
  # Use the exact example from the function documentation
  df <- data.frame(
    ID = 1:12,
    AGE = c(0.07, 28 / 365, 0.25, 1, 2, 4, 12, 16, 18, 24, 65, 70)
  )
  expect_warning(
    df <- dplyr::mutate(df, AGEC = agec(AGE)),
    "Neonate ages detected. Confirm assignment."
  )

  # Expected categories:
  # 0.07 (25 days) -> 1 (Neonate)
  # 28/365 (28 days) -> 2 (Infant)
  # 0.25, 1 -> 2 (Infant)
  # 2, 4 -> 3 (Child)
  # 12, 16 -> 4 (Adolescent)
  # 18, 24 -> 5 (Adult)
  # 65, 70 -> 6 (Elder Adult)
  expected_agec <- c(1, 2, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)
  expect_equal(df$AGEC, expected_agec, ignore_attr = TRUE)
})

test_that("agec produces appropriate warnings and messages", {
  # Test NA handling
  expect_message(agec(c(25, NA, 30)), "age contains missing values")

  # Test negative age warning
  expect_warning(agec(c(25, -1, 30)), "age contains values less than 0 years")

  # Test very old age warning
  expect_warning(agec(c(25, 120, 30)), "age contains values > 116 years")

  # Test neonate warning
  expect_warning(agec(0.05), "Neonate ages detected")

  # Test boundary message
  expect_message(agec(28 / 365), "Age near Neonate boundary \\(28 days\\)")
})

test_that("agec works with dplyr operations", {
  # Test with vector input
  ages <- c(0.07, 28 / 365, 2, 16, 25, 70)
  expected <- c(1, 2, 3, 4, 5, 6)

  expect_warning(
    agec(ages),
    "Neonate ages detected. Confirm assignment."
  )
  expect_equal(suppressWarnings(agec(ages)), expected, ignore_attr = TRUE)

  # Test with grouped data
  df <- data.frame(
    ID = c(1, 2, 3, 4),
    AGE = c(25, 30, 15, 70),
    SEX = c("M", "F", "M", "F")
  )

  df <- df %>%
    dplyr::group_by(SEX) %>%
    dplyr::mutate(AGEC = agec(AGE))

  expect_equal(df$AGEC, c(5, 5, 4, 6), ignore_attr = TRUE)
})

test_that("agec handles edge cases and invalid inputs", {
  # Test negative ages return -999
  expect_warning(
    agec(-1),
    "age contains values less than 0 years. Confirm data is correct"
  )
  expect_equal(suppressWarnings(agec(-1)), -999, ignore_attr = TRUE)

  # Test ages > 116 still get categorized correctly if otherwise valid
  expect_warning(
    agec(120),
    "age contains values > 116 years. Confirm data is correct."
  )
  expect_equal(suppressWarnings(agec(120)), 6, ignore_attr = TRUE)

  # Test non-numeric input should error due to checkmate
  expect_error(agec("not_numeric"))
  expect_error(agec(factor(c(1, 2, 3))))

  # Test boundary precision - the key case that was failing
  expect_equal(suppressWarnings(agec(28 / 365)), 2, ignore_attr = TRUE) # Exactly 28 days should be infant
  expect_equal(suppressWarnings(agec(27 / 365)), 1, ignore_attr = TRUE) # 27 days should be neonate
  expect_equal(agec(29 / 365), 2, ignore_attr = TRUE) # 29 days should be infant
})
