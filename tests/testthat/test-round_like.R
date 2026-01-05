test_that("round_like with explicit sdig rounds to specified significant digits", {
  expect_equal(round_like(123.456, sdig = 2), 120)
  expect_equal(round_like(123.456, sdig = 3), 123)
  expect_equal(round_like(123.456, sdig = 4), 123.5)
  expect_equal(round_like(0.001234, sdig = 2), 0.0012)
})

test_that("round_like with ref infers significant digits from reference", {
  expect_equal(round_like(123.456, ref = 12.3), 123)
  expect_equal(round_like(123.456, ref = 12.34), 123.5)
  expect_equal(round_like(123.456, ref = 1.2), 120)
})

test_that("round_like returns unchanged when both sdig and ref are NULL", {
  expect_equal(round_like(123.456), 123.456)
  expect_equal(round_like(0.123456789), 0.123456789)
})

test_that("round_like works with vectors", {
  expect_equal(
    round_like(c(123.456, 789.012), sdig = 2),
    c(120, 790)
  )
  expect_equal(
    round_like(c(123.456, 789.012), ref = 1.23),
    c(123, 789)
  )
})

test_that("round_like handles NA values", {
  expect_equal(round_like(NA_real_, sdig = 2), NA_real_)
  expect_equal(round_like(c(123.456, NA), sdig = 2), c(120, NA))
})

test_that("round_like handles edge cases", {
  expect_equal(round_like(0, sdig = 2), 0)
  expect_equal(round_like(-123.456, sdig = 2), -120)
})
