make_long <- function() {
  data.frame(
    ID = c(1, 1, 2, 2),
    TEST = c("ALT", "CREAT", "ALT", "CREAT"),
    VAL = c(20, 1.1, 25, 0.9),
    UNIT = c("U/L", "mg/dL", "U/L", "mg/dL"),
    stringsAsFactors = FALSE
  )
}

test_that("pivot_with_units spreads values and attaches units", {
  wide <- pivot_with_units(
    make_long(),
    values_from = VAL,
    units_from = UNIT,
    names_from = TEST
  )

  expect_setequal(colnames(wide), c("ID", "ALT", "CREAT"))
  expect_equal(as.character(units(wide$ALT)), "U/L")
  expect_equal(as.character(units(wide$CREAT)), "mg/dL")
  expect_equal(as.numeric(wide$ALT), c(20, 25))
  expect_equal(as.numeric(wide$CREAT), c(1.1, 0.9))
})

test_that("pivot_with_units matches plain pivot_wider on values", {
  long <- make_long()
  wide <- pivot_with_units(long, VAL, UNIT, TEST)
  plain <- tidyr::pivot_wider(
    dplyr::select(long, -UNIT),
    values_from = VAL,
    names_from = TEST
  )
  expect_equal(as.numeric(wide$ALT), plain$ALT)
  expect_equal(as.numeric(wide$CREAT), plain$CREAT)
})

test_that("pivot_with_units normalizes IU and micro before attaching", {
  long <- data.frame(
    ID = c(1, 2),
    TEST = c("ALT", "ALT"),
    VAL = c(20, 25),
    UNIT = c("IU/L", "IU/L"),
    stringsAsFactors = FALSE
  )
  wide <- pivot_with_units(long, VAL, UNIT, TEST)
  expect_equal(as.character(units(wide$ALT)), "U/L")
})

test_that("pivot_with_units errors when a test maps to multiple units", {
  long <- data.frame(
    ID = c(1, 2),
    TEST = c("CREAT", "CREAT"),
    VAL = c(1.1, 90),
    UNIT = c("mg/dL", "umol/L"),
    stringsAsFactors = FALSE
  )
  expect_error(
    pivot_with_units(long, VAL, UNIT, TEST),
    "CREAT"
  )
})

test_that("pivot_with_units leaves columns with missing units unitless", {
  long <- data.frame(
    ID = c(1, 2),
    TEST = c("SCORE", "SCORE"),
    VAL = c(5, 7),
    UNIT = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  wide <- pivot_with_units(long, VAL, UNIT, TEST)
  expect_false(inherits(wide$SCORE, "units"))
  expect_equal(wide$SCORE, c(5, 7))
})

test_that("pivot_with_units carries the value column's label onto pivoted columns", {
  long <- make_long()
  long$VAL <- structure(long$VAL, label = "Standard Result")

  wide <- pivot_with_units(long, VAL, UNIT, TEST)
  expect_equal(attr(wide$ALT, "label"), "Standard Result")
  expect_equal(attr(wide$CREAT, "label"), "Standard Result")
  # units still attached
  expect_equal(as.character(units(wide$ALT)), "U/L")
})

test_that("pivot_with_units errors on unparseable units", {
  long <- data.frame(
    ID = c(1, 2),
    TEST = c("X", "X"),
    VAL = c(5, 7),
    UNIT = c("not_a_unit", "not_a_unit"),
    stringsAsFactors = FALSE
  )
  expect_error(pivot_with_units(long, VAL, UNIT, TEST), "X")
})
