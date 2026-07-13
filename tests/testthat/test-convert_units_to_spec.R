test_that("worker converts unit-carrying columns to the target unit", {
  df <- data.frame(WT = c(70000, 80000))
  df$WT <- units::set_units(df$WT, "g", mode = "standard")

  out <- .convert_units_to_map(df, c(WT = "kg"))

  expect_equal(as.character(units(out$WT)), "kg")
  expect_equal(as.numeric(out$WT), c(70, 80))
})

test_that("worker attaches units to plain numeric columns with a warning", {
  df <- data.frame(WT = c(70, 80))

  expect_warning(
    out <- .convert_units_to_map(df, c(WT = "kg")),
    "Attached spec units to unitless column\\(s\\): WT \\[kg\\]"
  )
  expect_equal(as.character(units(out$WT)), "kg")
  expect_equal(as.numeric(out$WT), c(70, 80))
})

test_that("worker warns with offenders and leaves failed conversions untouched", {
  df <- data.frame(CREAT = c(1.1, 0.9), WT = c(70000, 80000))
  df$CREAT <- units::set_units(df$CREAT, "mg/dL", mode = "standard")
  df$WT <- units::set_units(df$WT, "g", mode = "standard")

  expect_warning(
    out <- .convert_units_to_map(df, c(CREAT = "hours", WT = "kg")),
    "Could not convert column\\(s\\) to spec units: CREAT \\[mg/dL\\] -> \\[hours\\]"
  )
  # failed column untouched
  expect_equal(as.character(units(out$CREAT)), "mg/dL")
  expect_equal(as.numeric(out$CREAT), c(1.1, 0.9))
  # other column still converted
  expect_equal(as.character(units(out$WT)), "kg")
})

test_that("worker leaves columns not in the map and non-numeric columns untouched", {
  df <- data.frame(ID = c("a", "b"), AGE = c(30, 40), OTHER = c(1, 2))

  expect_warning(
    out <- .convert_units_to_map(df, c(AGE = "years", ID = "kg")),
    "AGE \\[years\\]"
  )
  expect_identical(out$ID, df$ID)
  expect_identical(out$OTHER, df$OTHER)
  expect_equal(as.character(units(out$AGE)), "years")
})

test_that("worker ignores empty and NA spec units", {
  df <- data.frame(A = c(1, 2), B = c(3, 4))
  out <- expect_silent(.convert_units_to_map(df, c(A = "", B = NA_character_)))
  expect_identical(out, df)
})

test_that("convert_units_to_spec dispatches on a yspec object", {
  skip_if_not_installed("yspec")

  spec <- yspec::ys_help$spec()
  df <- data.frame(WT = c(70000, 80000), AGE = c(30, 40))
  df$WT <- units::set_units(df$WT, "g", mode = "standard")

  expect_warning(
    out <- convert_units_to_spec(df, spec),
    "AGE \\[years\\]"
  )
  expect_equal(as.character(units(out$WT)), "kg")
  expect_equal(as.numeric(out$WT), c(70, 80))
  expect_equal(as.character(units(out$AGE)), "years")
})

test_that("convert_units_to_spec errors informatively for unsupported spec classes", {
  df <- data.frame(A = 1)
  expect_error(
    convert_units_to_spec(df, list(A = "kg")),
    "No `convert_units_to_spec\\(\\)` method for class"
  )
})
