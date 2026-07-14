test_that("with_units attaches a single unit from the units column", {
  out <- with_units(c(10, 20, 30), c("ng/mL", "ng/mL", "ng/mL"))
  expect_s3_class(out, "units")
  expect_equal(as.character(units(out)), "ng/mL")
  expect_equal(as.numeric(out), c(10, 20, 30))
})

test_that("with_units normalizes IU and micro spellings", {
  expect_equal(as.character(units(with_units(c(15, 20), c("IU/L", "IU/L")))), "U/L")
  expect_equal(
    as.character(units(with_units(c(1, 2), c("\U03BCmol/L", "\U03BCmol/L")))),
    "umol/L"
  )
})

test_that("with_units ignores blank/NA units with a warning", {
  expect_warning(
    out <- with_units(c(10, NA, 30), c("ng/mL", NA, "")),
    "missing/blank unit"
  )
  expect_equal(as.character(units(out)), "ng/mL")
  expect_equal(as.numeric(out), c(10, NA, 30))
})

test_that("with_units errors when the units column has no usable unit", {
  expect_error(
    suppressWarnings(with_units(c(1, 2), c(NA, ""))),
    "no usable unit"
  )
})

test_that("with_units errors when multiple distinct units are present", {
  expect_error(
    with_units(c(1, 2), c("ng/mL", "mg/L")),
    "single unit"
  )
})

test_that("with_units output flows into convert_units_to_spec", {
  df <- data.frame(ID = 1:2)
  df$ODV <- with_units(c(1000, 2000), c("ng/mL", "ng/mL"))
  out <- convert_units_to_map(df, c(ODV = "ug/mL"))
  expect_equal(as.character(units(out$ODV)), "ug/mL")
  expect_equal(as.numeric(out$ODV), c(1, 2))
})
