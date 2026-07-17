test_that("body-composition calculations propagate the missing-value indicator", {
  expect_equal(
    units::drop_units(suppressWarnings(bmi(-999, 170))),
    -999
  )
  expect_equal(
    units::drop_units(suppressWarnings(bsa(70, -999))),
    -999
  )
  expect_equal(
    units::drop_units(suppressWarnings(ibw(-999, 0, 30))),
    -999
  )
  expect_equal(
    units::drop_units(suppressWarnings(aibw(70, 170, -999, 30))),
    -999
  )
})

test_that("renal calculations propagate the missing-value indicator", {
  expect_equal(
    units::drop_units(suppressWarnings(crcl(FALSE, -999, 1, 70))),
    -999
  )
  expect_equal(
    units::drop_units(suppressWarnings(aegfr(-999, 1.8))),
    -999
  )
  expect_equal(
    units::drop_units(suppressWarnings(egfr(TRUE, FALSE, -999, 1))),
    -999
  )
})

test_that("clinical categories propagate the missing-value indicator from every input", {
  expect_equal(suppressWarnings(bmic(25, -999)), -999, ignore_attr = TRUE)
  expect_equal(
    suppressWarnings(hfc(-999, 33, 0.8, 1.2)), -999, ignore_attr = TRUE
  )
})

test_that("conversions propagate the missing-value indicator", {
  expect_equal(
    units::drop_units(suppressWarnings(convert_alb(-999))),
    -999
  )
  expect_equal(
    units::drop_units(suppressWarnings(convert_bili(-999))),
    -999
  )
  expect_equal(
    units::drop_units(suppressWarnings(convert_creat(-999))),
    -999
  )

  molecular_weight <- units::set_units(743, "g/mol", mode = "standard")
  expect_equal(
    units::drop_units(suppressWarnings(
      convert_mass_to_mol(-999, molecular_weight, "nmol/L", mass_units = "ng/mL")
    )),
    -999
  )
})

test_that("mixed-unit conversions propagate the missing-value indicator", {
  mass <- units::mixed_units(c(-999, 1), c("ng/mL", "ug/mL"))
  molecular_weight <- units::set_units(c(743, 149327), "g/mol", mode = "standard")

  out <- suppressWarnings(
    convert_mass_to_mol(mass, molecular_weight, mol_units = "nmol/L")
  )

  expect_equal(units::drop_units(out)[1], -999)
  expect_gt(units::drop_units(out)[2], 0)
})

test_that("a sentinel is preserved through input-unit conversion", {
  weight <- units::set_units(-999, "g", mode = "standard")

  expect_equal(
    units::drop_units(suppressWarnings(bmi(weight, 170))),
    -999
  )
})

test_that("spec unit conversion preserves the missing-value indicator", {
  data <- data.frame(
    value = units::set_units(c(1000, -999), "ng/mL", mode = "standard")
  )

  out <- convert_units_to_map(data, c(value = "ug/mL"))

  expect_equal(units::drop_units(out$value), c(1, -999))
  expect_equal(as.character(units(out$value)), "ug/mL")
})

test_that("elementwise and aggregate utilities honor the missing-value indicator", {
  expect_equal(round_like(-999, sdig = 2), -999)
  expect_equal(cv(c(1, 2, -999), na.rm = TRUE), -999)
  expect_equal(geom_mean(c(1, 2, -999), na.rm = TRUE), -999)

  categorized <- suppressWarnings(categorize(c(1, 2, 3, 4, -999), nbins = 2))
  expect_equal(as.character(categorized)[5], "-999")
})
