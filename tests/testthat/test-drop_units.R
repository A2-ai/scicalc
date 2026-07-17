test_that("drop_units drops standard units", {
  x <- units::set_units(c(1, 2), "mg")

  expect_equal(drop_units(x), c(1, 2))
})

test_that("drop_units drops mixed units", {
  x <- units::mixed_units(c(1, 2), c("mg", "ug"))

  expect_equal(drop_units(x), c(1, 2))
})

test_that("drop_units treats NULL mixed-unit elements as missing", {
  x <- structure(
    list(units::set_units(1, "mg"), NULL),
    class = c("mixed_units", "list")
  )

  expect_equal(drop_units(x), c(1, NA_real_))
})

test_that("drop_units works across data-frame columns", {
  x <- tibble::tibble(
    standard = units::set_units(c(1, 2), "mg"),
    mixed = units::mixed_units(c(3, 4), c("ng", "ug")),
    label = c("a", "b")
  )

  out <- drop_units(x)

  expect_s3_class(out, "tbl_df")
  expect_equal(out$standard, c(1, 2))
  expect_equal(out$mixed, c(3, 4))
  expect_equal(out$label, c("a", "b"))
})
