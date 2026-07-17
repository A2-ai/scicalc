test_that("format_mixed_fast formats magnitudes with row-level units", {
  mixed <- units::mixed_units(c(0.158, 2.24), c("ng/mL", "ug/mL"))

  expect_equal(
    format_mixed_fast(mixed),
    c("0.158 [ng/mL]", "2.24 [ug/mL]")
  )
})

test_that("scicalc_view converts only mixed-units columns in a display copy", {
  mixed <- units::mixed_units(c(0.158, 2.24), c("ng/mL", "ug/mL"))
  source <- tibble::tibble(id = 1:2, ODV = mixed, flag = c(TRUE, FALSE))

  display <- scicalc_view(source)

  expect_s3_class(display, "tbl_df")
  expect_type(display$ODV, "character")
  expect_equal(display$ODV, format_mixed_fast(mixed))
  expect_equal(display$id, source$id)
  expect_equal(display$flag, source$flag)
  expect_s3_class(source$ODV, "mixed_units")
})

test_that("scicalc_view rejects non-data-frame input", {
  expect_error(scicalc_view(1:3), "is.data.frame")
})
