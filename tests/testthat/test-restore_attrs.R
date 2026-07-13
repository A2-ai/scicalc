test_that("restore_attrs copies a dropped label", {
  old <- structure(c(1, 2, 3), label = "Age")
  new <- c(1, 2, 3) # label dropped
  out <- restore_attrs(new, old)
  expect_equal(attr(out, "label"), "Age")
})

test_that("restore_attrs is attribute-agnostic", {
  old <- structure(1:3, label = "X", origin = "SDTM", custom = list(a = 1))
  new <- 1:3
  out <- restore_attrs(new, old)
  expect_equal(attr(out, "label"), "X")
  expect_equal(attr(out, "origin"), "SDTM")
  expect_equal(attr(out, "custom"), list(a = 1))
})

test_that("restore_attrs does not clobber units or class of new", {
  old <- structure(c(1, 2), label = "Weight", units = "kg") # old character-units convention
  new <- units::set_units(c(1, 2), "g", mode = "standard")  # real units object
  out <- restore_attrs(new, old)
  expect_s3_class(out, "units")
  expect_equal(as.character(units(out)), "g")   # real units untouched
  expect_equal(attr(out, "label"), "Weight")    # label carried over
})

test_that("restore_attrs keeps new's own attributes when both have one", {
  old <- structure(1:3, label = "old")
  new <- structure(1:3, label = "new")
  out <- restore_attrs(new, old)
  expect_equal(attr(out, "label"), "new")
})

test_that("restore_attrs handles an object with no attributes", {
  expect_equal(restore_attrs(1:3, 4:6), 1:3)
})
