test_that("worker converts unit-carrying columns to the target unit", {
  df <- data.frame(WT = c(70000, 80000))
  df$WT <- units::set_units(df$WT, "g", mode = "standard")

  out <- convert_units_to_map(df, c(WT = "kg"))

  expect_equal(as.character(units(out$WT)), "kg")
  expect_equal(as.numeric(out$WT), c(70, 80))
})

test_that("worker leaves plain numeric columns untouched (no attach)", {
  df <- data.frame(WT = c(70, 80))

  out <- expect_silent(convert_units_to_map(df, c(WT = "kg")))
  expect_false(inherits(out$WT, "units"))
  expect_identical(out$WT, df$WT)
})

test_that("worker records the spec unit for every mapped column", {
  lf <- withr::local_tempfile(fileext = ".log")
  withr::local_envvar(c(SCICALC_AUDITING = "test", SCICALC_AUDIT_LOG = lf))
  scicalc_audit_reset(log_file = lf)

  df <- data.frame(WT = c(70, 80), AGE = c(30, 40))
  df$WT <- units::set_units(df$WT, "kg", mode = "standard")
  convert_units_to_map(df, c(WT = "kg", AGE = "years"))

  a <- scicalc_audit(log_file = lf)
  spec_units <- a[a$event_type == "spec_unit", , drop = FALSE]
  expect_setequal(spec_units$target, c("WT", "AGE"))
  expect_equal(spec_units$unit[spec_units$target == "AGE"], "years")
})

test_that("worker aborts listing every failed conversion", {
  df <- data.frame(CREAT = c(1.1, 0.9), WT = c(70000, 80000))
  df$CREAT <- units::set_units(df$CREAT, "mg/dL", mode = "standard")
  df$WT <- units::set_units(df$WT, "g", mode = "standard")

  expect_error(
    convert_units_to_map(df, c(CREAT = "hours", WT = "kg")),
    "Could not convert column\\(s\\) to spec units:\\s+CREAT \\[mg/dL\\] -> \\[hours\\]"
  )
})

test_that("worker logs a failed event for each offender before aborting", {
  lf <- withr::local_tempfile(fileext = ".log")
  withr::local_envvar(c(SCICALC_AUDITING = "test", SCICALC_AUDIT_LOG = lf))
  scicalc_audit_reset(log_file = lf)

  df <- data.frame(CREAT = c(1.1, 0.9), BILI = c(0.5, 0.6))
  df$CREAT <- units::set_units(df$CREAT, "mg/dL", mode = "standard")
  df$BILI <- units::set_units(df$BILI, "mg/dL", mode = "standard")

  expect_error(convert_units_to_map(df, c(CREAT = "hours", BILI = "days")))

  a <- scicalc_audit(log_file = lf)
  failed <- a[!is.na(a$transform) & a$transform == "failed", , drop = FALSE]
  expect_setequal(failed$input, c("CREAT", "BILI"))
})

test_that("worker leaves columns not in the map and non-numeric columns untouched", {
  df <- data.frame(ID = c("a", "b"), AGE = c(30, 40), OTHER = c(1, 2))

  out <- convert_units_to_map(df, c(AGE = "years", ID = "kg"))
  expect_identical(out$ID, df$ID)
  expect_identical(out$OTHER, df$OTHER)
  # AGE is plain numeric: left untouched, not attached
  expect_false(inherits(out$AGE, "units"))
  expect_identical(out$AGE, df$AGE)
})

test_that("worker ignores empty and NA spec units", {
  df <- data.frame(A = c(1, 2), B = c(3, 4))
  out <- expect_silent(convert_units_to_map(df, c(A = "", B = NA_character_)))
  expect_identical(out, df)
})

test_that("convert_units_to_spec dispatches on a yspec object", {
  skip_if_not_installed("yspec")

  spec <- yspec::ys_help$spec()
  df <- data.frame(WT = c(70000, 80000), AGE = c(30, 40))
  df$WT <- units::set_units(df$WT, "g", mode = "standard")

  out <- convert_units_to_spec(df, spec)
  expect_equal(as.character(units(out$WT)), "kg")
  expect_equal(as.numeric(out$WT), c(70, 80))
  # AGE is plain numeric: left untouched
  expect_false(inherits(out$AGE, "units"))
})

test_that("worker converts a log column to a new log reference", {
  odv <- units::set_units(c(1000, 2000, 500), "ng/mL", mode = "standard")
  df <- data.frame(row = 1:3)
  df$LDV <- log(odv) # ln(re 1 ng/mL)

  out <- convert_units_to_map(df, c(LDV = "log(ug/mL)"))
  truth <- log(units::set_units(odv, "ug/mL"))

  expect_equal(as.numeric(out$LDV), as.numeric(truth))
  expect_equal(units(out$LDV), units(truth))
})

test_that("worker handles log10 and log2 spec bases", {
  odv <- units::set_units(c(1000, 2000), "ng/mL", mode = "standard")

  df10 <- data.frame(row = 1:2)
  df10$LDV <- log10(odv)
  out10 <- convert_units_to_map(df10, c(LDV = "log10(ug/mL)"))
  expect_equal(as.numeric(out10$LDV), as.numeric(log10(units::set_units(odv, "ug/mL"))))

  df2 <- data.frame(row = 1:2)
  df2$LDV <- log2(odv)
  out2 <- convert_units_to_map(df2, c(LDV = "log2(ug/mL)"))
  expect_equal(as.numeric(out2$LDV), as.numeric(log2(units::set_units(odv, "ug/mL"))))
})

test_that("worker reports a log column against a mismatched base as an offender", {
  odv <- units::set_units(c(1000, 2000), "ng/mL", mode = "standard")
  df <- data.frame(row = 1:2)
  df$LDV <- log(odv) # natural log

  expect_error(
    convert_units_to_map(df, c(LDV = "log10(ug/mL)")),
    "Could not convert"
  )
})

test_that("worker reports a log column against a non-log spec as an offender", {
  odv <- units::set_units(c(1000, 2000), "ng/mL", mode = "standard")
  df <- data.frame(row = 1:2)
  df$LDV <- log(odv)

  expect_error(
    convert_units_to_map(df, c(LDV = "ug/mL")),
    "Could not convert"
  )
})

test_that("worker leaves an already-logged plain numeric column untouched", {
  df <- data.frame(LDV = c(0, 0.6931472))
  out <- expect_silent(convert_units_to_map(df, c(LDV = "log(ug/mL)")))
  expect_false(inherits(out$LDV, "units"))
  expect_identical(out$LDV, df$LDV)
})

test_that("convert_units_to_spec preserves a column label through conversion", {
  df <- data.frame(row = 1:2)
  df$WT <- structure(units::set_units(c(70000, 80000), "g", mode = "standard"), label = "Weight")

  out <- convert_units_to_map(df, c(WT = "kg"))
  expect_equal(attr(out$WT, "label"), "Weight")
  expect_equal(as.character(units(out$WT)), "kg")
  expect_equal(as.numeric(out$WT), c(70, 80))
})


test_that("convert_units_to_spec errors informatively for unsupported spec classes", {
  df <- data.frame(A = 1)
  expect_error(
    convert_units_to_spec(df, list(A = "kg")),
    "No `convert_units_to_spec\\(\\)` method for class"
  )
})
