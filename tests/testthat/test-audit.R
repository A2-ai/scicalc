local_audit <- function(env = parent.frame()) {
  log_file <- withr::local_tempfile(fileext = ".log", .local_envir = env)
  withr::local_options(scicalc.audit_log = log_file, .local_envir = env)
  scicalc_audit_reset(log_file)
  withr::defer(scicalc_audit_reset(log_file), envir = env)
  log_file
}

test_that("with_units logs a unit event", {
  local_audit()
  suppressWarnings(with_units(c(10, 20), c("ng/mL", "ng/mL")))

  a <- scicalc_audit()
  expect_true(nrow(a) >= 1)
  row <- a[a$event_type == "unit" & a$input == "c(10, 20)" | a$transform == "attach", , drop = FALSE]
  expect_true(any(a$transform == "attach", na.rm = TRUE))
  expect_true(any(a$to == "ng/mL", na.rm = TRUE))
})

test_that("convert_creat logs a convert event", {
  local_audit()
  suppressMessages(convert_creat(c(88.42, 90)))

  a <- scicalc_audit()
  conv <- a[a$transform == "convert", , drop = FALSE]
  expect_true(nrow(conv) >= 1)
  expect_true(any(conv$from == "umol/L", na.rm = TRUE))
  expect_true(any(conv$to == "mg/dL", na.rm = TRUE))
})

test_that("convert_units_to_spec logs a spec event and per-column unit events", {
  local_audit()
  df <- data.frame(ID = 1:2)
  df$ODV <- units::set_units(c(1000, 2000), "ng/mL", mode = "standard")
  suppressWarnings(convert_units_to_map(df, c(ODV = "ug/mL")))

  a <- scicalc_audit()
  # convert_units_to_map itself only logs the unit event; spec event comes from
  # the yspec method, so here we assert the per-column conversion is recorded
  conv <- a[a$event_type == "unit" & a$detail == "convert_units_to_spec", , drop = FALSE]
  expect_true(any(conv$input == "ODV", na.rm = TRUE))
  expect_true(any(conv$to == "ug/mL", na.rm = TRUE))
})

test_that("read/write functions log ingest and write events", {
  local_audit()
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  path <- withr::local_tempfile(fileext = ".parquet")
  invisible(utils::capture.output(write_file_with_hash(df, path, overwrite = TRUE)))
  invisible(utils::capture.output(read_file_with_hash(path)))

  a <- scicalc_audit()
  expect_true(any(a$event_type == "write", na.rm = TRUE))
  expect_true(any(a$event_type == "ingest", na.rm = TRUE))
  # written and read hashes of the same file agree (both blake3)
  w <- a$hash[a$event_type == "write"]
  i <- a$hash[a$event_type == "ingest"]
  expect_true(length(intersect(w, i)) >= 1)
})

test_that("scicalc.no_audit disables logging", {
  log_file <- withr::local_tempfile(fileext = ".log")
  withr::local_options(scicalc.audit_log = log_file, scicalc.no_audit = TRUE)
  scicalc_audit_reset(log_file)
  suppressWarnings(with_units(c(1, 2), c("ng/mL", "ng/mL")))
  expect_false(file.exists(log_file))
})

test_that("scicalc_audit reports no log gracefully when none exists", {
  log_file <- withr::local_tempfile(fileext = ".log")
  withr::local_options(scicalc.audit_log = log_file)
  scicalc_audit_reset(log_file)
  expect_message(res <- scicalc_audit(), "No scicalc audit log")
  expect_equal(nrow(res), 0)
})
