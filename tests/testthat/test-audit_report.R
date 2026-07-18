test_that("scicalc_audit_report groups events into a readable report", {
  log_file <- withr::local_tempfile(fileext = ".log")
  withr::local_envvar(c(SCICALC_AUDITING = "test", SCICALC_AUDIT_LOG = log_file))
  scicalc_audit_reset(log_file = log_file)

  log_audit_event(
    "ingest", fn = "read_file_with_hash", file = "data/source.csv",
    hash = "input-hash", algo = "blake3"
  )
  log_audit_event(
    "run", fn = "audit_script", phase = "started", script = "analysis/pk.R",
    script_hash = "script-hash", script_type = "r", scicalc_version = "0.0.0",
    r_version = "4.5.0"
  )
  log_audit_event(
    "unit", fn = "with_units", input = "AVAL", from = NA_character_,
    to = "ng/mL", transform = "attach", detail = "PCSTRESU", n = 2
  )
  log_audit_event(
    "unit", fn = "with_units", input = "AVAL", from = NA_character_,
    to = "ng/mL", transform = "attach", detail = "PCSTRESU", n = 3
  )
  log_audit_event(
    "write", fn = "write_file_with_hash", file = "data/final.parquet",
    hash = "output-hash", algo = "blake3"
  )
  log_audit_event(
    "run", fn = "audit_script", phase = "completed", script = "analysis/pk.R",
    script_hash = "script-hash", script_type = "r", scicalc_version = "0.0.0",
    r_version = "4.5.0"
  )

  report <- scicalc_audit_report(log_file = log_file)

  expect_s3_class(report, "scicalc_audit_report")
  expect_equal(report$overview$status, "evidence captured")
  expect_equal(nrow(report$files), 2)
  expect_equal(report$run$phase, "completed")
  expect_equal(nrow(report$transformations), 1)
  expect_equal(report$transformations$n, 5)
  expect_equal(nrow(report$findings), 0)
  expect_error(print(report), NA)
})

test_that("scicalc_audit_report flags missing anchors and failed conversions", {
  log_file <- withr::local_tempfile(fileext = ".log")
  withr::local_envvar(c(SCICALC_AUDITING = "test", SCICALC_AUDIT_LOG = log_file))
  scicalc_audit_reset(log_file = log_file)

  log_audit_event(
    "unit", fn = "convert_units_to_spec", input = "ODV", from = "ng/mL",
    to = "kg", transform = "failed", detail = NA_character_, n = 2
  )

  report <- scicalc_audit_report(log_file = log_file)

  expect_equal(report$overview$status, "attention required")
  expect_true(any(report$findings$severity == "error"))
  expect_true(any(report$findings$severity == "warning"))
})
