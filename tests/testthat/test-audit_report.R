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
    "spec", fn = "convert_units_to_spec", spec_file = "data/pk.yml",
    spec_hash = "spec-hash"
  )
  log_audit_event(
    "spec", fn = "convert_units_to_spec", spec_file = "data/pk.yml",
    spec_hash = "spec-hash"
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
  expect_equal(nrow(report$files), 3)
  expect_equal(sum(report$files$role == "specification"), 1)
  expect_equal(report$run$phase, "completed")
  expect_equal(nrow(report$transformations), 1)
  expect_equal(report$transformations$n, 5)
  expect_equal(report$transformations$evidence, "source-recorded")
  expect_equal(report$evidence$values, 5)
  expect_equal(nrow(report$findings), 0)
  expect_error(print(report), NA)
})

test_that("audit report omits function-specific simple conversion factors", {
  row <- tibble::tibble(
    input = "ALB", fn = "convert_alb", transform = "convert",
    from = "g/L", to = "g/dL", detail = "x0.1", n = 219
  )

  expect_equal(
    audit_report_transformation_text(row),
    "ALB: g/L → g/dL via convert_alb() (219 values)"
  )
})

test_that("audit report never prints serialized mixed-unit inputs", {
  row <- tibble::tibble(
    input = "structure(list(...), class = c('mixed_units', 'list'))",
    fn = "convert_mass_to_mol", transform = "convert",
    from = "ng/mL", to = "nmol/L", detail = "MW=743 g/mol", n = 2
  )

  expect_equal(
    audit_report_transformation_text(row),
    "<unlabelled mixed-units input>: ng/mL → nmol/L [MW=743 g/mol] via convert_mass_to_mol() (2 values)"
  )
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
