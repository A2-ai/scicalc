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

test_that("knit_print renders the report as HTML reusing the report data", {
  skip_if_not_installed("knitr")
  log_file <- withr::local_tempfile(fileext = ".log")
  withr::local_envvar(c(SCICALC_AUDITING = "test", SCICALC_AUDIT_LOG = log_file))
  scicalc_audit_reset(log_file = log_file)

  log_audit_event(
    "run", fn = "audit_script", phase = "completed", script = "analysis/pk.R",
    script_hash = "script-hash", script_type = "r", scicalc_version = "0.0.0",
    r_version = "4.5.0"
  )
  log_audit_event(
    "unit", fn = "with_units", input = "AVAL", from = NA_character_,
    to = "ng/mL", transform = "attach", detail = "PCSTRESU", n = 2
  )

  report <- scicalc_audit_report(log_file = log_file)
  out <- knitr::knit_print(report)

  expect_s3_class(out, "knit_asis")
  text <- paste(as.character(out), collapse = "\n")
  expect_match(text, "<strong>scicalc audit</strong>", fixed = TRUE)
  expect_match(text, "<strong>Status:</strong>", fixed = TRUE)
  expect_match(text, "<pre>", fixed = TRUE)
  # not a captured console dump: no ANSI, no code fence
  expect_no_match(text, "\033", fixed = TRUE)
  expect_no_match(text, "```", fixed = TRUE)
})

test_that("knit_print HTML-escapes unit expressions containing angle brackets", {
  skip_if_not_installed("knitr")
  report <- structure(
    list(
      overview = tibble::tibble(status = "evidence captured"),
      run = tibble::tibble(phase = NA_character_),
      files = tibble::tibble(role = character(), file = character(), hash = character(), algo = character()),
      columns = tibble::tibble(target = "ODV", data_type = "units", has_units = TRUE, unit = "ng mL-1"),
      lineage = audit_empty_lineage(),
      units = list(
        stories = tibble::tibble(
          target = "ODV", kind = "call",
          line = "with_units(case_when(AVALC == \">12.5\" ~ 12.5)) in pct — attached ng/mL",
          refs = NA_character_
        ),
        residual = character()
      ),
      trace = "units",
      transformations = tibble::tibble(),
      findings = tibble::tibble(severity = character(), finding = character(), detail = character())
    ),
    class = "scicalc_audit_report"
  )

  text <- paste(as.character(knitr::knit_print(report)), collapse = "\n")
  expect_match(text, "&gt;12.5", fixed = TRUE)
  expect_no_match(text, "\">12.5", fixed = TRUE)
})

test_that("unit events are attributed to the final columns whose tags ran them", {
  columns <- tibble::tibble(
    target = c("TUM", "AMT", "DUR", "RATE", "ATFD"),
    data_type = "units",
    has_units = TRUE,
    unit = c("mm", "mg", "d", "mg d-1", "d")
  )
  lineage <- tibble::tibble(
    target = c("TUM", "AMT", "DUR", "RATE", "ATFD"),
    relation = "definition",
    object = c("trKey", "ext", "ext", "ext", "o1"),
    symbol = NA_character_,
    expression = c(
      "with_units(AVAL, PARAMU)",
      "with_units(EXDOSE, EXDOSU)",
      "with_units(round(DIFF, 3), \"days\")",
      "AMT/DUR",
      "round(as.numeric(difftime(DTIM, FDOSE, units = \"days\")), 3)"
    ),
    detail = NA_character_, source_object = NA_character_, source_column = NA_character_,
    path = NA_character_, depth = "0", order = as.character(1:5)
  )
  lineage <- dplyr::bind_rows(lineage, tibble::tibble(
    target = NA_character_, relation = "callsite", object = "final",
    symbol = NA_character_, expression = "convert_units_to_spec(o1, spec)",
    detail = "o1", source_object = NA_character_, source_column = NA_character_,
    path = NA_character_, depth = NA_character_, order = NA_character_
  ))
  events <- tibble::tibble(
    event_type = "unit",
    fn = c("with_units", "with_units", "with_units", "convert_units_to_spec", "with_units"),
    input = c("AVAL", "EXDOSE", "round(DIFF, 3)", "ATFD", "helper_internal"),
    from = NA_character_,
    to = c("mm", "mg", "days", "d", "kg"),
    transform = "attach",
    detail = c("PARAMU", "EXDOSU", "\"days\"", NA, NA),
    evidence = c("source-recorded", "source-recorded", "source-recorded", "assumed", "source-recorded"),
    context = c(NA, NA, NA, "o1", NA),
    n = c(189, 874, 874, 5241, 1)
  )
  files <- tibble::tibble(role = "specification", file = "pk.yml", hash = "h", algo = "blake3")

  units <- audit_report_units(events, columns, lineage, files)

  tum <- units$stories[units$stories$target == "TUM", , drop = FALSE]
  expect_equal(tum$kind, "call")
  expect_match(tum$line, "with_units\\(AVAL, PARAMU\\) in trKey")
  expect_match(tum$line, "attached mm from unit column PARAMU")
  expect_match(tum$line, "source-recorded \\(189 values\\)")

  dur <- units$stories[units$stories$target == "DUR", , drop = FALSE]
  expect_match(dur$line, "attached days from literal \"days\"")

  rate <- units$stories[units$stories$target == "RATE", , drop = FALSE]
  expect_equal(rate$kind, "derived")
  expect_match(rate$line, "AMT/DUR in ext")
  expect_match(rate$line, "AMT \\[mg\\] and DUR \\[d\\]")
  expect_equal(rate$refs, "AMT,DUR")

  atfd <- units$stories[units$stories$target == "ATFD", , drop = FALSE]
  expect_equal(atfd$kind, "spec")
  expect_match(atfd$line, "convert_units_to_spec\\(spec\\) in final")
  expect_match(atfd$line, "attached d to unitless numeric")
  expect_match(atfd$line, "from pk.yml")

  expect_length(units$residual, 1)
  expect_match(units$residual, "helper_internal")
})

test_that("identical call texts attribute to occurrences in execution order", {
  columns <- tibble::tibble(
    target = c("A", "B"), data_type = "units", has_units = TRUE, unit = c("mg", "kg")
  )
  lineage <- tibble::tibble(
    target = c("A", "B"),
    relation = "definition",
    object = c("first", "second"),
    symbol = NA_character_,
    expression = "with_units(X, U)",
    detail = NA_character_, source_object = NA_character_, source_column = NA_character_,
    path = NA_character_, depth = "0", order = c("1", "2")
  )
  events <- tibble::tibble(
    event_type = "unit",
    fn = "with_units",
    input = "X",
    from = NA_character_,
    to = c("mg", "kg"),
    transform = "attach",
    detail = "U",
    evidence = "source-recorded",
    n = c(10, 20)
  )
  files <- tibble::tibble(role = character(), file = character(), hash = character(), algo = character())

  units <- audit_report_units(events, columns, lineage, files)

  a <- units$stories[units$stories$target == "A", , drop = FALSE]
  b <- units$stories[units$stories$target == "B", , drop = FALSE]
  expect_match(a$line, "in first")
  expect_match(a$line, "attached mg")
  expect_match(b$line, "in second")
  expect_match(b$line, "attached kg")
})

test_that("spec calls whose result was not assigned are excluded from column evidence", {
  columns <- tibble::tibble(
    target = "NTFD", data_type = "units", has_units = TRUE, unit = "d"
  )
  lineage <- tibble::tibble(
    target = c("NTFD", NA),
    relation = c("definition", "callsite"),
    object = c("pct", "final"),
    symbol = NA_character_,
    expression = c("(CYCLEN - 1) * 3 * 7", "convert_units_to_spec(o2, spec)"),
    detail = c(NA, "o2"),
    source_object = NA_character_, source_column = NA_character_,
    path = NA_character_, depth = c("0", NA), order = c("1", NA)
  )
  events <- tibble::tibble(
    event_type = "unit",
    fn = "convert_units_to_spec",
    input = "NTFD",
    from = NA_character_,
    to = "d",
    transform = "attach",
    detail = NA_character_,
    evidence = "assumed",
    context = c("pct", "o2"),
    n = c(4128, 5002)
  )
  files <- tibble::tibble(role = "specification", file = "pk.yml", hash = "h", algo = "blake3")

  units <- audit_report_units(events, columns, lineage, files)

  ntfd <- units$stories[units$stories$target == "NTFD", , drop = FALSE]
  expect_equal(nrow(ntfd), 1)
  expect_match(ntfd$line, "in final")
  expect_match(ntfd$line, "5,002 values")
  expect_length(units$residual, 1)
  expect_match(units$residual, "whose result was not assigned")
  expect_match(units$residual, "pct")
})

test_that("no-op log reference shifts are not logged", {
  log_file <- withr::local_tempfile(fileext = ".log")
  withr::local_envvar(c(SCICALC_AUDITING = "test", SCICALC_AUDIT_LOG = log_file))
  scicalc_audit_reset(log_file = log_file)

  log_unit_conversion(
    "LDVML", "ln(re 1e-06 m-3.mol)", "ln(re 1e-06 m-3.mol)", "log-shift",
    5, "carried-converted", "input column carried units"
  )
  expect_false(file.exists(log_file))

  log_unit_conversion(
    "LDVML", "ln(re 0.001 m-3.kg)", "ln(re 1e-06 m-3.mol)", "log-shift",
    5, "carried-converted", "input column carried units"
  )
  expect_true(file.exists(log_file))
})

test_that("a spec attach suppresses the derived-by-arithmetic line", {
  columns <- tibble::tibble(
    target = c("NTFD", "NTLD"), data_type = "units", has_units = TRUE, unit = "d"
  )
  lineage <- tibble::tibble(
    target = c("NTFD", "NTLD"),
    relation = "definition",
    object = "pct",
    symbol = NA_character_,
    expression = c("case_when(NTLD != -999 ~ (CYCLEN - 1) * 3 * 7 + NTLD, .default = -999)", "0"),
    detail = NA_character_, source_object = NA_character_, source_column = NA_character_,
    path = NA_character_, depth = "0", order = c("1", "2")
  )
  events <- tibble::tibble(
    event_type = "unit",
    fn = "convert_units_to_spec",
    input = c("NTFD", "NTLD"),
    from = NA_character_,
    to = "d",
    transform = "attach",
    detail = NA_character_,
    evidence = "assumed",
    n = c(9130, 9130)
  )
  files <- tibble::tibble(role = "specification", file = "pk.yml", hash = "h", algo = "blake3")

  units <- audit_report_units(events, columns, lineage, files)

  ntfd <- units$stories[units$stories$target == "NTFD", , drop = FALSE]
  expect_false(any(ntfd$kind == "derived"))
  expect_equal(ntfd$kind, "spec")
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
