test_that("static lineage follows final columns to their input terminals", {
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "trKey <- adtr_parquet %>% left_join(idKey) %>% mutate(PARAMU = PARAM, TUMORBL = with_units(values = AVAL, units = PARAMU)) %>% select(USUBJID, ID, TUMORBL)",
    "covKey <- slKey %>% left_join(lbKey) %>% left_join(ulKey) %>% left_join(trKey)",
    "o2 <- o1 %>% left_join(covKey)",
    "final <- o2 %>% mutate(LINE = row_number()) %>% select(names(spec))"
  ), script)

  lineage <- audit_static_lineage(script, targets = "TUMORBL", target_object = "final")

  definition <- lineage[lineage$relation == "definition", , drop = FALSE]
  expect_equal(definition$object, "trKey")
  expect_equal(definition$expression, "with_units(values = AVAL, units = PARAMU)")
  expect_equal(definition$path, "final -> o2 -> covKey -> trKey")

  aval <- lineage[!is.na(lineage$symbol) & lineage$symbol == "AVAL", , drop = FALSE]
  expect_equal(aval$source_object, "adtr_parquet")
  expect_equal(aval$source_column, "AVAL")

  paramu <- lineage[!is.na(lineage$symbol) & lineage$symbol == "PARAMU", , drop = FALSE]
  expect_equal(paramu$source_object, "adtr_parquet")
  expect_equal(paramu$source_column, "PARAM")
})

test_that("static lineage reports every equal-depth sibling definition", {
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "ext <- adex %>% mutate(NTLD = 0)",
    "pct <- adpc %>% mutate(NTLD = case_when(ATPT == \"Pre-dose\" ~ 0, .default = tpt))",
    "o1 <- ext %>% bind_rows(pct)",
    "final <- o1 %>% select(NTLD)"
  ), script)

  lineage <- audit_static_lineage(script, targets = "NTLD", target_object = "final")

  definitions <- lineage[lineage$relation == "definition", , drop = FALSE]
  expect_setequal(definitions$object, c("ext", "pct"))
  expect_setequal(
    definitions$expression,
    c("0", "case_when(ATPT == \"Pre-dose\" ~ 0, .default = tpt)")
  )
  expect_setequal(
    definitions$path,
    c("final -> o1 -> ext", "final -> o1 -> pct")
  )

  tpt <- lineage[!is.na(lineage$symbol) & lineage$symbol == "tpt", , drop = FALSE]
  expect_equal(tpt$source_object, "adpc")
  expect_equal(tpt$source_column, "tpt")
})

test_that("a closer redefinition still shadows upstream definitions", {
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "ext <- adex %>% mutate(NTLD = 0)",
    "o1 <- ext %>% mutate(NTLD = tpt / 24)",
    "final <- o1 %>% select(NTLD)"
  ), script)

  lineage <- audit_static_lineage(script, targets = "NTLD", target_object = "final")

  definitions <- lineage[lineage$relation == "definition", , drop = FALSE]
  expect_equal(definitions$object, "o1")
  expect_equal(definitions$expression, "tpt/24")
})

test_that("static lineage records dynamic source expressions as terminals", {
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "raw <- source_df",
    "final <- raw %>% mutate(X = raw[[Sys.getenv(\"COLUMN\")]])"
  ), script)

  lineage <- audit_static_lineage(script, targets = "X", target_object = "final")

  expect_true(any(lineage$expression == "Sys.getenv(\"COLUMN\")", na.rm = TRUE))
})
