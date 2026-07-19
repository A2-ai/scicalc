# Activate an audit capture to a temp log (mimics what audit_script sets in the
# child process) for the duration of the calling test.
local_capture <- function(env = parent.frame()) {
  log_file <- withr::local_tempfile(fileext = ".log", .local_envir = env)
  withr::local_envvar(
    c(SCICALC_AUDITING = "test", SCICALC_AUDIT_LOG = log_file),
    .local_envir = env
  )
  scicalc_audit_reset(log_file = log_file)
  withr::defer(scicalc_audit_reset(log_file = log_file), envir = env)
  log_file
}

test_that("scicalc_project_root walks up to the nearest marker", {
  root <- withr::local_tempdir()
  # marker at the root, working dir two levels down
  writeLines("x", file.path(root, "proj.Rproj"))
  deep <- file.path(root, "analysis", "pk")
  dir.create(deep, recursive = TRUE)

  expect_equal(
    normalizePath(scicalc_project_root(deep)),
    normalizePath(root)
  )
})

test_that("scicalc_project_root anchors on an existing .scicalc-logs", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, ".scicalc-logs"))
  sub <- file.path(root, "sub")
  dir.create(sub)
  expect_equal(normalizePath(scicalc_project_root(sub)), normalizePath(root))
})

test_that("scicalc_project_root falls back to start when no marker exists", {
  bare <- withr::local_tempdir()
  # a bare temp dir has no markers up to the fs root
  expect_equal(scicalc_project_root(bare), bare)
})

test_that("startup message declares the project root", {
  root <- withr::local_tempdir()
  writeLines("x", file.path(root, "p.Rproj"))
  withr::local_dir(root)
  expect_message(scicalc_options_message(), "scicalc project root")
})

test_that("nothing is logged unless a capture is active", {
  log_file <- withr::local_tempfile(fileext = ".log")
  # path is set, but SCICALC_AUDITING is not -> capture inactive
  withr::local_envvar(c(SCICALC_AUDIT_LOG = log_file))
  scicalc_audit_reset(log_file = log_file)
  suppressWarnings(with_units(c(1, 2), c("ng/mL", "ng/mL")))
  expect_false(file.exists(log_file))
})

test_that("scicalc_audit output is tidied (time first, no level column)", {
  lf <- local_capture()
  suppressWarnings(with_units(c(1, 2), c("ng/mL", "ng/mL")))

  a <- scicalc_audit(log_file = lf)
  expect_false("time" %in% names(a))
  expect_false("level" %in% names(a))
  expect_identical(names(a)[1], "event_type")
  # the function that produced each event is recorded
  expect_true("fn" %in% names(a))
  expect_true(any(a$fn == "with_units", na.rm = TRUE))
})

test_that("with_units and convert_* log while a capture is active", {
  lf <- local_capture()
  suppressWarnings(with_units(c(10, 20), c("ng/mL", "ng/mL")))
  suppressMessages(convert_creat(c(88.42, 90)))

  a <- scicalc_audit(log_file = lf)
  expect_true(any(a$transform == "attach", na.rm = TRUE))
  expect_true(any(a$transform == "convert", na.rm = TRUE))
  expect_true(any(a$to == "mg/dL", na.rm = TRUE))
})

test_that("with_units logs each unit in a mixed_units vector", {
  lf <- local_capture()
  suppressWarnings(with_units(c(1, 2, 3), c("ug/mL", "ng/mL", "ng/mL")))

  a <- scicalc_audit(log_file = lf)
  attached <- a[a$fn == "with_units" & a$transform == "attach", , drop = FALSE]
  expect_setequal(attached$to, c("ug/mL", "ng/mL"))
  expect_equal(attached$n[attached$to == "ug/mL"], 1)
  expect_equal(attached$n[attached$to == "ng/mL"], 2)
  expect_true(all(attached$evidence == "source-recorded"))
})

test_that("mixed mass-to-molar conversion is captured as one audit event", {
  lf <- local_capture()
  mass <- units::mixed_units(c(1, 500), c("ug/mL", "ng/mL"))
  mw <- units::set_units(c(500, 250), "g/mol", mode = "standard")

  convert_mass_to_mol(mass, mw, mol_units = "nmol/L")

  a <- scicalc_audit(log_file = lf)
  event <- a[a$fn == "convert_mass_to_mol", , drop = FALSE]
  expect_equal(nrow(event), 1)
  expect_setequal(strsplit(event$from, ",", fixed = TRUE)[[1]], c("ug/mL", "ng/mL"))
  expect_equal(event$to, "nmol/L")
  expect_equal(event$n, 2)
  expect_equal(event$input, "mass")
  expect_equal(event$evidence, "carried-converted")
})

test_that("convert_units_to_spec logs a spec event with the spec file path", {
  skip_if_not_installed("yspec")
  lf <- local_capture()
  spec <- yspec::ys_help$spec()
  df <- data.frame(WT = units::set_units(c(70, 80), "kg", mode = "standard"))
  suppressWarnings(suppressMessages(convert_units_to_spec(df, spec)))

  a <- scicalc_audit(log_file = lf)
  spec_row <- a[a$event_type == "spec", , drop = FALSE]
  expect_equal(nrow(spec_row), 1)
  expect_true(!is.na(spec_row$spec_hash))
  expect_match(spec_row$spec_file, "\\.yml$")
})

test_that("convert_units_to_spec logs conversions but skips no-ops", {
  lf <- local_capture()
  df <- data.frame(ID = 1:2)
  df$ODV <- units::set_units(c(1000, 2000), "ng/mL", mode = "standard")
  df$AMT <- units::set_units(c(5, 6), "mg", mode = "standard")
  suppressWarnings(convert_units_to_map(df, c(ODV = "ug/mL", AMT = "mg")))

  a <- scicalc_audit(log_file = lf)
  expect_true(any(a$input == "ODV" & a$to == "ug/mL", na.rm = TRUE))
  # AMT mg -> mg is a no-op and must not be logged
  expect_false(any(a$input == "AMT", na.rm = TRUE))
})

test_that("audit_script is a no-op inside an active capture (re-entrancy guard)", {
  local_capture()
  expect_invisible(res <- audit_script("does-not-exist.R"))
  expect_null(res)
})

test_that("audit_script refuses to overwrite an existing log by default", {
  skip_if_not_installed("callr")
  dir <- withr::local_tempdir()
  script <- file.path(dir, "a.R")
  writeLines("invisible(NULL)", script)
  writeLines("{}", file.path(dir, "a.audit.log")) # pre-existing log

  expect_error(audit_script(script, dir = dir), "already exists")
})

test_that("scicalc_audit errors clearly when no log exists", {
  log_file <- withr::local_tempfile(fileext = ".log")
  expect_error(scicalc_audit(log_file = log_file), "No scicalc audit log")
})

test_that("audit_script captures a full assembly run in a subprocess", {
  skip_if_not_installed("callr")
  # only runnable when a child R process can load scicalc (installed, e.g. under
  # R CMD check) -- skipped under devtools::load_all where it is not installed
  # only meaningful when the child process loads a scicalc that has this
  # feature -- i.e. the current code is installed (R CMD check), not load_all
  child_ok <- tryCatch(
    isTRUE(callr::r(
      function() {
        requireNamespace("scicalc", quietly = TRUE) &&
          exists("audit_script", where = asNamespace("scicalc"))
      },
      libpath = .libPaths()
    )),
    error = function(e) FALSE
  )
  skip_if_not(child_ok, "child process cannot load current scicalc")

  dir <- withr::local_tempdir()
  script <- file.path(dir, "assembly.R")
  writeLines(
    c(
      "library(scicalc)",
      "df <- data.frame(PCSTRESN = c(1000, 2000, 500), PCSTRESU = 'ng/mL')",
      "df$ODV <- with_units(df$PCSTRESN, df$PCSTRESU)"
    ),
    script
  )

  a <- audit_script(script, name = "assembly", dir = dir, quiet = TRUE)
  expect_true(file.exists(file.path(dir, "assembly.audit.log")))
  expect_true(any(a$transform == "attach", na.rm = TRUE))
  expect_true(any(a$to == "ng/mL", na.rm = TRUE))
})
