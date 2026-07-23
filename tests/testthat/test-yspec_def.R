# SEXF
test_that("sexf returns expected value for female", {
  expect_equal(sexf("FEMALE"), 1)
  expect_equal(sexf("F"), 1)
  expect_equal(sexf("female"), 1)
  expect_equal(sexf("f"), 1)

  expect_true(sexf("MALE") != 1)
  expect_true(sexf("OTHER") != 1)
})

test_that("sexf returns expected value for male", {
  expect_equal(sexf("MALE"), 0)
  expect_equal(sexf("M"), 0)
  expect_equal(sexf("male"), 0)
  expect_equal(sexf("M"), 0)
  expect_equal(sexf("Other"), 0)

  expect_true(sexf("FEMALE") != 0)
})


# RACEN
test_that("racen returns expected value for white", {
  expect_equal(racen("WHITE"), 1)
  expect_equal(racen("white"), 1)
})

test_that("racen returns expected value for black", {
  expect_equal(racen("BLACK"), 2)
  expect_equal(racen("black"), 2)
  expect_equal(racen("AFRICAN AMERICAN"), 2)
  expect_equal(racen("african american"), 2)
  expect_equal(racen("BLACK OR AFRICAN AMERICAN"), 2)
  expect_equal(racen("black or african american"), 2)
})

test_that("racen returns expected value for asian", {
  expect_equal(racen("ASIAN"), 3)
  expect_equal(racen("asian"), 3)
})

test_that("racen returns expected value for american native", {
  expect_equal(racen("AMERICAN INDIAN OR ALASKA NATIVE"), 4)
  expect_equal(racen("american native"), 4)
  expect_equal(racen("Native American"), 4)
})

test_that("racen returns expected value for pacific islander", {
  expect_equal(racen("NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"), 5)
  expect_equal(racen("pacific islander"), 5)
  expect_equal(racen("Native Hawaiian"), 5)
})

test_that("racen returns expected value for other", {
  expect_equal(racen("OTHER"), 6)
  expect_equal(racen("other"), 6)
})

test_that("racen maps unknown and NA to the missing value", {
  expect_equal(racen("unknown"), -999)
  expect_equal(racen(NA_character_), -999)
})

test_that("racen warns and returns NA for unspecified values", {
  expect_warning(
    result <- racen("random input"),
    "Unspecified race value"
  )
  expect_true(is.na(result))
})

test_that("racen honors scicalc.racen_config overrides and additions", {
  withr::local_options(scicalc.racen_config = c("WHITE" = 2, "JAPANESE" = 7))

  expect_equal(racen("WHITE"), 2)
  expect_equal(racen("JAPANESE"), 7)
  expect_equal(racen("BLACK"), 2)
  # a configured novel category is not flagged as unspecified
  expect_silent(racen("japanese"))
})

test_that("racen rejects an invalid racen_config", {
  withr::local_options(scicalc.racen_config = c(1, 2))
  expect_error(racen("WHITE"))
})


# ETHNICN
test_that("ethnicn returns expected value for hispanic or latino", {
  expect_equal(ethnicn("HISPANIC OR LATINO"), 1)
  expect_equal(ethnicn("hispanic or latino"), 1)
})

test_that("ethnicn returns expected value for hispanic or latino", {
  expect_equal(ethnicn("NOT HISPANIC OR LATINO"), 0)
  expect_equal(ethnicn("not hispanic or latino"), 0)
})

test_that("ethnicn returns expected value for default", {
  expect_equal(ethnicn("unknown"), -999)
  expect_equal(ethnicn("random input"), -999)
})
