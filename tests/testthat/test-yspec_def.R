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

test_that("racen returns expected value for other", {
  expect_equal(racen("OTHER"), 4)
  expect_equal(racen("other"), 4)
})

test_that("racen returns expected value for default", {
  expect_equal(racen("unknown"), -999)
  expect_equal(racen("random input"), -999)
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
