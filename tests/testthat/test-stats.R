############################################
# cv
test_that("cv works for vector data", {
  x = c(1, 2, 1, 1, 2, 1, 3)
  sd = stats::sd(x)
  m = mean(x)
  expect_equal(cv(c(1, 2, 1, 1, 2, 1, 3)), sd / m)
})

test_that("cv handles NA accordingly", {
  x = c(1, 2, 1, 1, 2, 1, 3)
  sd = stats::sd(x)
  m = mean(x)

  expect_warning(
    cv(c(1, 2, 1, 1, 2, 1, 3, NA), na.rm = FALSE)
  )
  expect_message(
    cv(c(1, 2, 1, 1, 2, 1, 3, NA), na.rm = TRUE),
    "Your data contains NA and will be removed."
  )
  expect_equal(
    cv(c(1, 2, 1, 1, 2, 1, 3, NA), na.rm = TRUE),
    sd / m
  )
})

############################################
#geom_mean
test_that("geom_mean works for vector data", {
  expect_equal(geom_mean(c(1, 2, 3, 4, 3, 2, 3)) %>% round(3), 2.380)
})

test_that("geom_mean handles na accordingly", {
  expect_warning(
    geom_mean(c(1, 2, 3, 4, 3, 2, 3, NA), na.rm = FALSE)
  )
  expect_message(
    geom_mean(c(1, 2, 3, 4, 3, 2, 3, NA), na.rm = TRUE),
    "Your data contains NA and will be removed."
  )
  expect_equal(
    geom_mean(c(1, 2, 3, 4, 3, 2, 3, NA), na.rm = TRUE) %>% round(3),
    2.380
  )
})
############################################
# geom_sd
test_that("geom_sd works for vector data", {
  expect_equal(geom_sd(c(1, 2, 3, 4, 3, 2, 3)) %>% round(3), 1.576)
})

test_that("geom_sd handles na accordingly", {
  expect_warning(
    geom_sd(c(1, 2, 3, 4, 3, 2, 3, NA), na.rm = FALSE)
  )
  expect_message(
    geom_sd(c(1, 2, 3, 4, 3, 2, 3, NA), na.rm = TRUE),
    "Your data contains NA and will be removed."
  )
  expect_equal(
    geom_sd(c(1, 2, 3, 4, 3, 2, 3, NA), na.rm = TRUE) %>% round(3),
    1.576
  )
})
############################################
# geom_cv
test_that("geom_sd works for vector data", {
  expect_equal(geom_cv(c(1, 2, 3, 4, 3, 2, 3)) %>% round(3), 0.480)
})

test_that("geom_sd handles na accordingly", {
  expect_warning(
    geom_cv(c(1, 2, 3, 4, 3, 2, 3, NA), na.rm = FALSE)
  )
  expect_message(
    geom_cv(c(1, 2, 3, 4, 3, 2, 3, NA), na.rm = TRUE),
    "Your data contains NA and will be removed."
  )
  expect_equal(
    geom_cv(c(1, 2, 3, 4, 3, 2, 3, NA), na.rm = TRUE) %>% round(3),
    0.480
  )
})
