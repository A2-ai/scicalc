test_that("startup message lists categorical configs under their own section", {
  withr::local_options(
    scicalc.racen_config = c(white = 10, black = 20),
    scicalc.agec_config = data.frame(
      label = c("child", "adult", "senior"), min = c(0, 18, 65), code = c(1, 2, 3)
    )
  )
  all <- cli::ansi_strip(paste(scicalc_config_messages(), collapse = "\n"))
  expect_match(all, "Categorical Configurations")
  expect_match(all, "scicalc.racen_config: white=10, black=20")
  expect_match(all, "0 <= age < 18 -> child, [1]", fixed = TRUE)
  expect_match(all, "18 <= age < 65 -> adult, [2]", fixed = TRUE)
  expect_match(all, "65 <= age -> senior, [3]", fixed = TRUE)
})

test_that("no config lines are shown when the options are unset", {
  withr::local_options(
    scicalc.racen_config = NULL,
    scicalc.agec_config = NULL,
    scicalc.bmic_config = NULL
  )
  expect_length(scicalc_config_messages(), 0)
})
