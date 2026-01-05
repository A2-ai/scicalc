test_that("cor_df basic structure and sorting", {
  set.seed(123)
  df <- data.frame(
    A = c(1, 2, 3, 4, 5),
    B = c(2, 4, 6, 8, 10), # Perfect + correlation with A
    C = c(5, 4, 3, 2, 1), # Perfect - correlation with A
    D = c(1, 1, 1, 1, 1) # Constant (NA correlations)
  )

  result <- cor_df(df)

  # Structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("name1", "name2", "CORR", "ABSCORR") %in% names(result)))
  expect_true(is.character(result$name1))
  expect_true(is.character(result$name2))
  expect_true(is.numeric(result$CORR))
  expect_true(is.numeric(result$ABSCORR))

  # Self-correlations removed
  expect_false(any(result$name1 == result$name2))

  # ABSCORR = abs(CORR) for non-NA rows; NA-ness matches
  nz <- !is.na(result$CORR)
  expect_true(all(result$ABSCORR[nz] == abs(result$CORR[nz])))
  expect_identical(is.na(result$ABSCORR), is.na(result$CORR))

  # Sorted by absolute correlation (descending) for non-NA rows
  if (any(nz)) {
    expect_equal(
      result$ABSCORR[nz],
      sort(result$ABSCORR[nz], decreasing = TRUE)
    )
  }
})

test_that("cor_df computes correct correlations on known data", {
  df <- data.frame(
    X = c(1, 2, 3, 4, 5),
    Y = c(2, 4, 6, 8, 10), # r = +1 with X
    Z = c(5, 4, 3, 2, 1) # r = -1 with X
  )

  result <- cor_df(df)

  xy_row <- result[result$name1 == "X" & result$name2 == "Y", ]
  xz_row <- result[result$name1 == "X" & result$name2 == "Z", ]

  expect_equal(as.numeric(xy_row$CORR), 1.0, tolerance = 1e-12)
  expect_equal(as.numeric(xz_row$CORR), -1.0, tolerance = 1e-12)
  expect_equal(as.numeric(xy_row$ABSCORR), 1.0, tolerance = 1e-12)
  expect_equal(as.numeric(xz_row$ABSCORR), 1.0, tolerance = 1e-12)
})

test_that("cor_df works with column specification and warns on non-numeric", {
  df <- data.frame(
    A = 1:5,
    B = 2:6,
    C = 3:7,
    D = letters[1:5] # non-numeric
  )

  # Specific columns
  res_ab <- cor_df(df, columns = c("A", "B"))
  expect_equal(nrow(res_ab), 1)
  expect_true(all(sort(c(res_ab$name1, res_ab$name2)) == c("A", "B")))

  # Mixed numeric & non-numeric -> warning + drop non-numeric
  expect_warning(
    res_abd <- cor_df(df, columns = c("A", "B", "D")),
    regexp = "Non-numeric"
  )
  expect_equal(nrow(res_abd), 1)
  expect_true(all(sort(c(res_abd$name1, res_abd$name2)) == c("A", "B")))
})

test_that("cor_df auto-detects numeric columns and returns correct pair count", {
  df <- data.frame(
    num1 = 1:5,
    num2 = 2:6,
    char1 = letters[1:5],
    logical1 = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    num3 = 5:1
  )

  result <- cor_df(df)
  vars <- sort(unique(c(result$name1, result$name2)))
  expect_setequal(vars, c("num1", "num2", "num3"))

  # k choose 2 pairs
  k <- 3L
  expect_equal(nrow(result), k * (k - 1L) / 2L)
})

test_that("cor_df handles different correlation methods (pearson/spearman/kendall)", {
  df <- data.frame(
    X = c(1, 2, 3, 4, 5),
    Y = c(1, 4, 9, 16, 25) # monotone increasing but non-linear
  )

  pearson <- cor_df(df, method = "pearson")
  spearman <- cor_df(df, method = "spearman")
  kendall <- cor_df(df, method = "kendall")

  expect_equal(nrow(pearson), 1)
  expect_equal(nrow(spearman), 1)
  expect_equal(nrow(kendall), 1)

  # Rank-based correlations should be high and >= Pearson here
  expect_true(spearman$CORR >= pearson$CORR - 1e-12)
  # Kendall is usually a bit lower than Spearman on same data, but still high
  expect_true(kendall$CORR > 0.8)
})

test_that("cor_df respects 'use' modes with missing data", {
  df <- data.frame(
    A = c(1, 2, 3, NA, 5),
    B = c(2, 4, 6, 8, 10),
    C = c(1, NA, 3, 4, 5)
  )

  # everything: presence of any NA yields NA correlations
  res_every <- cor_df(df, use = "everything")
  expect_true(all(is.na(res_every$CORR)))

  # complete.obs: rows with any NA are dropped; finite result
  res_complete <- cor_df(df, use = "complete.obs")
  expect_true(all(!is.na(res_complete$CORR)))

  # pairwise.complete.obs: usually more pairs finite than complete.obs
  res_pairwise <- cor_df(df, use = "pairwise.complete.obs")
  expect_true(all(!is.na(res_pairwise$CORR)))

  # all.obs: should error because data contain NA
  expect_error(cor_df(df, use = "all.obs"), regexp = "missing|NA|observ")
})

test_that("cor_df fails appropriately for invalid inputs", {
  # Non-data.frame
  expect_error(cor_df("not a dataframe"), "Assertion on 'data' failed")
  expect_error(cor_df(matrix(1:6, 2, 3)), "Assertion on 'data' failed")

  # Empty data.frame
  expect_error(cor_df(data.frame()), "data cannot be empty")

  # No numeric columns
  df_no_num <- data.frame(x = letters[1:5], y = LETTERS[1:5])
  expect_error(cor_df(df_no_num), "No numeric columns")

  # Columns that don't exist
  df <- data.frame(A = 1:5, B = 2:6)
  expect_error(cor_df(df, columns = c("A", "Z")), "Columns not found.*Z")

  # Fewer than 2 numeric columns requested
  df_one <- data.frame(A = 1:5, B = letters[1:5])
  expect_error(cor_df(df_one, columns = "A"), "At least 2 numeric columns")
})

test_that("cor_df handles edge cases: constant columns and tiny data", {
  df <- data.frame(
    A = c(1, 1, 1, 1, 1), # zero variance
    B = 1:5,
    C = 2:6
  )
  res <- cor_df(df)

  # At least one finite correlation should exist (B-C)
  expect_true(any(!is.na(res$CORR)))

  # Very small dataset
  small <- data.frame(X = c(1, 2), Y = c(3, 4))
  res_small <- cor_df(small)
  expect_equal(nrow(res_small), 1)
  expect_true(!is.na(res_small$CORR))
})

test_that("cor_df preserves lexicographic pair ordering (name1 <= name2)", {
  df <- data.frame(
    Z = 1:5, # lexicographically last
    A = 2:6, # first
    M = 3:7 # middle
  )
  res <- cor_df(df)

  expect_true(all(res$name1 <= res$name2))

  # Each pair appears exactly once
  want <- rbind(
    c("A", "M"),
    c("A", "Z"),
    c("M", "Z")
  )
  got <- cbind(res$name1, res$name2)
  # order rows to compare unordered
  want_ord <- want[order(want[, 1], want[, 2]), , drop = FALSE]
  got_ord <- got[order(got[, 1], got[, 2]), , drop = FALSE]
  expect_equal(got_ord, want_ord)
})

test_that("cor_df returns exactly k*(k-1)/2 pairs for k numeric columns", {
  df <- data.frame(n1 = 1:5, n2 = 2:6, n3 = 3:7, n4 = 4:8)
  res <- cor_df(df)
  k <- 4L
  expect_equal(nrow(res), k * (k - 1L) / 2L)
})
