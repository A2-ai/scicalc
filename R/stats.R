#' Computes the coefficient of variation of input vector.
#'
#' @param x Input vector to compute CV for.
#' @param na.rm boolean to remove NA. default is FALSE
#'
#' @return CV of x. Standard deviation divided by mean. If you want % you'll need to multiply by 100
#' @export
#'
#' @examples
#' cv(c(1, 2, 1, 1, 2, 1, 2, 3))
cv <- function(x, na.rm = FALSE) {
  checkmate::assertNumeric(x)

  if (any(is.na(x))) {
    if (na.rm) {
      message("Your data contains NA and will be removed.")
    } else {
      warning(
        "Your data contains NA. Please make sure na.rm is appropriately set."
      )
    }
  }

  return(stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm))
}


#' Computes the geometric mean of a vector.
#'
#' @param x vector to compute geometric mean of
#' @param na.rm boolean to remove NA from vector in calcualtion. Default is False
#'
#' @return geometric mean of input vector x
#' @export
#'
#' @examples
#' geom_mean(c(1, 2, 3, 2, 1))
geom_mean <- function(x, na.rm = FALSE) {
  checkmate::assertNumeric(x)

  if (any(is.na(x))) {
    if (na.rm) {
      message("Your data contains NA and will be removed.")
    } else {
      warning(
        "Your data contains NA. Please make sure na.rm is appropriately set."
      )
    }
  }

  return(exp(mean(log(x), na.rm = na.rm)))
}

#' Computes the geometric standard deviation of a vector x.
#'
#' @param x The vector of data you want the geometric sd of.
#' @param na.rm a boolean to remove NA values. Default is False
#'
#' @return the geometric standard deviation of x
#' @export
#'
#' @examples
#' geom_sd(c(1, 2, 3, 2, 1))
geom_sd <- function(x, na.rm = FALSE) {
  checkmate::assertNumeric(x)

  if (any(is.na(x))) {
    if (na.rm) {
      message("Your data contains NA and will be removed.")
    } else {
      warning(
        "Your data contains NA. Please make sure na.rm is appropriately set."
      )
    }
  }

  return(exp(stats::sd(log(x), na.rm = na.rm)))
}

#' Computes the geometric CV of a vector x
#'
#' @param x vector of data you want the geometric CV of.
#' @param na.rm boolean to remove NA from vector. Default is FALSE
#'
#' @return the geometric CV of the input vector x
#' @export
#'
#' @examples
#' geom_cv(c(1, 2, 3, 2, 1))
geom_cv <- function(x, na.rm = FALSE) {
  checkmate::assertNumeric(x)

  if (any(is.na(x))) {
    if (na.rm) {
      message("Your data contains NA and will be removed.")
    } else {
      warning(
        "Your data contains NA. Please make sure na.rm is appropriately set."
      )
    }
  }

  return(sqrt(exp(stats::sd(log(x), na.rm = na.rm)**2) - 1))
}


#' Convert dataframe to correlation matrix in tidy format
#'
#' Computes pairwise correlations between numeric columns and returns
#' results in a tidy long format, sorted by absolute correlation.
#'
#' @param data A data.frame containing the variables to correlate
#' @param columns Character vector of column names to include. If NULL,
#'   all numeric columns will be used.
#' @param use Method for handling missing values, passed to cor().
#'   Default is "complete.obs".
#' @param method Correlation method, passed to cor(). Default is "pearson".
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name1, name2}{Variable pair names (lexicographically ordered)}
#'     \item{CORR}{Correlation coefficient}
#'     \item{ABSCORR}{Absolute correlation coefficient}
#'   }
#'   Results are sorted by ABSCORR in descending order.
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Create sample data
#' set.seed(123)
#' df <- data.frame(
#'   A = rnorm(100, 5, 2),
#'   B = rnorm(100, 10, 3),
#'   C = rnorm(100, 15, 1),
#'   D = letters[1:100] # non-numeric
#' )
#' df$B <- df$A * 0.8 + rnorm(100, 0, 1) # Create some correlation
#'
#' # All numeric columns
#' cor_df(df)
#'
#' # Specific columns
#' cor_df(df, columns = c("A", "B", "C"))
#'
cor_df <- function(
  data,
  columns = NULL,
  use = "complete.obs",
  method = "pearson"
) {
  # Input validation
  checkmate::assert_data_frame(data)
  if (nrow(data) == 0) stop("data cannot be empty")

  checkmate::assert_character(columns, null.ok = TRUE)
  checkmate::assert_choice(
    use,
    c(
      "everything",
      "all.obs",
      "complete.obs",
      "na.or.complete",
      "pairwise.complete.obs"
    )
  )
  checkmate::assert_choice(method, c("pearson", "kendall", "spearman"))

  # Column selection
  if (is.null(columns)) {
    # Auto-detect numeric columns
    columns <- names(data)[sapply(data, is.numeric)]
    if (length(columns) == 0) {
      stop("No numeric columns found in data")
    }
  } else {
    columns <- unique(columns)
    missing_cols <- setdiff(columns, names(data))
    if (length(missing_cols) > 0) {
      stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
    }

    # Filter to numeric columns only
    numeric_mask <- sapply(data[columns], is.numeric)
    non_numeric <- columns[!numeric_mask]
    if (length(non_numeric) > 0) {
      warning(
        "Non-numeric columns removed: ",
        paste(non_numeric, collapse = ", ")
      )
      columns <- columns[numeric_mask]
    }
  }

  # Check we have at least 2 columns
  if (length(columns) < 2) {
    stop("At least 2 numeric columns required for correlation")
  }

  # Compute correlation matrix
  cor_matrix <- stats::cor(
    data[, columns, drop = FALSE],
    use = use,
    method = method
  )

  # Convert to long format
  result <- cor_matrix |>
    as.data.frame() |>
    tibble::rownames_to_column("rowname") |>
    tidyr::pivot_longer(-"rowname", names_to = "name", values_to = "CORR") |>
    dplyr::mutate(
      name1 = pmin(.data$rowname, .data$name),
      name2 = pmax(.data$rowname, .data$name),
      ABSCORR = abs(.data$CORR)
    ) |>
    dplyr::distinct(.data$name1, .data$name2, .data$CORR, .data$ABSCORR) |>
    dplyr::filter(.data$name1 != .data$name2) |>
    dplyr::arrange(dplyr::desc(.data$ABSCORR)) |>
    dplyr::select("name1", "name2", "CORR", "ABSCORR")

  return(result)
}
