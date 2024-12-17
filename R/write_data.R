#' Writes data to path, if directory doesn't exist it is created before file is written
#'
#' @param data the data object to write to file
#' @param path the destination of the file (csv or parquet)
#' @param overwrite boolean of whether to overwrite or not.
#' @param ... additional arguments for digest or write_file.
#'
#' @return Nothing, File is created and hash of created file is printed
#' @export
#'
#' @examples \dontrun{
#' df <- data.frame(
#'   "a" = c(1, 2, 3, 4)
#'   "b" = c("A", "B", "C", "D")
#' )
#' write_data_with_hash(df, "data.csv")
#' }
write_file_with_hash <- function(data, path, overwrite = FALSE, ...) {
  if (!overwrite) {
    checkmate::assert(!file.exists(path))
  }

  args <- rlang::list2(...)

  #overwrite == true and/or file doesn't already exist
  extension <- tools::file_ext(basename(path))
  if (extension %in% c("csv", "parquet")) {
    if (!dir.exists(dirname(path))) {
      fs::dir_create(dirname(path), recurse = TRUE)
    }
    if (extension == "csv") {
      write_csv_with_hash(data, path, ...)
    } else if (extension == "parquet") {
      write_parquet_with_hash(data, path, ...)
    }
  } else {
    rlang::warn("File type not yet supported")
  }
}

#' Writes data to csv_path with na_value replacing NA values.
#'
#' @param data a data object to write to file
#' @param csv_path the file path to save the csv
#' @param ... additional arguments to digest or write_csv
#'
#' @return Nothing, creates csv_path file and prints hash of the file
#' @export
#'
#' @examples \dontrun{
#' df <- data.frame(
#'   "a" = c(1, 2, 3, 4)
#'   "b" = c("A", "B", "C", "D")
#' )
#' write_csv_with_hash(df, "test/test.csv")
#' }
write_csv_with_hash <- function(data, csv_path, ...) {
  checkmate::assert(tools::file_ext(basename(csv_path)) == "csv")

  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = csv_path

  write_csv_args <- args[names(args) %in% names(formals(readr::read_csv))]
  write_csv_args$x = data
  write_csv_args$file = csv_path

  do.call(readr::write_csv, write_csv_args)
  hash <- do.call(digest::digest, digest_args)
  cat(csv_path, hash, sep = ": ")
  cat("\n")
}

#' Writes data to parquet_path and prints hash
#'
#' @param data the data object to save to parquet_path
#' @param parquet_path the path to the desired parquet destination
#' @param ... additional arguments to digest and write_parquet
#'
#' @return Nothing. creates parquet_path file and prints hash
#' @export
#'
#' @examples \dontrun{
#' df <- data.frame(
#'   "a" = c(1, 2, 3, 4)
#'   "b" = c("A", "B", "C", "D")
#' )
#' write_parquet_with_hash(df, "test/test.parquet")
#' }
write_parquet_with_hash <- function(data, parquet_path, ...) {
  checkmate::assert(tools::file_ext(basename(parquet_path)) == "parquet")

  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = parquet_path

  write_parquet_args <- args[names(args) %in% names(formals(arrow::read_parquet))]
  write_parquet_args$sink = parquet_path
  write_parquet_args$x = data

  do.call(arrow::write_parquet, write_parquet_args)

  hash <- do.call(digest::digest, digest_args)
  cat(parquet_path, hash, sep = ": ")
  cat("\n")
}
