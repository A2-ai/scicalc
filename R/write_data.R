#' Writes data to path, if directory doesn't exist it is created before file is written
#'
#' @param data the data object to write to file
#' @param path the destination of the file (csv or parquet)
#' @param overwrite boolean of whether to overwrite or not.
#' @param na_value string for what to replace NA with. default is "."
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
write_file_with_hash <- function(data, path, overwrite = FALSE, na_value = ".") {
  if (!overwrite) {
    checkmate::assert(!file.exists(path))
  }

  #overwrite == true and/or file doesn't already exist
  extension <- strsplit(basename(path), split = "\\.")[[1]][[2]]
  if (extension %in% c("csv", "parquet")) {
    if (!dir.exists(dirname(path))) {
      create_dir(dirname(path))
    }
    if (extension == "csv") {
      write_csv_with_hash(data, path, na_value)
    } else if (extension == "parquet") {
      write_parquet_with_hash(data, path)
    }
  } else {
    rlang::warn("File type not yet supported")
  }
}

#' Writes data to csv_path with na_value replacing NA values.
#'
#' @param data a data object to write to file
#' @param csv_path the file path to save the csv
#' @param na_value the value of what to replace NA with
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
write_csv_with_hash <- function(data, csv_path, na_value = ".") {
  checkmate::assert(strsplit(basename(csv_path), split = "\\.")[[1]][[2]] == "csv")

  readr::write_csv(data, csv_path, na = na_value)
  hash <- digest::digest(file = csv_path)
  cat(csv_path, hash, sep = ": ")
  cat("\n")
}

#' Writes data to parquet_path and prints hash
#'
#' @param data the data object to save to parquet_path
#' @param parquet_path the path to the desired parquet destination
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
write_parquet_with_hash <- function(data, parquet_path) {
  checkmate::assert(strsplit(basename(parquet_path), split = "\\.")[[1]][[2]] == "parquet")

  arrow::write_parquet(data, sink = parquet_path)

  hash <- digest::digest(file = parquet_path)
  cat(parquet_path, hash, sep = ": ")
  cat("\n")
}
