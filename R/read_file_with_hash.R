#' Reads the data from a file (csv or parquet) and prints the hash
#'
#' @param file_path path to data file
#' @param na Character vector of strings to interpret as missing values.
#' @param show_col_types logical values to show column types when reading csv files.
#' @param table optional argument only needed for reading prism pzfx files.
#'
#' @return data within the supplied file
#' @export
#'
#' @examples \dontrun{
#' dat <- read_file_with_hash("data/derived/PK_data.parquet")
#' dat2 <- read_file_with_hash("data/source/data.csv")
#' }
read_file_with_hash <- function(file_path, na = c("", "NA"), show_col_types = TRUE, table = NULL) {
  checkmate::assert(file.exists(file_path))

  extension <- strsplit(basename(file_path), split = "\\.")[[1]][[2]]
  if (extension == "csv") {
    read_csv_with_hash(file_path, na, show_col_types)
  } else if (extension == "parquet") {
    read_parquet_with_hash(file_path)
  } else if (extension == "sas7bdat") {
    read_sas_with_hash(file_path)
  } else if (extension == "pzfx") {
    read_pzfx_with_hash(file_path, table)
  } else {
    warning(paste0("File type: ", extension, " not currently supported\n"))
  }
}

#' Reads data from csv file and prints hash of contents.
#'
#' @param csv_file_path path to csv file to ingest
#' @param na Character vector of strings to interpret as missing values.
#' @param show_col_types logical values to show column types when reading csv files.
#' @return dataframe of data within file
#' @export
#'
#' @examples \dontrun{
#' read_csv_with_hash("data/derived/example_data.csv")
#' }
read_csv_with_hash <- function(csv_file_path, na = c("", "NA"), show_col_types = TRUE) {
  checkmate::assert(file.exists(csv_file_path))
  checkmate::assert_logical(show_col_types)
  checkmate::assert_character(na)
  checkmate::assert(strsplit(
    basename(csv_file_path),
    split = "\\.")[[1]][2] == "csv"
  )

  hash <- digest::digest(file = csv_file_path)
  cat(basename(csv_file_path), hash, sep = ": ")
  cat("\n")
  readr::read_csv(csv_file_path, na = na, show_col_types = show_col_types)
}

#' Reads data from parquet file and prints hash of contents.
#'
#' @param parquet_file_path path to parquet file to ingest
#'
#' @return a tibble of data within file
#' @export
#'
#' @examples \dontrun{
#' read_parquet_with_hash("data/derived/example_data.parquet")
#' }
read_parquet_with_hash <- function(parquet_file_path) {
  checkmate::assert(file.exists(parquet_file_path))
  checkmate::assert(strsplit(
    basename(parquet_file_path),
    split = "\\.")[[1]][[2]] == "parquet"
  )

  hash <- digest::digest(file = parquet_file_path)
  cat(basename(parquet_file_path), hash, sep = ": ")
  cat("\n")
  arrow::read_parquet(parquet_file_path)
}

#' Reads data from sas file and prints hash of contents.
#'
#' @param sas_file_path path to sas file to ingest
#'
#' @return a dataframe(?) of data within file
#' @export
#'
#' @examples \dontrun{
#' read_sas_with_hash("data/source/example.sas7bdat")
#' }
read_sas_with_hash <- function(sas_file_path) {
  checkmate::assert(file.exists(sas_file_path))
  checkmate::assert(strsplit(
    basename(sas_file_path),
    split = "\\.")[[1]][[2]] == "sas7bdat"
  )

  hash <- digest::digest(file = sas_file_path)
  cat(basename(sas_file_path), hash, sep = ": ")
  cat("\n")
  haven::read_sas(sas_file_path)
}

#' Reads in table from a prism pzfx file.
#'
#' @param pzfx_file_path path to pzfx file
#' @param table name of table wihtin file to read. Must be in pzfx_tables(pzfx_file_path)
#'
#' @return data within the table of the pzfx file
#' @export
#'
#' @examples \dontrun{
#' read_pzfx_with_hash("mydata.pzfx", table = "experiment1")
#' }
read_pzfx_with_hash <- function(pzfx_file_path, table) {
  checkmate::assert(file.exists(pzfx_file_path))
  checkmate::assert(strsplit(
    basename(pzfx_file_path),
    split = "\\.")[[1]][[2]] == "pzfx"
  )
  checkmate::assert_choice(table, pzfx::pzfx_tables(pzfx_file_path))

  hash <- digest::digest(file = pzfx_file_path)
  cat(basename(pzfx_file_path), hash, sep = ": ")
  cat("\n")
  pzfx::read_pzfx(pzfx_file_path, table = table)
}

################################################################################
##### read in hashed file

#' Reads a file if the supplied hash matches the file's hash
#'
#' @param file_path path to file with data you want to read
#' @param hash hash you expect the file to have
#'
#' @return data object of contents of file_path
#' @export
#'
#' @examples \dontrun{
#' file_path <- "data/derived/example_pk.parquet"
#'
#' hash <- 0cfd6da55e6c1e198effe1e584c26d79
#' read_hashed_file(file_path, hash)
#' }
read_hashed_file <- function(file_path, hash) {
  checkmate::assert(file.exists(file_path))
  file_hash <- digest::digest(file = file_path)
  extension <- strsplit(basename(file_path), split = "\\.")[[1]][[2]]

  if (file_hash == hash) {
    if (extension == "csv") {
      readr::read_csv(file_path)
    } else if (extension == "parquet") {
      arrow::read_parquet(file_path)
    } else if (extension == "sas7bdat") {
      haven::read_sas(file_path)
    } else {
      warning(paste0("File type: ", extension, " not currently supported\n"))
    }
  } else {
    rlang::abort("Hash does not match file's hash!")
  }
}

