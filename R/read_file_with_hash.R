#' Reads the data from a file (csv or parquet) and prints the hash
#'
#' @param file_path path to data file
#' @param ... additional arguments to digest, read_csv, read_parquet, read_sas, read_pzfx, read_xpt
#'
#' @return data within the supplied file
#' @export
#'
#' @examples \dontrun{
#' dat <- read_file_with_hash("data/derived/PK_data.parquet")
#' dat2 <- read_file_with_hash("data/source/data.csv")
#' }
read_file_with_hash <- function(file_path, ...) {
  checkmate::assert(file.exists(file_path))

  extension <- tools::file_ext(file_path)
  if (extension == "csv") {
    read_csv_with_hash(file_path, ...)
  } else if (extension == "parquet") {
    read_parquet_with_hash(file_path, ...)
  } else if (extension == "sas7bdat") {
    read_sas_with_hash(file_path, ...)
  } else if (extension == "pzfx") {
    read_pzfx_with_hash(file_path, ...)
  } else if (extension == "xpt") {
    read_xpt_with_hash(file_path, ...)
  } else {
    warning(paste0("File type: ", extension, " not currently supported\n"))
  }
}

#' Reads data from csv file and prints hash of contents.
#'
#' @param csv_file_path path to csv file to ingest
#' @param ... additional arguments for digest or read_csv
#'
#' @return dataframe of data within file
#' @export
#'
#' @examples \dontrun{
#' read_csv_with_hash("data/derived/example_data.csv")
#' }
read_csv_with_hash <- function(csv_file_path, ...) {
  checkmate::assert(file.exists(csv_file_path))
  checkmate::assert(
    tools::file_ext(basename(csv_file_path)) == "csv"
  )
  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = csv_file_path

  read_csv_args <- args[names(args) %in% names(formals(readr::read_csv))]
  read_csv_args$file = csv_file_path

  hash <- do.call(digest::digest, digest_args)
  cat(basename(csv_file_path), hash, sep = ": ")
  cat("\n")

  do.call(readr::read_csv, read_csv_args)
}

#' Reads data from parquet file and prints hash of contents.
#'
#' @param parquet_file_path path to parquet file to ingest
#' @param ... additional arguments to digest or read_parquet
#'
#' @return a tibble of data within file
#' @export
#'
#' @examples \dontrun{
#' read_parquet_with_hash("data/derived/example_data.parquet")
#' }
read_parquet_with_hash <- function(parquet_file_path, ...) {
  checkmate::assert(file.exists(parquet_file_path))
  checkmate::assert(
    tools::file_ext(basename(parquet_file_path)) == "parquet"
  )
  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = parquet_file_path

  read_parquet_args <- args[names(args) %in% names(formals(arrow::read_parquet))]
  read_parquet_args$file = parquet_file_path

  hash <- do.call(digest::digest, digest_args)
  cat(basename(parquet_file_path), hash, sep = ": ")
  cat("\n")
  do.call(arrow::read_parquet, read_parquet_args)
}

#' Reads data from sas file and prints hash of contents.
#'
#' @param sas_file_path path to sas file to ingest
#' @param ... additional arguments to digest or read_sas
#'
#' @return a dataframe(?) of data within file
#' @export
#'
#' @examples \dontrun{
#' read_sas_with_hash("data/source/example.sas7bdat")
#' }
read_sas_with_hash <- function(sas_file_path, ...) {
  checkmate::assert(file.exists(sas_file_path))
  checkmate::assert(
    tools::file_ext(basename(sas_file_path)) == "sas7bdat"
  )
  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = sas_file_path

  read_sas_args <- args[names(args) %in% names(formals(haven::read_sas))]
  read_sas_args$data_file = sas_file_path

  hash <- do.call(digest::digest, digest_args)
  cat(basename(sas_file_path), hash, sep = ": ")
  cat("\n")
  do.call(haven::read_sas, read_sas_args)
}

#' Reads data from xpt file and prints hash of contents.
#'
#' @param xpt_file_path an xpt file to ingest
#' @param ... additional arguments to digest or read_xpt
#'
#' @return a dataframe(?) of data within file
#' @export
#'
#' @examples \dontrun{
#' read_xpt_with_hash("data/source/example.xpt")
#' }
read_xpt_with_hash <- function(xpt_file_path, ...) {
  checkmate::assert(file.exists(xpt_file_path))
  checkmate::assert(
    tools::file_ext(basename(xpt_file_path)) == "xpt"
  )
  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = xpt_file_path

  read_xpt_args <- args[names(args) %in% names(formals(haven::read_xpt))]
  read_xpt_args$file = xpt_file_path

  hash <- do.call(digest::digest, digest_args)
  cat(basename(xpt_file_path), hash, sep = ": ")
  cat("\n")
  do.call(haven::read_xpt, read_xpt_args)
}


#' Reads in table from a prism pzfx file.
#'
#' @param pzfx_file_path path to pzfx file
#' @param ... additional arguments to digest or read_pzfx
#'
#' @return data within the table of the pzfx file
#' @export
#'
#' @examples \dontrun{
#' read_pzfx_with_hash("mydata.pzfx", table = "experiment1")
#' }
read_pzfx_with_hash <- function(pzfx_file_path, ...) {
  checkmate::assert(file.exists(pzfx_file_path))
  checkmate::assert(
    tools::file_ext(basename(pzfx_file_path)) == "pzfx"
  )
  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = pzfx_file_path

  read_pzfx_args <- args[names(args) %in% names(formals(pzfx::read_pzfx))]
  read_pzfx_args$path = pzfx_file_path
  checkmate::assert(!is.null(read_pzfx_args$table))
  checkmate::assert_choice(read_pzfx_args$table, pzfx::pzfx_tables(read_pzfx_args$path))

  hash <- do.call(digest::digest, digest_args)
  cat(basename(pzfx_file_path), hash, sep = ": ")
  cat("\n")
  do.call(pzfx::read_pzfx, read_pzfx_args)
}

################################################################################
##### read in hashed file

#' Reads a file if the supplied hash matches the file's hash
#'
#' @param file_path path to file with data you want to read
#' @param hash hash you expect the file to have
#' @param ... additional arguments for digest or read_csv, parquet, sas
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
read_hashed_file <- function(file_path, hash, ...) {
  checkmate::assert(file.exists(file_path))

  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = file_path

  file_hash <- do.call(digest::digest, digest_args)
  extension <- tools::file_ext(basename(file_path))

  if (file_hash == hash) {
    if (extension == "csv") {
      read_csv_args <- args[names(args) %in% names(formals(readr::read_csv))]
      read_csv_args$file = file_path
      do.call(readr::read_csv, read_csv_args)
    } else if (extension == "parquet") {
      read_parquet_args <- args[names(args) %in% names(formals(arrow::read_parquet))]
      read_parquet_args$file = file_path
      arrow::read_parquet(file_path)
    } else if (extension == "sas7bdat") {
      read_sas_args <- args[names(args) %in% names(formals(haven::read_sas))]
      read_sas_args$data_file = file_path
      haven::read_sas(file_path)
    } else {
      warning(paste0("File type: ", extension, " not currently supported\n"))
    }
  } else {
    rlang::abort("Hash does not match file's hash!")
  }
}

