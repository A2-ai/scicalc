#' Read Data File with Hash Verification
#'
#' @param file_path path to data file
#' @param ... additional arguments to digest, read_csv, read_parquet, read_sas, read_pzfx, read_xpt
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return data within the supplied file
#'
#' @family file_io
#' @export
#'
#' @examples \dontrun{
#' dat <- read_file_with_hash("data/derived/PK_data.parquet")
#' dat2 <- read_file_with_hash("data/source/data.csv")
#' }
read_file_with_hash <- function(file_path, ..., algo = "blake3") {
  checkmate::assert(file.exists(file_path))

  valid_algos <- eval(formals(digest::digest)$algo)
  if (!algo %in% valid_algos) {
    rlang::abort(
      message = paste0(
        "Invalid algorithm: '", algo, "'.\n",
        "Valid algorithms are: ", paste(valid_algos, collapse = ", ")
      )
    )
  }

  extension <- tools::file_ext(file_path)
  if (extension == "csv") {
    read_csv_with_hash(file_path, ..., algo = algo)
  } else if (extension == "parquet") {
    read_parquet_with_hash(file_path, ..., algo = algo)
  } else if (extension == "sas7bdat") {
    read_sas_with_hash(file_path, ..., algo = algo)
  } else if (extension == "pzfx") {
    read_pzfx_with_hash(file_path, ..., algo = algo)
  } else if (extension == "xpt") {
    read_xpt_with_hash(file_path, ..., algo = algo)
  } else if (extension %in% c("xlsx", "xls", "xlsm")) {
    read_excel_with_hash(file_path, ..., algo = algo)
  } else {
    warning(paste0("File type: ", extension, " not currently supported\n"))
  }
}

#' Read CSV File with Hash Verification
#'
#' @param csv_file_path path to csv file to ingest
#' @param ... additional arguments for digest or read_csv
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return dataframe of data within file
#'
#' @family file_io
#' @export
#'
#' @examples \dontrun{
#' read_csv_with_hash("data/derived/example_data.csv")
#' }
read_csv_with_hash <- function(csv_file_path, ..., algo = "blake3") {
  lifecycle::deprecate_soft(
    when = "0.6.0",
    what = "read_csv_with_hash()",
    with = "read_file_with_hash()",
    details = "read_csv_with_hash() will become internal in a future version. Use read_file_with_hash() which automatically detects file type."
  )

  checkmate::assert(file.exists(csv_file_path))
  checkmate::assert(
    tools::file_ext(basename(csv_file_path)) == "csv"
  )
  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = csv_file_path
  digest_args$algo <- algo

  read_csv_args <- args[names(args) %in% names(formals(readr::read_csv))]
  read_csv_args$file = csv_file_path

  hash <- do.call(digest::digest, digest_args)
  cat(basename(csv_file_path), hash, sep = ": ")
  cat("\n")

  do.call(readr::read_csv, read_csv_args)
}

#' Read Parquet File with Hash Verification
#'
#' @param parquet_file_path path to parquet file to ingest
#' @param ... additional arguments to digest or read_parquet
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return a tibble of data within file
#'
#' @family file_io
#' @export
#'
#' @examples \dontrun{
#' read_parquet_with_hash("data/derived/example_data.parquet")
#' }
read_parquet_with_hash <- function(parquet_file_path, ..., algo = "blake3") {
  lifecycle::deprecate_soft(
    when = "0.6.0",
    what = "read_parquet_with_hash()",
    with = "read_file_with_hash()",
    details = "read_parquet_with_hash() will become internal in a future version. Use read_file_with_hash() which automatically detects file type."
  )

  checkmate::assert(file.exists(parquet_file_path))
  checkmate::assert(
    tools::file_ext(basename(parquet_file_path)) == "parquet"
  )
  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = parquet_file_path
  digest_args$algo <- algo

  read_parquet_args <- args[
    names(args) %in% names(formals(arrow::read_parquet))
  ]
  read_parquet_args$file = parquet_file_path

  hash <- do.call(digest::digest, digest_args)
  cat(basename(parquet_file_path), hash, sep = ": ")
  cat("\n")
  do.call(arrow::read_parquet, read_parquet_args)
}

#' Read SAS File with Hash Verification
#'
#' @param sas_file_path path to sas file to ingest
#' @param ... additional arguments to digest or read_sas
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return a dataframe(?) of data within file
#'
#' @family file_io
#' @export
#'
#' @examples \dontrun{
#' read_sas_with_hash("data/source/example.sas7bdat")
#' }
read_sas_with_hash <- function(sas_file_path, ..., algo = "blake3") {
  lifecycle::deprecate_soft(
    when = "0.6.0",
    what = "read_sas_with_hash()",
    with = "read_file_with_hash()",
    details = "read_sas_with_hash() will become internal in a future version. Use read_file_with_hash() which automatically detects file type."
  )

  checkmate::assert(file.exists(sas_file_path))
  checkmate::assert(
    tools::file_ext(basename(sas_file_path)) == "sas7bdat"
  )
  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = sas_file_path
  digest_args$algo <- algo

  read_sas_args <- args[names(args) %in% names(formals(haven::read_sas))]
  read_sas_args$data_file = sas_file_path

  hash <- do.call(digest::digest, digest_args)
  cat(basename(sas_file_path), hash, sep = ": ")
  cat("\n")
  do.call(haven::read_sas, read_sas_args)
}

#' Read XPT File with Hash Verification
#'
#' @param xpt_file_path an xpt file to ingest
#' @param ... additional arguments to digest or read_xpt
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return a dataframe(?) of data within file
#'
#' @family file_io
#' @export
#'
#' @examples \dontrun{
#' read_xpt_with_hash("data/source/example.xpt")
#' }
read_xpt_with_hash <- function(xpt_file_path, ..., algo = "blake3") {
  lifecycle::deprecate_soft(
    when = "0.6.0",
    what = "read_xpt_with_hash()",
    with = "read_file_with_hash()",
    details = "read_xpt_with_hash() will become internal in a future version. Use read_file_with_hash() which automatically detects file type."
  )

  checkmate::assert(file.exists(xpt_file_path))
  checkmate::assert(
    tools::file_ext(basename(xpt_file_path)) == "xpt"
  )
  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = xpt_file_path
  digest_args$algo <- algo

  read_xpt_args <- args[names(args) %in% names(formals(haven::read_xpt))]
  read_xpt_args$file = xpt_file_path

  hash <- do.call(digest::digest, digest_args)
  cat(basename(xpt_file_path), hash, sep = ": ")
  cat("\n")
  do.call(haven::read_xpt, read_xpt_args)
}


#' Read Excel File with Hash Verification
#'
#' @param xlsx_file_path an xlsx/xls file to ingest
#' @param ... additional arguments to digest or read_excel
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return a dataframe(?) of data within file
#'
#' @family file_io
#' @export
#'
#' @examples \dontrun{
#' read_excel_with_hash("data/source/example.xpt")
#' }
read_excel_with_hash <- function(xlsx_file_path, ..., algo = "blake3") {
  lifecycle::deprecate_soft(
    when = "0.6.0",
    what = "read_excel_with_hash()",
    with = "read_file_with_hash()",
    details = "read_excel_with_hash() will become internal in a future version. Use read_file_with_hash() which automatically detects file type."
  )

  checkmate::assert(file.exists(xlsx_file_path))
  checkmate::assert(
    tools::file_ext(basename(xlsx_file_path)) %in% c("xlsx", "xls", "xlsm")
  )
  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = xlsx_file_path
  digest_args$algo <- algo

  read_excel_args <- args[names(args) %in% names(formals(readxl::read_excel))]
  read_excel_args$path = xlsx_file_path

  hash <- do.call(digest::digest, digest_args)
  cat(basename(xlsx_file_path), hash, sep = ": ")
  cat("\n")
  cat(sprintf("Sheets in %s: ", basename(xlsx_file_path)))
  cat(readxl::excel_sheets(xlsx_file_path), sep = ", ")
  cat("\n")
  do.call(readxl::read_excel, read_excel_args)
}

#' Read Prism PZFX File with Hash Verification
#'
#' @param pzfx_file_path path to pzfx file
#' @param ... additional arguments to digest or read_pzfx
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return data within the table of the pzfx file
#'
#' @family file_io
#' @export
#'
#' @examples \dontrun{
#' read_pzfx_with_hash("mydata.pzfx", table = "experiment1")
#' }
read_pzfx_with_hash <- function(pzfx_file_path, ..., algo = "blake3") {
  lifecycle::deprecate_soft(
    when = "0.6.0",
    what = "read_pzfx_with_hash()",
    with = "read_file_with_hash()",
    details = "read_pzfx_with_hash() will become internal in a future version. Use read_file_with_hash() which automatically detects file type."
  )

  rlang::check_installed("pzfx")
  checkmate::assert(file.exists(pzfx_file_path))
  checkmate::assert(
    tools::file_ext(basename(pzfx_file_path)) == "pzfx"
  )
  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = pzfx_file_path
  digest_args$algo <- algo

  read_pzfx_args <- args[names(args) %in% names(formals(pzfx::read_pzfx))]
  read_pzfx_args$path = pzfx_file_path
  checkmate::assert(!is.null(read_pzfx_args$table))
  checkmate::assert_choice(
    read_pzfx_args$table,
    pzfx::pzfx_tables(read_pzfx_args$path)
  )

  hash <- do.call(digest::digest, digest_args)
  cat(basename(pzfx_file_path), hash, sep = ": ")
  cat("\n")
  do.call(pzfx::read_pzfx, read_pzfx_args)
}

#####     read in hashed file     #####

#' Read File with Required Hash Match
#'
#' @param file_path path to file with data you want to read
#' @param hash hash you expect the file to have
#' @param ... additional arguments for digest or read_csv, parquet, sas
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return data object of contents of file_path
#'
#' @family file_io
#' @export
#'
#' @examples \dontrun{
#' file_path <- "data/derived/example_pk.parquet"
#'
#' hash <- 0cfd6da55e6c1e198effe1e584c26d79
#' read_hashed_file(file_path, hash)
#' }
read_hashed_file <- function(file_path, hash, ..., algo = "blake3") {
  checkmate::assert(file.exists(file_path))

  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = file_path
  digest_args$algo <- algo

  file_hash <- do.call(digest::digest, digest_args)
  extension <- tools::file_ext(basename(file_path))

  if (file_hash == hash) {
    if (extension == "csv") {
      read_csv_args <- args[names(args) %in% names(formals(readr::read_csv))]
      read_csv_args$file = file_path
      do.call(readr::read_csv, read_csv_args)
    } else if (extension == "parquet") {
      read_parquet_args <- args[
        names(args) %in% names(formals(arrow::read_parquet))
      ]
      read_parquet_args$file = file_path
      do.call(arrow::read_parquet, read_parquet_args)
    } else if (extension == "sas7bdat") {
      read_sas_args <- args[names(args) %in% names(formals(haven::read_sas))]
      read_sas_args$data_file = file_path
      do.call(haven::read_sas, read_sas_args)
    } else if (extension == "xpt") {
      read_xpt_args <- args[names(args) %in% names(formals(haven::read_xpt))]
      read_xpt_args$file <- file_path
      do.call(haven::read_xpt, read_xpt_args)
    } else if (extension == "pzfx") {
      read_pzfx_args <- args[names(args) %in% names(formals(pzfx::read_pzfx))]
      read_pzfx_args$path = file_path
      checkmate::assert(!is.null(read_pzfx_args$table))
      checkmate::assert_choice(
        read_pzfx_args$table,
        pzfx::pzfx_tables(read_pzfx_args$path)
      )
      do.call(pzfx::read_pzfx, read_pzfx_args)
    } else if (extension %in% c("xlsx", "xls", "xlsm")) {
      read_excel_args <- args[
        names(args) %in% names(formals(readxl::read_excel))
      ]
      read_excel_args$path = file_path
      do.call(readxl::read_excel, read_excel_args)
    } else {
      warning(paste0("File type: ", extension, " not currently supported\n"))
    }
  } else {
    rlang::abort("Hash does not match file's hash!")
  }
}
