#' @noRd
print_file_hash <- function(file_path, ..., label = file_path, event = "ingest") {
  args <- rlang::list2(...)
  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file <- file_path

  hash <- do.call(digest::digest, digest_args)
  cat(label, hash, sep = ": ")
  cat("\n")
  fn <- if (identical(event, "write")) "write_file_with_hash" else "read_file_with_hash"
  invisible(hash)
}

#' @noRd
file_reader <- function(extension) {
  switch(extension,
    csv = .read_csv,
    parquet = .read_parquet,
    sas7bdat = .read_sas,
    xpt = .read_xpt,
    pzfx = .read_pzfx,
    xlsx = ,
    xls = ,
    xlsm = .read_excel,
    NULL
  )
}

#' Read Data File with Hash Verification
#'
#' @param file_path path to data file
#' @param ... additional arguments to digest, read_csv, read_parquet, read_sas, read_pzfx, read_xpt, or `reader`
#' @param algo hashing algorithm to use, default is "blake3"
#' @param reader optional function used to read the file, called as `reader(file_path, ...)`.
#'   Required for file extensions this function doesn't know how to read. For a known
#'   extension (csv, parquet, sas7bdat, xpt, pzfx, xlsx/xls/xlsm), `reader` is ignored
#'   unless `force = TRUE`.
#' @param force if `TRUE`, use `reader` even for a known extension instead of the built-in
#'   reader. Must be `FALSE` (the default) when `reader` is not supplied.
#'
#' @return data within the supplied file, carrying `file_hash` and `data_hash` attributes
#'
#' @family file_io
#' @export
#'
#' @examples \dontrun{
#' dat <- read_file_with_hash("data/derived/PK_data.parquet")
#' dat2 <- read_file_with_hash("data/source/data.csv")
#' dat3 <- read_file_with_hash("data/derived/pk.feather", reader = arrow::read_feather)
#' }
read_file_with_hash <- function(file_path, ..., algo = "blake3", reader = NULL, force = FALSE) {
  checkmate::assert(file.exists(file_path))

  if (!is.null(reader)) {
    checkmate::assert_function(reader)
  }
  checkmate::assert_flag(force)
  if (force && is.null(reader)) {
    rlang::abort("`force` requires `reader` to be supplied.")
  }

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
  built_in <- file_reader(extension)

  if (!is.null(built_in)) {
    if (!is.null(reader) && !force) {
      rlang::warn(paste0(
        "Supplied `reader` ignored for known extension '", extension,
        "'; use `force = TRUE` to override."
      ))
      reader <- NULL
    }
  } else if (is.null(reader)) {
    warning(paste0(
      "File type: ", extension,
      " not currently supported. Supply a `reader` function to read this file type.\n"
    ))
    return(invisible(NULL))
  }

  if (is.null(reader)) {
    reader <- built_in
  }

  file_hash <- print_file_hash(file_path, ..., algo = algo, label = basename(file_path))
  data <- reader(file_path, ...)

  attr(data, "data_hash") <- digest::digest(data, algo = algo)
  attr(data, "file_hash") <- file_hash
  data
}

#####     readers     #####

#' @noRd
.read_csv <- function(file_path, ...) {
  args <- rlang::list2(...)
  read_csv_args <- args[names(args) %in% names(formals(readr::read_csv))]
  read_csv_args$file <- file_path
  do.call(readr::read_csv, read_csv_args)
}

#' @noRd
.read_parquet <- function(file_path, ...) {
  args <- rlang::list2(...)
  read_parquet_args <- args[names(args) %in% names(formals(arrow::read_parquet))]
  read_parquet_args$file <- file_path
  do.call(arrow::read_parquet, read_parquet_args)
}

#' @noRd
.read_sas <- function(file_path, ...) {
  args <- rlang::list2(...)
  read_sas_args <- args[names(args) %in% names(formals(haven::read_sas))]
  read_sas_args$data_file <- file_path
  do.call(haven::read_sas, read_sas_args)
}

#' @noRd
.read_xpt <- function(file_path, ...) {
  args <- rlang::list2(...)
  read_xpt_args <- args[names(args) %in% names(formals(haven::read_xpt))]
  read_xpt_args$file <- file_path
  do.call(haven::read_xpt, read_xpt_args)
}

#' @noRd
.read_excel <- function(file_path, ...) {
  args <- rlang::list2(...)
  read_excel_args <- args[names(args) %in% names(formals(readxl::read_excel))]
  read_excel_args$path <- file_path

  cat(sprintf("Sheets in %s: ", basename(file_path)))
  cat(readxl::excel_sheets(file_path), sep = ", ")
  cat("\n")
  do.call(readxl::read_excel, read_excel_args)
}

#' @noRd
.read_pzfx <- function(file_path, ...) {
  rlang::check_installed("pzfx")
  args <- rlang::list2(...)
  read_pzfx_args <- args[names(args) %in% names(formals(pzfx::read_pzfx))]
  read_pzfx_args$path <- file_path
  checkmate::assert(!is.null(read_pzfx_args$table))
  checkmate::assert_choice(read_pzfx_args$table, pzfx::pzfx_tables(file_path))
  do.call(pzfx::read_pzfx, read_pzfx_args)
}

#####     deprecated extension-specific wrappers     #####

#' @noRd
deprecated_read <- function(file_path, ..., algo, extension) {
  checkmate::assert(file.exists(file_path))
  checkmate::assert(tools::file_ext(basename(file_path)) %in% extension)
  read_file_with_hash(file_path, ..., algo = algo)
}

#' Read CSV File with Hash Verification
#'
#' @param csv_file_path path to csv file to ingest
#' @param ... additional arguments for digest or read_csv
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return dataframe of data within file
#'
#' @export
#'
#' @examples \dontrun{
#' read_csv_with_hash("data/derived/example_data.csv")
#' }
read_csv_with_hash <- function(csv_file_path, ..., algo = "blake3") {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "read_csv_with_hash()",
    with = "read_file_with_hash()",
    details = "read_csv_with_hash() will become internal in a future version. Use read_file_with_hash() which automatically detects file type."
  )
  deprecated_read(csv_file_path, ..., algo = algo, extension = "csv")
}

#' Read Parquet File with Hash Verification
#'
#' @param parquet_file_path path to parquet file to ingest
#' @param ... additional arguments to digest or read_parquet
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return a tibble of data within file
#'
#' @export
#'
#' @examples \dontrun{
#' read_parquet_with_hash("data/derived/example_data.parquet")
#' }
read_parquet_with_hash <- function(parquet_file_path, ..., algo = "blake3") {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "read_parquet_with_hash()",
    with = "read_file_with_hash()",
    details = "read_parquet_with_hash() will become internal in a future version. Use read_file_with_hash() which automatically detects file type."
  )
  deprecated_read(parquet_file_path, ..., algo = algo, extension = "parquet")
}

#' Read SAS File with Hash Verification
#'
#' @param sas_file_path path to sas file to ingest
#' @param ... additional arguments to digest or read_sas
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return a dataframe(?) of data within file
#'
#' @export
#'
#' @examples \dontrun{
#' read_sas_with_hash("data/source/example.sas7bdat")
#' }
read_sas_with_hash <- function(sas_file_path, ..., algo = "blake3") {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "read_sas_with_hash()",
    with = "read_file_with_hash()",
    details = "read_sas_with_hash() will become internal in a future version. Use read_file_with_hash() which automatically detects file type."
  )
  deprecated_read(sas_file_path, ..., algo = algo, extension = "sas7bdat")
}

#' Read XPT File with Hash Verification
#'
#' @param xpt_file_path an xpt file to ingest
#' @param ... additional arguments to digest or read_xpt
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return a dataframe(?) of data within file
#'
#' @export
#'
#' @examples \dontrun{
#' read_xpt_with_hash("data/source/example.xpt")
#' }
read_xpt_with_hash <- function(xpt_file_path, ..., algo = "blake3") {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "read_xpt_with_hash()",
    with = "read_file_with_hash()",
    details = "read_xpt_with_hash() will become internal in a future version. Use read_file_with_hash() which automatically detects file type."
  )
  deprecated_read(xpt_file_path, ..., algo = algo, extension = "xpt")
}

#' Read Excel File with Hash Verification
#'
#' @param xlsx_file_path an xlsx/xls file to ingest
#' @param ... additional arguments to digest or read_excel
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return a dataframe(?) of data within file
#'
#' @export
#'
#' @examples \dontrun{
#' read_excel_with_hash("data/source/example.xpt")
#' }
read_excel_with_hash <- function(xlsx_file_path, ..., algo = "blake3") {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "read_excel_with_hash()",
    with = "read_file_with_hash()",
    details = "read_excel_with_hash() will become internal in a future version. Use read_file_with_hash() which automatically detects file type."
  )
  deprecated_read(xlsx_file_path, ..., algo = algo, extension = c("xlsx", "xls", "xlsm"))
}

#' Read Prism PZFX File with Hash Verification
#'
#' @param pzfx_file_path path to pzfx file
#' @param ... additional arguments to digest or read_pzfx
#' @param algo hashing algorithm to use, default is "blake3"
#'
#' @return data within the table of the pzfx file
#'
#' @export
#'
#' @examples \dontrun{
#' read_pzfx_with_hash("mydata.pzfx", table = "experiment1")
#' }
read_pzfx_with_hash <- function(pzfx_file_path, ..., algo = "blake3") {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "read_pzfx_with_hash()",
    with = "read_file_with_hash()",
    details = "read_pzfx_with_hash() will become internal in a future version. Use read_file_with_hash() which automatically detects file type."
  )
  deprecated_read(pzfx_file_path, ..., algo = algo, extension = "pzfx")
}

#####     read in hashed file     #####

#' Read File with Required Hash Match
#'
#' @param file_path path to file with data you want to read
#' @param hash hash you expect the file to have
#' @param ... additional arguments for digest or read_csv, parquet, sas, or `reader`
#' @param algo hashing algorithm to use, default is "blake3"
#' @param reader optional function used to read the file, called as `reader(file_path, ...)`
#'   once the hash check passes. Required for file extensions this function doesn't know
#'   how to read. For a known extension (csv, parquet, sas7bdat, xpt, pzfx, xlsx/xls/xlsm),
#'   `reader` is ignored unless `force = TRUE`.
#' @param force if `TRUE`, use `reader` even for a known extension instead of the built-in
#'   reader. Must be `FALSE` (the default) when `reader` is not supplied.
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
read_hashed_file <- function(file_path, hash, ..., algo = "blake3", reader = NULL, force = FALSE) {
  checkmate::assert(file.exists(file_path))

  if (!is.null(reader)) {
    checkmate::assert_function(reader)
  }
  checkmate::assert_flag(force)
  if (force && is.null(reader)) {
    rlang::abort("`force` requires `reader` to be supplied.")
  }

  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = file_path
  digest_args$algo <- algo

  file_hash <- do.call(digest::digest, digest_args)
  if (file_hash != hash) {
    rlang::abort("Hash does not match file's hash!")
  }

  extension <- tools::file_ext(basename(file_path))
  built_in <- file_reader(extension)

  if (!is.null(built_in) && !is.null(reader) && !force) {
    rlang::warn(paste0(
      "Supplied `reader` ignored for known extension '", extension,
      "'; use `force = TRUE` to override."
    ))
    reader <- NULL
  }

  if (is.null(reader)) {
    reader <- built_in
  }

  if (is.null(reader)) {
    warning(paste0(
      "File type: ", extension,
      " not currently supported. Supply a `reader` function to read this file type.\n"
    ))
    return(invisible(NULL))
  }

  reader(file_path, ...)
}
