#' Write Data File with Hash Output
#'
#' @param data the data object to write to file
#' @param path the destination of the file (csv or parquet)
#' @param overwrite boolean of whether to overwrite or not.
#' @param ... additional arguments for digest, write_csv, write_parquet, or `writer`.
#' @param algo hashing algorithm to use, default is "blake3"
#' @param writer optional function used to write the file, called as `writer(data, path, ...)`.
#'   Required for file extensions this function doesn't know how to write. For a known
#'   extension (csv, parquet), `writer` is ignored unless `force = TRUE`.
#' @param force if `TRUE`, use `writer` even for a known extension instead of the built-in
#'   writer. Must be `FALSE` (the default) when `writer` is not supplied.
#'
#' @return Nothing, File is created and hash of created file is printed
#'
#' @family file_io
#' @export
#'
#' @examples \dontrun{
#' df <- data.frame(
#'   "a" = c(1, 2, 3, 4)
#'   "b" = c("A", "B", "C", "D")
#' )
#' write_file_with_hash(df, "data.csv")
#' write_file_with_hash(df, "data.rds", writer = saveRDS)
#' }
write_file_with_hash <- function(data, path, overwrite = FALSE, ..., algo = "blake3", writer = NULL, force = FALSE) {
  if (!overwrite) {
    checkmate::assert(!file.exists(path))
  }

  if (!is.null(writer)) {
    checkmate::assert_function(writer)
  }
  checkmate::assert_flag(force)
  if (force && is.null(writer)) {
    rlang::abort("`force` requires `writer` to be supplied.")
  }

  extension <- tools::file_ext(basename(path))
  known_extensions <- c("csv", "parquet")

  if (extension %in% known_extensions) {
    if (!is.null(writer) && !force) {
      rlang::warn(paste0(
        "Supplied `writer` ignored for known extension '", extension,
        "'; use `force = TRUE` to override."
      ))
      writer <- NULL
    }
  } else if (is.null(writer)) {
    rlang::warn("File type not yet supported. Supply a `writer` function to write this file type.")
    return(invisible(NULL))
  }

  if (!dir.exists(dirname(path))) {
    fs::dir_create(dirname(path), recurse = TRUE)
  }

  if (!is.null(writer)) {
    writer(data, path, ...)
    return(print_file_hash(path, ..., algo = algo, event = "write"))
  }

  if (extension == "csv") {
    .write_csv_with_hash(data, path, ..., algo = algo)
  } else if (extension == "parquet") {
    .write_parquet_with_hash(data, path, ..., algo = algo)
  }
}

#' @noRd
.write_csv_with_hash <- function(data, csv_path, ..., algo = "blake3") {
  checkmate::assert(tools::file_ext(basename(csv_path)) == "csv")

  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = csv_path
  digest_args$algo <- algo

  write_csv_args <- args[names(args) %in% names(formals(readr::write_csv))]
  write_csv_args$x = data
  write_csv_args$file = csv_path

  do.call(readr::write_csv, write_csv_args)
  hash <- do.call(digest::digest, digest_args)
  cat(csv_path, hash, sep = ": ")
  cat("\n")
  log_audit_event("write", file = basename(csv_path), hash = hash, algo = algo)
}

#' Write CSV File with Hash Output
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated; use [write_file_with_hash()].
#'
#' @param data a data object to write to file
#' @param csv_path the file path to save the csv
#' @param ... additional arguments to digest or write_csv
#'
#' @return Nothing, creates csv_path file and prints hash of the file
#'
#' @family file_io
#' @keywords internal
#' @export
#'
#' @examples \dontrun{
#' df <- data.frame(
#'   "a" = c(1, 2, 3, 4)
#'   "b" = c("A", "B", "C", "D")
#' )
#' write_file_with_hash(df, "test/test.csv")
#' }
write_csv_with_hash <- function(data, csv_path, ...) {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "write_csv_with_hash()",
    with = "write_file_with_hash()"
  )
  .write_csv_with_hash(data, csv_path, ...)
}

#' @noRd
.write_parquet_with_hash <- function(data, parquet_path, ..., algo = "blake3") {
  checkmate::assert(tools::file_ext(basename(parquet_path)) == "parquet")

  args <- rlang::list2(...)

  digest_args <- args[names(args) %in% names(formals(digest::digest))]
  digest_args$file = parquet_path
  digest_args$algo <- algo

  write_parquet_args <- args[
    names(args) %in% names(formals(arrow::write_parquet))
  ]
  write_parquet_args$sink = parquet_path
  write_parquet_args$x = data

  do.call(arrow::write_parquet, write_parquet_args)

  hash <- do.call(digest::digest, digest_args)
  cat(parquet_path, hash, sep = ": ")
  cat("\n")
  log_audit_event("write", file = basename(parquet_path), hash = hash, algo = algo)
}

#' Write Parquet File with Hash Output
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated; use [write_file_with_hash()].
#'
#' @param data the data object to save to parquet_path
#' @param parquet_path the path to the desired parquet destination
#' @param ... additional arguments to digest and write_parquet
#'
#' @return Nothing. creates parquet_path file and prints hash
#'
#' @family file_io
#' @keywords internal
#' @export
#'
#' @examples \dontrun{
#' df <- data.frame(
#'   "a" = c(1, 2, 3, 4)
#'   "b" = c("A", "B", "C", "D")
#' )
#' write_file_with_hash(df, "test/test.parquet")
#' }
write_parquet_with_hash <- function(data, parquet_path, ...) {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "write_parquet_with_hash()",
    with = "write_file_with_hash()"
  )
  .write_parquet_with_hash(data, parquet_path, ...)
}
