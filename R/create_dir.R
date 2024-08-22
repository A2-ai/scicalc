#' Creates the directory if it doesn't exist
#'
#' @param path path of directory to be created
#'
#' @return Nothing
#' @export
#'
#' @examples \dontrun{
#' create_dir("derived/data/test")
#' }
create_dir <- function(path) {
  normal_path <- normalizePath(path, mustWork = FALSE)
  checkmate::assert(!dir.exists(normal_path))

  dirs_to_create <- c()
  current_path <- normal_path

  while (!dir.exists(current_path) && current_path != dirname(current_path)) {
    dirs_to_create <- c(current_path, dirs_to_create)
    current_path <- dirname(current_path)
  }

  if (length(dirs_to_create) > 0) {
    for (dir in dirs_to_create) {
      dir.create(dir, recursive = TRUE, showWarnings = TRUE)
    }
  }
}
