#' Download a file
#'
#' Backend-agnostic (currently `httr`)
#'
#' @param url a URL
#' @param path a file path
#' @param overwrite should the file at path be overwritten if it already exists? Default is FALSE.
#'
#' @returns the `httr` request
#' @noRd
download <- function(url, path, overwrite = FALSE) {
    dir <- dirname(path)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    if (!file.exists(path) || overwrite) {
        httr::GET(url = url, httr::write_disk(path))
    } else {
        message(paste0("File already downloaded at", path, ". Set `overwrite = TRUE` to overwrite."))
    }
}
