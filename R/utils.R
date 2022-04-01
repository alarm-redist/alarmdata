# parse `state` and convert to a slug
get_slug = function(state, type="cd", year=2020) {
    abbr = censable::match_abb(state)
    if (length(abbr) == 0)
        cli::cli_abort("State {.val {state}} not found.", call=parent.frame())
    paste0(abbr, "_", type, "_", as.integer(year))
}

# figure out compression of raw vector
id_compression = function(raw) {
    gz_raw = c(0x1f, 0x8b, 0x08)
    xz_raw = c(0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00)
    bz_raw = c(0x42, 0x5a, 0x68)
    if (all(raw[1:6] == xz_raw)) {
        "xz"
    } else if (all(raw[1:3] == gz_raw)) {
        "gzip"
    } else if (all(raw[1:3] == bz_raw)) {
        "bzip2"
    } else {
        NA
    }
}

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
