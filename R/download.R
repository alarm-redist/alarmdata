#' Figure out where to download things
#'
#' @noRd
alarm_download_path <- function(doi = NULL) {
    user_cache <- getOption("alarm.cache_dir")
    if (!is.null(user_cache)) {
        path <- user_cache
    } else if (getOption("alarm.use_cache", FALSE)) {
        path <- rappdirs::user_cache_dir("alarm")
    } else {
        path <- tempdir()
    }

    if (!is.null(doi)) {
        path <- paste0(path, "/", sub("^.*/", "", doi))
        if (!dir.exists(path)) {
            dir.create(path, recursive = TRUE)
        }
    }
    path
}
