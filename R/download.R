#' Figure out where to download things
#'
#' @noRd
alarm_download_path <- function() {
    user_cache <- getOption("alarm.cache_dir")
    if (!is.null(user_cache)) {
        user_cache
    } else if (getOption("alarm.use_cache", FALSE)) {
        rappdirs::user_cache_dir("alarm")
    } else {
        tempdir()
    }
}
