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

# Cache a file under a subdirectory named for its Dataverse dataset (the
# identifier from the DOI), so files from different datasets never collide.
cache_file <- function(name, doi) {
    dir <- file.path(alarm_download_path(), sub("^.*/", "", doi))
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    file.path(dir, name)
}
