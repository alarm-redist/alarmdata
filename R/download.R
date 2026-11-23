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

dv_files_cache <- new.env(parent = emptyenv())

dv_file_ids <- function(doi, refresh = FALSE) {
    if (is.null(dv_files_cache[[doi]]) || isTRUE(refresh)) {
        full_files <- if (isTRUE(refresh)) {
            dataverse::dataset_files(doi, server = DV_SERVER, use_cache = FALSE)
        } else {
            dataverse::dataset_files(doi, server = DV_SERVER)
        }
        ids <- sapply(full_files, function(f) f$dataFile$id)
        names(ids) <- sapply(full_files, function(f) f$label)
        dv_files_cache[[doi]] <- ids
    }
    dv_files_cache[[doi]]
}
