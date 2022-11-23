#' Work with the the `alarmdata` cache
#'
#' Functions to inspect and clear the cache. If the cache is not enabled, uses a
#' temporary directory.
#'
#' @returns For `alarm_cache_size()`, the size in bytes, invisibly
#'
#' @examples
#' alarm_cache_size()
#'
#' @concept other
#' @export
#' @rdname alarm_cache
alarm_cache_size <- function() {
    files <- list.files(alarm_download_path(), recursive = TRUE, full.names = TRUE)
    x <- sum(vapply(files, file.size, numeric(1)))
    class(x) <- "object_size"
    message(format(x, unit = "auto"))
    invisible(as.numeric(x))
}

#' @param force FALSE by default. Asks the user to confirm if interactive. Does
#' not clear cache if force is FALSE and not interactive.
#' @returns For `alarm_cache_clear()`, the path to the cache, invisibly.
#'
#' @examples
#' alarm_cache_clear()
#'
#' @export
#' @rdname alarm_cache
alarm_cache_clear <- function(force = FALSE) {
    path <- alarm_download_path()
    if (interactive() && !force) {
        del <- utils::askYesNo(
            msg = "Are you sure? The entire cache will be deleted.",
            default = FALSE
        )
    } else {
        del <- force
    }
    if (del) unlink(path, recursive = TRUE)
    invisible(path)
}

#' @returns For `alarm_cache_path()`, the path to the cache
#'
#' @examples
#' alarm_cache_path()
#'
#' @export
#' @rdname alarm_cache
alarm_cache_path <- function() {
    alarm_download_path()
}
