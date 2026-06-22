# parse `state` and convert to a slug
get_slug <- function(state, type = "cd", year = 2020) {
    abbr <- censable::match_abb(state)
    if (length(abbr) == 0)
        cli::cli_abort("State {.val {state}} not found.", call = parent.frame())

    avail_years = c(2010, 2020)
    if (!year %in% avail_years) {
        cli::cli_abort("Only year{?s} {as.character(avail_years)} {?is/are} supported.",
                       call = parent.frame())
    }

    paste0(abbr, "_", type, "_", as.integer(year))
}

# read a raw vector as an RDS
read_rds_mem <- function(raw, err_fname = "") {
    comp_fmt <- id_compression(raw)
    if (is.na(comp_fmt))
        cli_abort(c("File has unknown compression format.",
            ">" = "Please file an issue at {.url https://github.com/alarm-redist/fifty-states/issues}",
            ">" = "Provide filename {.val {err_fname}}"))

    con = rawConnection(memDecompress(raw, type = comp_fmt))
    on.exit(close(con))
    readRDS(gzcon(con, allowNonCompressed = TRUE))
}

# figure out compression of raw vector
id_compression <- function(raw) {
    gz_raw <- c(0x1f, 0x8b, 0x08)
    xz_raw <- c(0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00)
    bz_raw <- c(0x42, 0x5a, 0x68)
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

#' Download a file, with optional "caching"
#'
#' Back-end agnostic (currently `curl`)
#'
#' @param url a URL
#' @param path a file path
#' @param overwrite should the file at path be overwritten if it already exists? Default is FALSE.
#'
#' @returns the `curl` request
#' @noRd
download <- function(url, path, overwrite = FALSE) {
    dir <- dirname(path)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    if (!file.exists(path) || overwrite) {
        curl::curl_download(url = url, destfile = path)
    } else {
        cli_inform("File already downloaded at {.path {path}}. Set {.arg overwrite = TRUE} to overwrite.")
    }
}
