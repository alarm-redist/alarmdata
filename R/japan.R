#' Download maps and plans from the Japan 47-Prefecture Simulation Project
#'
#' These functions will download [redist_map][redist::redist_map] and
#' [redist_plans][redist::redist_plans] objects for the Japan 47-Prefecture Simulation
#' Project from the ALARM Project's Dataverse. `alarm_japan_doc()` will
#' download documentation for a particular prefecture and show it in a browser.
#' `alarm_japan_stats` will download just the summary statistics for a prefecture
#'
#' The goal of the 47-Prefecture Simulation Project is to generate and analyze
#' redistricting plans for the single-member districts of the House of Representatives
#' of Japan using a redistricting simulation algorithm.
#' In this project, we analyzed the partisan bias of the 2022 redistricting
#' for 25 prefectures subject to redistricting.
#' Our simulations are designed to comply with the that the Council abides by.
#'
#' @template pref
#' @param year The redistricting cycle to download. Currently only `2022` is available.
#' @param stats If `TRUE` (the default), download summary statistics for each plan.
#' @param refresh If `TRUE`, ignore the cache and download again.
#' @param compress The compression level used for caching [redist_plans][redist::redist_plans] objects.
#'
#' @returns For `alarm_japan_map()`, a [redist_map][redist::redist_map]. For
#'   `alarm_japan_plans()`, a [redist_plans][redist::redist_plans]. For
#'   `alarm_japan_doc()`, invisibly returns the path to the HTML documentation,
#'   and also loads an HTML file into the viewer or web browser.
#'   For `alarm_japan_stats()`, a [tibble][dplyr::tibble].
#'
#' @examplesIf Sys.getenv("DATAVERSE_KEY") != ''
#'
#' # requires Harvard Dataverse API key
#' alarm_japan_map("miyagi")
#' alarm_japan_plans("miyagi", stats = FALSE)
#' alarm_japan_stats("miyagi")
#' alarm_japan_doc("miyagi")
#'
#' map <- alarm_japan_map("miyagi")
#' pl <- alarm_japan_plans("miyagi")
#'
#' @name alarm_japan
NULL

DV_DOI <- "doi:10.7910/DVN/Z9UKSH"
DV_SERVER <- "dataverse.harvard.edu"

#' @rdname alarm_japan
#' @export
alarm_japan_map <- function(pref, year = 2022, refresh = FALSE) {
    requireNamespace('sf', quietly = TRUE)
    slug <- get_slug_japan(pref, year = year)
    slug <- sub("^0", "", slug)
    path <- stringr::str_glue("{alarm_download_path()}/{slug}_map.rds")
    readr::read_rds(file = path)
}
#' @rdname alarm_japan
#' @export
alarm_japan_plans <- function(pref, stats = TRUE, year = 2022, refresh = FALSE, compress = "xz") {
    slug <- get_slug_japan(pref, year = year)
    slug <- sub("^0", "", slug)
    path <- stringr::str_glue("{alarm_download_path()}/{slug}_plans.rds")
    path_stats <- stringr::str_glue("{alarm_download_path()}/{slug}_stats.tab")

    if (isTRUE(stats)) {
        # farm out cache for stats to the stats fn
        d_stats <- alarm_japan_stats(pref, year = year, refresh = refresh)
        join_vars <- intersect(colnames(plans), colnames(d_stats))
        plans <- dplyr::left_join(plans, d_stats, by = join_vars)
    } else {
        plans <- readr::read_rds(file = path)
        }

    plans
}

#' @rdname alarm_japan
#' @export
alarm_japan_stats <- function(pref, year = 2022, refresh = FALSE) {
    slug <- get_slug_japan(pref, year = year)
    slug <- sub("^0", "", slug)
    path <- stringr::str_glue("{alarm_download_path()}/{slug}_stats.tab")

    stats <- readr::read_csv(path,
                                 col_types = readr::cols(draw = "f", district = "i", .default="d"),
                                 progress = FALSE,
                                 show_col_types = FALSE
        )
    stats
}


#' @rdname alarm_japan
#' @export
alarm_japan_doc <- function(pref, year = 2022) {
    slug <- get_slug_japan(pref, year = year)
    slug <- sub("_lh_2022$", "", slug)
    fname <- paste0("doc_", slug, ".md")

    raw <- dv_download_handle(fname, "Documentation", pref)
    if (is.null(raw)) cli::cli_abort("Download failed.")
    tmp_md <- tempfile(slug, fileext = ".md")
    writeBin(raw, tmp_md)

    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::viewer(tmp_md)
    } else {
        browseURL(tmp_md)
    }

    invisible(tmp_md)
}

dv_files_cache = list()

# try to download `fname` from the Japan 47-Prefecture dataverse
# Provide a human-readable error if the file doesn't exist.
dv_download_handle <- function(fname, type = "File", pref = "") {
    if (length(dv_files_cache) == 0) {
        full_files <- dataverse::dataset_files(DV_DOI, server = DV_SERVER)
        dv_files_cache[[1]] <- sapply(full_files, function(f) f$dataFile$id)
        names(dv_files_cache[[1]]) <- sapply(full_files, function(f) f$label)
    }

    raw <- NULL
    tryCatch(
        {
            raw <- dataverse::get_file_by_id(dv_files_cache[[1]][fname], server = DV_SERVER)
        },
        error = function(e) {
            if (stringr::str_detect(e$message, "[Nn]ot [Ff]ound")) {
                tryCatch(
                    {
                        dataverse::get_dataset(DV_DOI, server = DV_SERVER)
                    },
                    error = function(e) {
                        cli::cli_abort("Could not connect to Dataverse.
                               Check your API key and/or internet connection.", call = NULL)
                    })

                cli::cli_abort("{type} not found for {.val {pref}}.", call = NULL)
            } else {
                e
            }
        })
    raw
}


# parse `pref` and convert to a slug
get_slug_japan <- function(pref, type = "lh", year = 2022) {
    prefecture_codes <- c("01" = "hokkaido", "04" = "miyagi", "07" = "fukushima",
                          "08" = "ibaraki", "09" = "tochigi", "10" = "gunma",
                          "11" = "saitama", "12" = "chiba", "13" = "tokyo",
                          "14" = "kanagawa", "15" = "niigata", "21" = "gifu",
                          "22" = "shizuoka", "23" = "aichi", "25" = "shiga",
                          "27" = "osaka", "28" = "hyogo", "30" = "wakayama",
                          "32" = "shimane", "33" = "okayama", "34" = "hiroshima",
                          "35" = "yamaguchi", "38" = "ehime", "40" = "fukuoka",
                          "42" = "nagasaki")
    pref_num <- names(prefecture_codes[prefecture_codes == pref])
    if (length(pref_num) == 0)
        cli::cli_abort("Prefecture {.val {pref}} not found.", call = parent.frame())

    avail_years = c(2022)
    if (!year %in% avail_years) {
        cli::cli_abort("Only year{?s} {as.character(avail_years)} {?is/are} supported.",
                       call = parent.frame())
    }

    paste0(pref_num, "_", pref, "_", type, "_", as.integer(year))
}
