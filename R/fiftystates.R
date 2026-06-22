#' Download maps and plans from the 50-State Simulation Project
#'
#' These functions will download [redist_map][redist::redist_map] and
#' [redist_plans][redist::redist_plans] objects for the 50-State Simulation
#' Project from the ALARM Project's Dataverse. `alarm_50state_doc()` will
#' download documentation for a particular state and show it in a browser.
#' `alarm_50state_stats` will download just the summary statistics for a state.
#'
#' Every decade following the Census, states and municipalities must redraw
#' districts for Congress, state houses, city councils, and more. The goal of
#' the 50-State Simulation Project is to enable researchers, practitioners, and
#' the general public to use cutting-edge redistricting simulation analysis to
#' evaluate enacted congressional districts.
#'
#' Evaluating a redistricting plan requires analysts to take into account each
#' state’s redistricting rules and particular political geography. Comparing the
#' partisan bias of a plan for Texas with the bias of a plan for New York, for
#' example, is likely misleading. Comparing a state’s current plan to a past
#' plan is also problematic because of demographic and political changes over
#' time. Redistricting simulations generate an ensemble of alternative
#' redistricting plans within a given state which are tailored to its
#' redistricting rules. Unlike traditional evaluation methods, therefore,
#' simulations are able to directly account for the state’s political geography
#' and redistricting criteria.
#'
#' @template state
#' @param year The redistricting cycle to download. Currently only `2020` and `2010` are available.
#' @param stats If `TRUE` (the default), download summary statistics for each plan.
#' @param refresh If `TRUE`, ignore the cache and download again.
#' @param compress The compression level used for caching [redist_plans][redist::redist_plans] objects.
#' @param callais If `TRUE`, download the post-Callais 2020 simulations, in which
#'   race-based constraints were removed, from the Callais Dataverse
#'   (`doi:10.7910/DVN/A3SE7Y`). Only available for `year = 2020`, and only the
#'   states that were re-simulated differ from the standard data; any other
#'   state (including single-district states) falls back to the standard 2020
#'   data. Defaults to `FALSE`.
#'
#' @returns For `alarm_50state_map()`, a [redist_map][redist::redist_map]. For
#'   `alarm_50state_plans()`, a [redist_plans][redist::redist_plans]. For
#'   `alarm_50state_doc()`, invisibly returns the path to the HTML documentation,
#'   and also loads an HTML file into the viewer or web browser.
#'   For `alarm_50state_stats()`, a [tibble][dplyr::tibble].
#'
#' @examplesIf Sys.getenv("DATAVERSE_KEY") != ''
#'
#' # requires Harvard Dataverse API key
#' alarm_50state_map("WA")
#' alarm_50state_plans("WA", stats = FALSE)
#' alarm_50state_stats("WA")
#' alarm_50state_doc("WA")
#'
#' map <- alarm_50state_map("WY")
#' pl <- alarm_50state_plans("WY")
#'
#' @name alarm_50state
NULL

DV_DOI_50s <- function(year, callais = FALSE) {
  if (isTRUE(callais)) {
    "doi:10.7910/DVN/A3SE7Y" # Callais Dataverse
  } else if (year == 2000) {
    "doi:10.7910/DVN/LV7VIX"
  } else {
    "doi:10.7910/DVN/SLCD3E"
  }
}

# States whose post-Callais 2020 ensembles actually differ from the standard
# data: their original fifty-states analysis applied a race-based constraint
# (passed to `redist_smc()`) that the Callais run removes. Only these states
# exist on the Callais Dataverse; any other state falls back to the standard
# 2020 data, which is unchanged.
#
# Excluded despite a re-run script: CO/MA/MN/VA/WI had no race constraint in the
# original (re-run only for sampler/convergence reasons), and IL built a `constr`
# object that was never passed to `redist_smc()` in the original IL_2020 script,
# so removing it changes nothing.
CALLAIS_STATES <- c("AL", "AZ", "CA", "FL", "GA", "LA", "MI", "MO", "MS", "NC",
                    "OH", "RI", "SC", "TX", "WA")

# Resolve whether the Callais data should actually be used: only when requested,
# only for 2020, and only for states that were re-simulated. Otherwise fall back
# to the standard behavior (download from the main Dataverse or build
# single-district states locally).
use_callais <- function(state, year, callais) {
  isTRUE(callais) && as.integer(year) == 2020L &&
    censable::match_abb(state) %in% CALLAIS_STATES
}

#' @rdname alarm_50state
#' @export
alarm_50state_map <- function(state, year = 2020, refresh = FALSE, callais = FALSE) {
    requireNamespace('sf', quietly = TRUE)
    callais <- use_callais(state, year, callais)
    slug <- get_slug(state, year = year, callais = callais)
    path <- stringr::str_glue("{alarm_download_path()}/{slug}_map.rds")

    if (!file.exists(path) || isTRUE(refresh)) {
        if ((toupper(state) %in% c("AK", "DE", "ND", "SD", "VT", "WY") && year == 2020L) ||
            (toupper(state) %in% c("AK", "DE", "MT", "ND", "SD", "VT", "WY") && year %in% c(2000L, 2010L))) {
            out <- make_state_map_one(state, year = year)
        } else {
            fname <- paste0(slug, "_map.rds")
            raw <- dv_download_handle(fname, "Map", state, year, callais = callais)
            if (is.null(raw)) cli::cli_abort("Download failed.")

            out <- read_rds_mem(raw, fname)
            writeBin(raw, path)
        }
    } else {
        out <- readr::read_rds(file = path)
    }
    out
}

#' @rdname alarm_50state
#' @export
alarm_50state_plans <- function(state, stats = TRUE, year = 2020, refresh = FALSE, compress = "xz", callais = FALSE) {
    callais <- use_callais(state, year, callais)
    slug <- get_slug(state, year = year, callais = callais)
    path <- stringr::str_glue("{alarm_download_path()}/{slug}_plans.rds")
    path_stats <- stringr::str_glue("{alarm_download_path()}/{slug}_stats.csv")

    if (!file.exists(path) || isTRUE(refresh)) {

        single_states_polsby <- c("AK" = 0.06574469, "DE" = 0.4595251,
                                  "MT" = 0.4813638,
                                  "ND" = 0.5142261, "SD" = 0.5576591,
                                  "VT" = 0.3692381, "WY" = 0.7721791)
        if (year == 2020) {
            single_states_polsby <- single_states_polsby[-3]
        }

        if (toupper(state) %in% names(single_states_polsby)) {
            plans <- make_state_plans_one(state, year = year, stats = stats)
            if (stats) {
                plans <- plans %>%
                    dplyr::mutate(comp_polsby = single_states_polsby[toupper(state)])
            }
            readr::write_csv(plans, file = path)
        } else {
            fname_plans <- paste0(slug, "_plans.rds")

            raw_plans <- dv_download_handle(fname_plans, "Plans", state, year, callais = callais)
            if (is.null(raw_plans)) cli::cli_abort("Download failed.")
            plans <- read_rds_mem(raw_plans, fname_plans) %>%
                dplyr::mutate(district = as.integer(.data$district))
        }

        readr::write_rds(plans, file = path, compress = compress)
    } else {
        plans <- readr::read_rds(file = path)
    }

    if (isTRUE(stats)) {
        # farm out cache for stats to the stats fn
        d_stats <- alarm_50state_stats(state, year = year, refresh = refresh, callais = callais)
        # rounding errors will cause bad join
        if ('pop_overlap' %in% colnames(plans) && 'pop_overlap' %in% colnames(d_stats)) {
            d_stats$pop_overlap <- NULL
        }
        join_vars <- intersect(colnames(plans), colnames(d_stats))
        plans <- dplyr::left_join(plans, d_stats, by = join_vars)
    }

    plans
}

#' @rdname alarm_50state
#' @export
alarm_50state_stats <- function(state, year = 2020, refresh = FALSE, callais = FALSE) {
    callais <- use_callais(state, year, callais)
    slug <- get_slug(state, year = year, callais = callais)
    path <- stringr::str_glue("{alarm_download_path()}/{slug}_stats.csv")

    if (!file.exists(path) || isTRUE(refresh)) {

        state <- censable::match_abb(state)
        if (length(state) != 1) {
            cli_abort(c("{.arg state} could not be matched to a single state.",
                        "x" = "Please make {arg state} correspond to the name, abbreviation, or FIPS of one state."
            ))
        }

        single_states_polsby <- c("AK" = 0.06574469, "DE" = 0.4595251, "ND" = 0.5142261,
                                  "MT" = 0.4813638,
                                  "SD" = 0.5576591, "VT" = 0.3692381, "WY" = 0.7721791)
        if (year == 2020) {
            single_states_polsby <- single_states_polsby[-4]
        }
        if (state %in% names(single_states_polsby)) {
            stats <- make_state_plans_one(state, year = year, geometry = FALSE, stats = TRUE) %>%
                dplyr::mutate(comp_polsby = single_states_polsby[toupper(state)]) %>%
                dplyr::as_tibble()
            readr::write_csv(stats, file = path)
        } else {
            slug <- get_slug(state, year = year, callais = callais)
            fname_stats <- paste0(slug, "_stats.tab")
            raw_stats <- dv_download_handle(fname_stats, "Plan statistics", state, year, callais = callais)
            if (is.null(raw_stats)) cli::cli_abort("Download failed.")

            stats <- readr::read_csv(raw_stats,
                                     col_types = readr::cols(draw = "f", district = "i", .default="d"),
                                     progress = FALSE,
                                     show_col_types = FALSE
            )
            readr::write_csv(stats, file = path)
        }
    } else {
        stats <- readr::read_csv(path,
                                 col_types = readr::cols(draw = "f", district = "i", .default="d"),
                                 progress = FALSE,
                                 show_col_types = FALSE
        )
    }
    stats
}


#' @rdname alarm_50state
#' @export
alarm_50state_doc <- function(state, year = 2020, callais = FALSE) {
    callais <- use_callais(state, year, callais)
    slug <- get_slug(state, year = year, callais = callais)
    fname <- paste0(slug, "_doc.html")

    raw <- dv_download_handle(fname, "Documentation", state, year, callais = callais)
    if (is.null(raw)) cli::cli_abort("Download failed.")
    tmp_html <- tempfile(slug, fileext = ".html")
    writeBin(raw, tmp_html)

    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::viewer(tmp_html)
    } else {
        browseURL(tmp_html)
    }

    invisible(tmp_html)
}

dv_files_cache = list()

# try to download `fname` from the 50-states dataverse
# Provide a human-readable error if the file doesn't exist.
dv_download_handle <- function(fname, type = "File", state = "", year, callais = FALSE) {
    doi <- DV_DOI_50s(year, callais = callais)
    if (is.null(dv_files_cache[[doi]])) {
        full_files <- dataverse::dataset_files(doi, server = DV_SERVER)
        ids <- sapply(full_files, function(f) f$dataFile$id)
        names(ids) <- sapply(full_files, function(f) f$label)
        dv_files_cache[[doi]] <- ids
    }

    raw <- NULL
    tryCatch(
        {
            raw <- dataverse::get_file_by_id(dv_files_cache[[doi]][fname], server = DV_SERVER)
        },
        error = function(e) {
            if (stringr::str_detect(e$message, "[Nn]ot [Ff]ound")) {
                tryCatch(
                    {
                        dataverse::get_dataset(doi, server = DV_SERVER)
                    },
                    error = function(e) {
                        cli::cli_abort("Could not connect to Dataverse.
                               Check your API key and/or internet connection.", call = NULL)
                    })

                cli::cli_abort("{type} not found for {.val {state}}.", call = NULL)
            } else {
                e
            }
        })
    raw
}
