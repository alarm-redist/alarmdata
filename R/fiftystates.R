#' Download maps and plans from the 50-State Simulation Project
#'
#' These functions will download [redist_map][redist::redist_map] and
#' [redist_plans][redist::redist_plans] objects for the 50-State Simulation
#' Project from the ALARM Project's Dataverse. `alarm_50state_doc()` will
#' download documentation for a particular state and show it in a browser.
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
#' @param year The redistricting cycle to download. Currently only "2020" is available.
#' @param stats if `TRUE` (the default), download summary statistics for each plan.
#'
#'
#' @name alarm_50state
NULL

DV_DOI = "doi:10.7910/DVN/SLCD3E"
DV_SERVER = "dataverse.harvard.edu"

#' @rdname alarm_50state
#' @export
alarm_50state_map = function(state, year=2020) {
    fname = paste0(get_slug(state, year=year), "_map.rds")
    raw = dv_download_handle(fname, "Map", state)

    comp_fmt = id_compression(raw)
    if (is.na(comp_fmt))
        cli_abort(c("Map file has unknown compression format.",
                    ">"="Please file an issue at {.url https://github.com/alarm-redist/fifty-states/issues}",
                    ">"="Provide filename {.val {fname}}"))
    # magic so that we don't have to write to disk first
    readRDS(gzcon(rawConnection(memDecompress(raw, type=comp_fmt))))
}

#' @rdname alarm_50state
#' @export
alarm_50state_plans = function(state, stats=TRUE, year=2020) {
    slug = get_slug(state, year=year)
    fname_plans = paste0(slug, "_plans.rds")

    raw_plans = dv_download_handle(fname_plans, "Plans", state)
    comp_fmt = id_compression(raw_plans)
    if (is.na(comp_fmt))
        cli_abort(c("Plans file has unknown compression format.",
                    ">"="Please file an issue at {.url https://github.com/alarm-redist/fifty-states/issues}",
                    ">"="Provide filename {.val {fname_plans}}"))
    # magic so that we don't have to write to disk first
    plans = readRDS(gzcon(rawConnection(memDecompress(raw_plans, type=comp_fmt))))

    if (isTRUE(stats)) {
        fname_stats = paste0(slug, "_stats.tab")
        raw_stats = dv_download_handle(fname_stats, "Plan statistics", state)
        d_stats = readr::read_csv(raw_stats,
                                  col_types=readr::cols(draw="f", district="i"),
                                  show_col_types=FALSE)
        plans = dplyr::left_join(plans, d_stats, by=c("draw", "district", "total_pop"))
    }

    plans
}


#' @rdname alarm_50state
#' @export
alarm_50state_doc = function(state, stats=TRUE, year=2020) {
    slug = get_slug(state, year=year)
    fname = paste0(slug, "_doc.html")

    raw = dv_download_handle(fname, "Documentation", state)
    tmp_html = tempfile(slug, fileext=".html")
    writeBin(raw, tmp_html)

    if (requireNamespace("rstudioapi", quietly=TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::viewer(tmp_html)
    } else {
        browseURL(tmp_html)
    }
}

# try to download `fname` from the 50-states dataverse
# Provide a human-readable error if the file doesn't exist.
dv_download_handle = function(fname, type="File", state="") {
    tryCatch({
        raw = dataverse::get_file_by_name(fname, DV_DOI, server=DV_SERVER)
    }, error=function(e) {
        if (e$message == "File not found")
            cli::cli_abort("{type} not found for {.val {state}}.")
        else
            e
    })
    raw
}

