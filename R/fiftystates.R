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
#' @returns For `alarm_50state_map()`, a [redist_map][redist::redist_map]. For
#'   `alarm_50state_plans()`, a [redist_plans][redist::redist_plans]. For
#'   `alarm_50state_doc()`, nothing (but load an HTML file into the viewer or web
#'   browser).
#'
#' @examples \dontrun{
#' alarm_50state_map("WA")
#' alarm_50state_plans("WA", stats=FALSE)
#' alarm_50state_doc("WA")
#' }
#'
#' @name alarm_50state
NULL

DV_DOI = "doi:10.7910/DVN/SLCD3E"
DV_SERVER = "dataverse.harvard.edu"

single_states_polsby <- c("ak" = 0.06574469, "de" = 0.4595251, "nd" = 0.5142261, "sd" = 0.5576591, "vt" = 0.3692381, "wy" = 0.7721791)

#' @rdname alarm_50state
#' @export
alarm_50state_map = function(state, year=2020) {

    if (tolower(state) %in% names(single_states_polsby)) {
        make_state_map_one(state, stats = TRUE)
    } else {
    fname = paste0(get_slug(state, year=year), "_map.rds")
    raw = dv_download_handle(fname, "Map", state)

    read_rds_mem(raw, fname)
    }
}

#' @rdname alarm_50state
#' @export
alarm_50state_plans = function(state, stats=TRUE, year=2020) {

    if (tolower(state) %in% names(single_states_polsby)) {
        make_state_plans_one(state) %>% dplyr::mutate(comp_polsby = single_states_polsby[tolower(state)])

    } else {

        slug = get_slug(state, year=year)
        fname_plans = paste0(slug, "_plans.rds")

        raw_plans = dv_download_handle(fname_plans, "Plans", state)
        plans = read_rds_mem(raw_plans, fname_plans)

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
}


#' @rdname alarm_50state
#' @export
alarm_50state_doc = function(state, year=2020) {
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

