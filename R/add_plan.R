#' Add a reference plan to a set of plans
#'
#' Facilitates comparing an existing (i.e., non-simulated) redistricting plan to a set of simulated plans.
#'
#' @param plans A `redist_plans` object.
#' @param ref_plan An integer vector containing the reference plan or a block assignment file as a `tibble` or `data.frame`.
#' @param map A `redist_map` object. Only required if the `redist_plans` object includes summary statistics.
#' @param name A human-readable name for the reference plan. Defaults to the name of `ref_plan`. If `ref_plan` is a
#' `tibble` or `data.frame`, it should be the name of the column of `ref_plan` that identifies districts.
#' @param calc_polsby A logical value indicating whether a Polsby-Popper compactness score should be calculated for the reference plan. Defaults to `FALSE`.
#' @param GEOID character. Ignored unless `ref_plan` is a `tibble` or `data.frame`.
#' Should correspond to the column of `ref_plan` that identifies block `GEOID`s.
#' Default is `'GEOID'`.
#' @param year the decade to request if passing a `tibble` to `ref_plan`, either `2010` or `2020`. Default is `2020`.
#'
#' @returns A modified `redist_plans` object containing the reference plan. Includes summary statistics if the original `redist_plans` object had them as well.
#' @export
#'
#' @examplesIf Sys.getenv("DATAVERSE_KEY") != ''
#' # requires Harvard Dataverse API key
#' map <- alarm_50state_map("WY")
#' pl <- alarm_50state_plans("WY")
#' pl_new <- alarm_add_plan(pl, ref_plan = c(1), map, name = "example")
#'
#' # download and load a comparison plan
#' url <- paste0("https://github.com/PlanScore/Redistrict2020/raw/main/files/",
#'   "NM-2021-10/Congressional_Concept_A.zip")
#' tf <- tempfile(fileext = ".zip")
#' utils::download.file(url, tf)
#' utils::unzip(tf, exdir = dirname(tf))
#' baf <- readr::read_csv(file = paste0(dirname(tf), "/Congressional Concept A.csv"),
#'                        col_types = "ci")
#' names(baf) <- c("GEOID", "concept_a")
#' # Add it to the plans object
#' map_nm <- alarm_50state_map("NM")
#' plans_nm <- alarm_50state_plans("NM", stats = FALSE)
#' alarm_add_plan(plans_nm, baf, map = map_nm, name = "concept_a")
#'
alarm_add_plan <- function(plans, ref_plan, map = NULL, name = NULL,
                           calc_polsby = FALSE, GEOID = "GEOID", year = 2020) {
    # redist_plans object already has summary statistics, so they must be calculated for ref_plan as well
    if (!inherits(plans, "redist_plans"))
        cli_abort("{.arg plans} must be a {.cls redist_plans}")
    if (isTRUE(attr(plans, "partial")))
        cli_abort("Reference plans not supported for partial plans objects")

    if (is.null(name)) {
        ref_str <- deparse(substitute(ref_plan))
        if (stringr::str_detect(ref_str, stringr::fixed("$"))) {
            name <- strsplit(ref_str, "$", fixed = TRUE)[[1]][2]
        } else {
            name <- ref_str
        }
    } else if (!is.character(name)) {
        cli_abort("{.arg name} must be a {.cls chr}")
    }

    if (name %in% levels(plans$draw)) {
        cli_abort("Reference plan name already exists")
    }

    if (!is.numeric(ref_plan)) {
        if (is.data.frame(ref_plan)) {
            if (is.null(map)) {
                cli::cli_abort("{.arg map} must be provided to use a {.cls data.frame} for {.arg ref_plan}.")
            }
            if (year != 2020 && utils::packageVersion('geomander') < '2.3.0') {
                cli::cli_abort('geomander must be updated to use {.arg year} != 2020')
            }
            if (utils::packageVersion('geomander') < '2.3.0') {
                ref_plan <- geomander::baf_to_vtd(ref_plan, name, GEOID)
            } else {
                ref_plan <- geomander::baf_to_vtd(ref_plan, name, GEOID, year = year)
            }

            ref_plan <- ref_plan[[name]][match(ref_plan[[GEOID]], map[[names(map)[stringr::str_detect(names(map), "GEOID")][1]]])]
        } else {
            cli_abort("{.arg ref_plan} must be numeric or inherit {.cls data.frame}.")
        }
    }

    if (length(ref_plan) != nrow(redist::get_plans_matrix(plans)))
        cli_abort("{.arg ref_plan} must have the same number of precincts as {.arg plans}")
    if (dplyr::n_distinct(ref_plan) != dplyr::n_distinct(plans$district)) {
        cli::cli_abort("{.arg ref_plan} must have the same number of districts as {.arg plans}")
    } else {
        if (max(ref_plan) != dplyr::n_distinct(ref_plan)) {
            ref_plan <- match(ref_plan, unique(sort(ref_plan, na.last = TRUE)))
            cli::cli_warn(c("{.arg ref_plan} should be numbered {{1, 2, ..., ndists}}.",
                "i" = "{.arg ref_plan} was renumbered based on the order of entries."))
        }
    }

    if ("comp_polsby" %in% names(plans)) {
        if (is.null(map)) {
            cli_abort("{.arg map} must be a {.cls redist_map} in order to calculate summary statistics for the provided reference plan.")
        }

        ref_redist_plan <- redist::redist_plans(
            plans = ref_plan, map = map, algorithm = attr(plans, "algorithm")
        )
        ref_plan_stats <- calc_plan_stats(ref_redist_plan, map, calc_polsby)

        ref_plan_stats$draw <- name
        if ("chain" %in% names(plans)) ref_plan_stats$chain <- NA
        attr(ref_plan_stats, "resampled") <- attr(plans, "resampled")
        attr(ref_plan_stats, "compactness") <- attr(plans, "compactness")
        attr(ref_plan_stats, "constraints") <- attr(plans, "constraints")

        if (is.null(attr(plans, "ndists"))) {
            attr(plans, "ndists") <- max(as.matrix(plans)[, 1])
        }
        attr(ref_plan_stats, "ndists") <- attr(plans, "ndists")

        new_plans <- rbind(ref_plan_stats, plans)
        m <- redist::get_plans_matrix(ref_plan_stats)
        colnames(m)[1] <- name
        attr(new_plans, "plans") <- cbind(m, redist::get_plans_matrix(plans))
        new_plans$draw = factor(new_plans$draw, levels=unique(new_plans$draw))

        new_plans

    } else { # just add reference
        redist::add_reference(plans, ref_plan, name)
    }
}
