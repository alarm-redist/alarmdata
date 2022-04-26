calc_plan_stats <- function(redist_plans, redist_map, ...) {

    redist_plans <- redist_plans %>%
        mutate(total_vap = tally_var(redist_map, vap),
               plan_dev =  plan_parity(redist_map),
               comp_edge = distr_compactness(redist_map),
               comp_polsby = distr_compactness(redist_map,
                                               measure = "PolsbyPopper"),
               ndv = tally_var(redist_map, ndv),
               nrv = tally_var(redist_map, nrv),
               ndshare = ndv / (ndv + nrv),
               ...)

    tally_cols <- names(redist_map)[c(tidyselect::eval_select(starts_with("pop_"), redist_map),
                                      tidyselect::eval_select(starts_with("vap_"), redist_map),
                                      tidyselect::eval_select(matches("_(dem|rep)_"), redist_map),
                                      tidyselect::eval_select(matches("^a[dr]v_"), redist_map))]
    for (col in tally_cols) {
        redist_plans <- mutate(redist_plans, {{ col }} := tally_var(redist_map, redist_map[[col]]), .before = ndv)
    }

    elecs <- select(as_tibble(redist_map), contains("_dem_")) %>%
        names() %>%
        str_sub(1, 6) %>%
        unique()

    elect_tb <- purrr::map_dfr(elecs, function(el) {
        vote_d = select(as_tibble(redist_map),
                        starts_with(paste0(el, "_dem_")),
                        starts_with(paste0(el, "_rep_")))
        if (ncol(vote_d) != 2) return(tibble())
        dvote <- pull(vote_d, 1)
        rvote <- pull(vote_d, 2)

        redist_plans %>%
            mutate(dem = group_frac(redist_map, dvote, dvote + rvote),
                   egap = partisan_metrics(redist_map, "EffGap", rvote, dvote),
                   pbias = partisan_metrics(redist_map, "Bias", rvote, dvote)) %>%
            as_tibble() %>%
            group_by(draw) %>%
            transmute(draw = draw,
                      district = district,
                      e_dvs = dem,
                      pr_dem = dem > 0.5,
                      e_dem = sum(dem > 0.5, na.rm=T),
                      pbias = -pbias[1], # flip so dem = negative (only for old redist versioning)
                      egap = egap[1])
    })

    elect_tb <- elect_tb %>%
        group_by(draw, district) %>%
        summarize(across(everything(), mean))
    redist_plans <- left_join(redist_plans, elect_tb, by = c("draw", "district"))

    split_cols <- names(redist_map)[tidyselect::eval_select(any_of(c("county", "muni")), redist_map)]
    for (col in split_cols) {
        if (col == "county") {
            redist_plans <- mutate(redist_plans, county_splits = county_splits(redist_map, county), .before = ndv)
        } else if (col == "muni") {
            redist_plans <- mutate(redist_plans, muni_splits = muni_splits(redist_map, muni), .before = ndv)
        } else {
            redist_plans <- mutate(redist_plans, "{col}_splits" := county_splits(redist_map, redist_map[[col]]), .before = ndv)
        }
    }

    redist_plans

}

#' Tally a variable by district
#'
#' @param redist_map a `redist_map` object
#' @param pop a variable to tally. Tidy-evaluated.
#' @param .data a `redist_plans` object
#'
#' @return a vector containing the tallied values by district and plan (column-major)
#' @export
tally_var <- function(redist_map, pop, .data = redist:::cur_plans()) {

    redist:::check_tidy_types(redist_map, .data)
    if (length(unique(diff(as.integer(.data$district)))) > 2)
        warning("Districts not sorted in ascending order; output may be incorrect.")
    idxs <- unique(as.integer(.data$draw))
    pop <- rlang::eval_tidy(rlang::enquo(pop), redist_map)
    as.numeric(redist:::pop_tally(get_plans_matrix(.data)[, idxs, drop = FALSE],
                                  pop, attr(redist_map, "ndists")))

}
