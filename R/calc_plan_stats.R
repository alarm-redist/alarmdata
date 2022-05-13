calc_plan_stats <- function(redist_plans, redist_map, calc_polsby = FALSE, ...) {

    redist_plans <- redist_plans %>%
        dplyr::mutate(total_vap = redist::tally_var(redist_map, .data$vap),
                      plan_dev =  redist::plan_parity(redist_map),
                      comp_edge = redist::distr_compactness(redist_map),
                      ndv = redist::tally_var(redist_map, .data$ndv),
                      nrv = redist::tally_var(redist_map, .data$nrv),
                      ndshare = .data$ndv / (.data$ndv + .data$nrv),
                      ...)

    if (calc_polsby == TRUE) {
        state <- redist_map$state[1]
        if (state %in% c("CA", "HI", "OR")) {
            shp <- tigris::tracts(censable::match_fips(state))
        } else {
            shp <- tigris::voting_districts(censable::match_fips(state))
        }
        redist_map <- redist_map %>%
            sf::st_drop_geometry() %>%
            dplyr::left_join(y = shp, by = c("GEOID" = "GEOID20")) %>%
            sf::st_as_sf() %>%
            sf::st_transform(crs = alarm_epsg(state))

        redist_plans <- redist_plans %>% dplyr::mutate(comp_polsby = redist::distr_compactness(redist_map, measure = "PolsbyPopper"))
    }

    tally_cols <- names(redist_map)[c(tidyselect::eval_select(tidyselect::starts_with("pop_"), redist_map),
                                      tidyselect::eval_select(tidyselect::starts_with("vap_"), redist_map),
                                      tidyselect::eval_select(tidyselect::matches("_(dem|rep)_"), redist_map),
                                      tidyselect::eval_select(tidyselect::matches("^a[dr]v_"), redist_map))]
    for (col in tally_cols) {
        redist_plans <- dplyr::mutate(redist_plans, {{ col }} := redist::tally_var(redist_map, redist_map[[col]]), .before = .data$ndv)
    }

    elecs <- dplyr::select(dplyr::as_tibble(redist_map), dplyr::contains("_dem_")) %>%
        names() %>%
        stringr::str_sub(1, 6) %>%
        unique()

    elect_tb <- do.call(dplyr::bind_rows, lapply(elecs, function(el) {
        vote_d = dplyr::select(dplyr::as_tibble(redist_map),
                        dplyr::starts_with(paste0(el, "_dem_")),
                        dplyr::starts_with(paste0(el, "_rep_")))
        if (ncol(vote_d) != 2) return(dplyr::tibble())
        dvote <- dplyr::pull(vote_d, 1)
        rvote <- dplyr::pull(vote_d, 2)

        redist_plans %>%
            dplyr::mutate(dem = redist::group_frac(redist_map, dvote, dvote + rvote),
                          egap = redist::partisan_metrics(redist_map, "EffGap", rvote, dvote),
                          pbias = redist::partisan_metrics(redist_map, "Bias", rvote, dvote)) %>%
            dplyr::as_tibble() %>%
            dplyr::group_by(.data$draw) %>%
            dplyr::transmute(draw = .data$draw,
                             district = .data$district,
                             e_dvs = .data$dem,
                             pr_dem = .data$dem > 0.5,
                             e_dem = sum(.data$dem > 0.5, na.rm=T),
                             pbias = -.data$pbias[1], # flip so dem = negative (only for old redist versioning)
                             egap = .data$egap[1])
    }))

    elect_tb <- elect_tb %>%
        dplyr::group_by(.data$draw, .data$district) %>%
        dplyr::summarize(dplyr::across(dplyr::everything(), mean))
    redist_plans <- dplyr::left_join(redist_plans, elect_tb, by = c("draw", "district"))

    split_cols <- names(redist_map)[tidyselect::eval_select(tidyselect::any_of(c("county", "muni")), redist_map)]
    for (col in split_cols) {
        if (col == "county") {
            redist_plans <- dplyr::mutate(redist_plans, county_splits = redist::county_splits(redist_map, .data$county), .before = .data$ndv)
        } else if (col == "muni") {
            redist_plans <- dplyr::mutate(redist_plans, muni_splits = redist::muni_splits(redist_map, .data$muni), .before = .data$ndv)
        } else {
            redist_plans <- dplyr::mutate(redist_plans, "{col}_splits" := redist::county_splits(redist_map, redist_map[[col]]), .before = .data$ndv)
        }
    }

    if (!("comp_polsby" %in% names(redist_plans))) {
        redist_plans <- redist_plans %>%
            dplyr::mutate(comp_polsby = NA_real_)
    }

    redist_plans <- redist_plans %>% dplyr::relocate(.data$comp_polsby, .after = .data$comp_edge)

    redist_plans

}
