calc_plan_stats <- function(plans, map, calc_polsby = FALSE, ...) {

    plans <- plans %>%
        dplyr::mutate(total_vap = redist::tally_var(map, .data$vap),
                      plan_dev =  redist::plan_parity(map),
                      comp_edge = redist::distr_compactness(map),
                      ndv = redist::tally_var(map, .data$ndv),
                      nrv = redist::tally_var(map, .data$nrv),
                      ndshare = .data$ndv / (.data$ndv + .data$nrv),
                      ...)

    if (calc_polsby == TRUE) {
        state <- map$state[1]
        single_states_polsby <- c("AK" = 0.06574469, "DE" = 0.4595251, "ND" = 0.5142261, "SD" = 0.5576591, "VT" = 0.3692381, "WY" = 0.7721791)
        if (toupper(state) %in% names(single_states_polsby)) {
            plans <- plans %>% dplyr::mutate(comp_polsby = single_states_polsby[toupper(state)])
        } else {
            if (state %in% c("CA", "HI", "OR")) {
                shp <- tigris::tracts(censable::match_fips(state))
            } else {
                shp <- tigris::voting_districts(censable::match_fips(state))
            }

            map <- map %>%
                sf::st_drop_geometry() %>%
                dplyr::left_join(y = shp, by = c("GEOID" = "GEOID20")) %>%
                sf::st_as_sf() %>%
                sf::st_transform(crs = alarm_epsg(state))

            plans <- plans %>% dplyr::mutate(comp_polsby = redist::distr_compactness(map, measure = "PolsbyPopper"))
        }
    }

    tally_cols <- names(map)[c(tidyselect::eval_select(tidyselect::starts_with("pop_"), map),
                                      tidyselect::eval_select(tidyselect::starts_with("vap_"), map),
                                      tidyselect::eval_select(tidyselect::matches("_(dem|rep)_"), map),
                                      tidyselect::eval_select(tidyselect::matches("^a[dr]v_"), map))]
    for (col in tally_cols) {
        plans <- dplyr::mutate(plans, {{ col }} := redist::tally_var(map, map[[col]]), .before = .data$ndv)
    }

    elecs <- dplyr::select(dplyr::as_tibble(map), dplyr::contains("_dem_")) %>%
        names() %>%
        stringr::str_sub(1, 6) %>%
        unique()

    elect_tb <- do.call(dplyr::bind_rows, lapply(elecs, function(el) {
        vote_d = dplyr::select(dplyr::as_tibble(map),
                        dplyr::starts_with(paste0(el, "_dem_")),
                        dplyr::starts_with(paste0(el, "_rep_")))
        if (ncol(vote_d) != 2) return(dplyr::tibble())
        dvote <- dplyr::pull(vote_d, 1)
        rvote <- dplyr::pull(vote_d, 2)

        plans %>%
            dplyr::mutate(dem = redist::group_frac(map, dvote, dvote + rvote),
                          egap = redist::partisan_metrics(map, "EffGap", rvote, dvote),
                          pbias = redist::partisan_metrics(map, "Bias", rvote, dvote)) %>%
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
    plans <- dplyr::left_join(plans, elect_tb, by = c("draw", "district"))

    split_cols <- names(map)[tidyselect::eval_select(tidyselect::any_of(c("county", "muni")), map)]
    for (col in split_cols) {
        if (col == "county") {
            plans <- dplyr::mutate(plans, county_splits = redist::county_splits(map, .data$county), .before = .data$ndv)
        } else if (col == "muni") {
            plans <- dplyr::mutate(plans, muni_splits = redist::muni_splits(map, .data$muni), .before = .data$ndv)
        } else {
            plans <- dplyr::mutate(plans, "{col}_splits" := redist::county_splits(map, map[[col]]), .before = .data$ndv)
        }
    }

    if (!("comp_polsby" %in% names(plans))) {
        plans <- plans %>%
            dplyr::mutate(comp_polsby = NA_real_)
    }

    plans <- plans %>% dplyr::relocate(.data$comp_polsby, .after = .data$comp_edge)

    plans

}
