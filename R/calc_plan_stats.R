calc_plan_stats <- function(redist_plans, redist_map, calc_polsby = FALSE, ...) {

    redist_plans <- redist_plans %>%
        dplyr::mutate(total_vap = redist::tally_var(redist_map, .data$vap),
                      plan_dev =  plan_parity(redist_map),
                      comp_edge = distr_compactness(redist_map),
                      ndv = redist::tally_var(redist_map, .data$ndv),
                      nrv = redist::tally_var(redist_map, .data$nrv),
                      ndshare = .data$ndv / (.data$ndv + .data$nrv),
                      ...)

    if (calc_polsby == TRUE) {
        state <- redist_map$state[1]
        if (state %in% c("CA", "HI", "OR")) {
            shp <- tigris::tracts(state = state)
        } else {
            shp <- tigris::voting_districts(state = state)
        }
        redist_map <- redist_map %>%
            sf::st_drop_geometry() %>%
            left_join(y = shp, by = c("GEOID" = "GEOID20")) %>%
            sf::st_as_sf() %>%
            sf::st_transform(crs = alarm_epsg(state))

        redist_plans <- redist_plans %>% dplyr::mutate(comp_polsby = distr_compactness(redist_map, measure = "PolsbyPopper"))
    }

    tally_cols <- names(redist_map)[c(tidyselect::eval_select(tidyselect::starts_with("pop_"), redist_map),
                                      tidyselect::eval_select(tidyselect::starts_with("vap_"), redist_map),
                                      tidyselect::eval_select(matches("_(dem|rep)_"), redist_map),
                                      tidyselect::eval_select(matches("^a[dr]v_"), redist_map))]
    for (col in tally_cols) {
        redist_plans <- dplyr::mutate(redist_plans, {{ col }} := redist::tally_var(redist_map, redist_map[[col]]), .before = .data$ndv)
    }

    elecs <- dplyr::select(as_tibble(redist_map), contains("_dem_")) %>%
        names() %>%
        str_sub(1, 6) %>%
        unique()

    elect_tb <- do.call(dplyr::bind_rows, lapply(elecs, function(el) {
        vote_d = select(as_tibble(redist_map),
                        starts_with(paste0(el, "_dem_")),
                        starts_with(paste0(el, "_rep_")))
        if (ncol(vote_d) != 2) return(tibble())
        dvote <- pull(vote_d, 1)
        rvote <- pull(vote_d, 2)

        redist_plans %>%
            dplyr::mutate(dem = group_frac(redist_map, dvote, dvote + rvote),
                          egap = partisan_metrics(redist_map, "EffGap", rvote, dvote),
                          pbias = partisan_metrics(redist_map, "Bias", rvote, dvote)) %>%
            as_tibble() %>%
            group_by(draw) %>%
            dplyr::transmute(draw = .data$draw,
                             district = .data$district,
                             e_dvs = .data$dem,
                             pr_dem = .data$dem > 0.5,
                             e_dem = sum(.data$dem > 0.5, na.rm=T),
                             pbias = -.data$pbias[1], # flip so dem = negative (only for old redist versioning)
                             egap = .data$egap[1])
    }))

    elect_tb <- elect_tb %>%
        group_by(draw, district) %>%
        summarize(across(everything(), mean))
    redist_plans <- left_join(redist_plans, elect_tb, by = c("draw", "district"))

    split_cols <- names(redist_map)[tidyselect::eval_select(any_of(c("county", "muni")), redist_map)]
    for (col in split_cols) {
        if (col == "county") {
            redist_plans <- dplyr::mutate(redist_plans, county_splits = county_splits(redist_map, county), .before = ndv)
        } else if (col == "muni") {
            redist_plans <- dplyr::mutate(redist_plans, muni_splits = muni_splits(redist_map, muni), .before = ndv)
        } else {
            redist_plans <- dplyr::mutate(redist_plans, "{col}_splits" := county_splits(redist_map, redist_map[[col]]), .before = ndv)
        }
    }

    if (!("comp_polsby" %in% names(redist_plans))) {
        redist_plans <- redist_plans %>%
            dplyr::mutate(comp_polsby = NA_real_)
    }

    redist_plans <- redist_plans %>% dplyr::relocate(comp_polsby, .after = comp_edge)

    redist_plans

}
