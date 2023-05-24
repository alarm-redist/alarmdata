make_state_map_one <- function(state, year = 2020L, geometry = TRUE, epsg = alarm_epsg(state)) {
    if (year == 2010L) {

        nd <- geomander::get_alarm(state = state, year = year, geometry = FALSE, epsg = epsg)

        st_name <- ifelse(year == 2020, 'STUSPS', 'STUSPS10')
        if (geometry) {
            add_geo <- tinytiger::tt_states(year = year) %>%
                dplyr::filter(.data[[st_name]] == .env$state) %>%
                sf::st_transform(epsg) %>%
                sf::st_geometry()
        } else {
            add_geo <- sf::st_sfc(sf::st_polygon())
        }
        nd <- nd %>%
            dplyr::summarise(
                dplyr::across(tidyselect::starts_with("pop"), sum),
                dplyr::across(tidyselect::starts_with("vap"), sum),
                dplyr::across(tidyselect::matches("_(dem|rep)_"), sum),
                dplyr::across(tidyselect::matches("^a[dr]v_"), sum),
                geometry =  add_geo
            )

        if (!geometry) {
            nd <- sf::st_sf(nd, crs = epsg)
        }

        id_nrv <- stringr::str_detect(colnames(nd), "arv_")
        id_ndv <- stringr::str_detect(colnames(nd), "adv_")

        nd <- nd %>%
            dplyr::mutate(nrv = rowMeans(as.data.frame(nd)[,id_nrv]),
                          ndv = rowMeans(as.data.frame(nd)[,id_ndv]),
                          nrv = round(.data$nrv, 1),
                          ndv = round(.data$ndv, 1)) %>%
            dplyr::relocate(geometry, .after = .data$ndv)

        nd <- nd %>%
            dplyr::mutate(cd_2010 = 1L, .before = dplyr::everything())

        map <- suppressWarnings(redist::redist_map(nd, existing_plan = "cd_2010", pop_tol = 0.005, adj = list(integer())))
        map <- map %>%
            dplyr::mutate(
                GEOID = censable::match_fips(.env$state),
                state = as.character(.env$state), .before = dplyr::everything()
            )
        attr(map, "analysis_name") <- paste0(censable::match_abb(state), "_", year)
    } else {
        map <- maps[[paste0(state, "_2020")]]

        if (!geometry) {
            map$geometry <- sf::st_sfc(sf::st_polygon())
        }
    }

    map
}

make_state_plans_one <- function(state, stats = TRUE, year = 2020L, geometry = TRUE, epsg = alarm_epsg(state)) {
    nsims <- 5000

    m <- matrix(1L, nrow = 1, ncol = nsims)
    map <- make_state_map_one(state, year = year, geometry = geometry, epsg = alarm_epsg(state))

    pl <- redist::redist_plans(m, map, algorithm = "Single", wgt = NULL)
    pl <- redist::add_reference(pl, ref_plan = redist::get_existing(map),
        name = attr(map, "existing_col"))

    if (stats) {
        pl <- suppressMessages(calc_plan_stats(pl, map))
    }

    pl
}
