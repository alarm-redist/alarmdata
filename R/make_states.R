make_state_map_one <- function(state, geometry = TRUE, epsg = alarm_epsg(state)) {

    nd <- geomander::get_alarm(state = state, geometry = geometry, epsg = epsg)

    nd <- nd %>%
        dplyr::summarise(dplyr::across(tidyselect::starts_with("pop"), sum),
                  dplyr::across(tidyselect::starts_with("vap"), sum),
                  dplyr::across(tidyselect::matches("_(dem|rep)_"), sum),
                  dplyr::across(tidyselect::matches("^a[dr]v_"), sum),
                  geometry = sf::st_union(geometry))

    id_nrv <- stringr::str_detect(colnames(nd), "arv_")
    id_ndv <- stringr::str_detect(colnames(nd), "adv_")

    nd <- nd %>%
        dplyr::mutate(nrv = rowMeans(as.data.frame(nd)[,id_nrv]),
               ndv = rowMeans(as.data.frame(nd)[,id_ndv]),
               nrv = round(.data$nrv, 1),
               ndv = round(.data$ndv, 1)) %>%
        dplyr::relocate(geometry, .after = .data$ndv)

    suppressWarnings(redist::redist_map(nd, ndists = 1, pop_tol = 0.01))
}

make_state_plans_one <- function(state, stats = TRUE) {

    m <- matrix(1, nrow = 1, ncol = 5000)
    map <- make_state_map_one(state)

    pl <- redist::redist_plans(m, map, algorithm = "Single", wgt = NULL)

    if (stats) {
        pl <- calc_plan_stats(pl, map)
    }

    pl
}
