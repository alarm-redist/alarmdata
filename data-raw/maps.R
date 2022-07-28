devtools::load_all()

make_state_map_one <- function(state, geometry = TRUE, epsg = alarm_epsg(state)) {
    year <- 2020

    nd <- geomander::get_alarm(state = state, geometry = geometry, epsg = epsg)

    if (isTRUE(geometry)) {
        obj_geom = sf::st_union(nd$geometry) %>%
            rmapshaper::ms_simplify(keep_shapes=TRUE)
    } else {
        obj_geom = sf::st_sfc(sf::st_polygon())
    }

    nd <- nd %>%
        dplyr::summarise(dplyr::across(tidyselect::starts_with("pop"), sum),
                         dplyr::across(tidyselect::starts_with("vap"), sum),
                         dplyr::across(tidyselect::matches("_(dem|rep)_"), sum),
                         dplyr::across(tidyselect::matches("^a[dr]v_"), sum),
                         geometry = obj_geom)

    if (!geometry) {
        nd <- sf::st_sf(nd, crs = epsg)
    }

    id_nrv <- stringr::str_detect(colnames(nd), "arv_")
    id_ndv <- stringr::str_detect(colnames(nd), "adv_")

    nd <- nd %>%
        dplyr::mutate(
            nrv = rowMeans(as.data.frame(nd)[, id_nrv]),
            ndv = rowMeans(as.data.frame(nd)[, id_ndv]),
            nrv = round(.data$nrv, 1),
            ndv = round(.data$ndv, 1)
        ) %>%
        dplyr::relocate(geometry, .after = .data$ndv)

    nd <- nd %>%
        dplyr::mutate(cd_2020 = 1L, .before = dplyr::everything())

    map <- suppressWarnings(redist::redist_map(nd, existing_plan = "cd_2020", pop_tol = 0.005, adj = list(integer())))
    map$state <- state

    attr(map, "analysis_name") <- paste0(censable::match_abb(state), "_", year)


    map
}

maps <- lapply(c("AK", "DE", "ND", "SD", "VT", "WY"), make_state_map_one)
names(maps) <- paste0(c("AK", "DE", "ND", "SD", "VT", "WY"), "_2020")

maps$SD_2020$nrv <- rowMeans(dplyr::select(dplyr::as_tibble(maps$SD), dplyr::contains('_rep_')), na.rm = TRUE)
maps$SD_2020$ndv <- rowMeans(dplyr::select(dplyr::as_tibble(maps$SD), dplyr::contains('_dem_')), na.rm = TRUE)

usethis::use_data(maps, internal=TRUE, overwrite=TRUE, compress="xz")
