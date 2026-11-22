devtools::load_all()

make_state_map_one <- function(state, geometry = TRUE, epsg = alarm_epsg(state)) {
    year <- 2020

    nd <- geomander::get_alarm(state = state, geometry = geometry, epsg = epsg)

    if (isTRUE(geometry)) {
        obj_geom = sf::st_union(nd$geometry) |>
            rmapshaper::ms_simplify(keep_shapes=TRUE)
    } else {
        obj_geom = sf::st_sfc(sf::st_polygon())
    }

    nd <- nd |>
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

    nd <- nd |>
        dplyr::mutate(
            nrv = rowMeans(as.data.frame(nd)[, id_nrv]),
            ndv = rowMeans(as.data.frame(nd)[, id_ndv]),
            nrv = round(.data$nrv, 1),
            ndv = round(.data$ndv, 1)
        ) |>
        dplyr::relocate(geometry, .after = .data$ndv)

    nd <- nd |>
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


# alaska data
ak_data <- dplyr::tibble(
  year = c(1990, 2000),
  pop = c(550043, 626932),
  pop_white = c(406722, 423788),
  pop_black = c(21799, 21073),
  pop_hisp = c(17803, 25852),
  pop_aian = c(84594, 96505),
  pop_asian = c(18730, 24741),
  pop_nhpi = c(NA_real_, 3181),
  pop_other = c(395, 1338),
  pop_two = c(NA_real_, 30454),
  vap = c(377699, 436215),
  vap_white = c(288976, 311655),
  vap_black = c(14200, 14113),
  vap_hisp = c(11048, 15588),
  vap_aian = c(50435, 59553),
  vap_asian = c(12853, 18193),
  vap_nhpi = c(NA_real_, 1869),
  vap_other = c(187, 832),
  vap_two = c(NA_real_, 14412)
)
ak_dem_votes <- c(62007, 72584, 84943, 60201)
ak_rep_votes <- c(138377, 119251, 76515, 50991)

# ROAD normalization: geometric-mean turnout and mean two-party vote share.
ak_turnout <- round(exp(mean(log(ak_dem_votes + ak_rep_votes))))
ak_data$ndv <- round(ak_turnout * mean(ak_dem_votes / (ak_dem_votes + ak_rep_votes)))
ak_data$nrv <- ak_turnout - ak_data$ndv

states <- c("AK", "DE", "MT", "ND", "SD", "VT", "WY")
state_geom <- tinytiger::tt_states(year = 2010)

for (year in c(1990, 2000, 2010)) {
  for (state in states) {
    if (year == 2010) {
      nd <- geomander::get_alarm(state, year = year, geometry = FALSE)
      nd <- nd |>
        dplyr::summarise(dplyr::across(
          c(tidyselect::starts_with("pop"), tidyselect::starts_with("vap"),
            tidyselect::matches("_(dem|rep)_|^a[dr]v_")), sum
        ))
      nd$ndv <- round(mean(unlist(nd[grepl("^adv_", names(nd))])), 1)
      nd$nrv <- round(mean(unlist(nd[grepl("^arv_", names(nd))])), 1)
    } else if (state == "AK") {
      nd <- ak_data |>
        dplyr::filter(.data$year == .env$year) |>
        dplyr::select(-year) |>
        dplyr::select(dplyr::where(function(x) !all(is.na(x))))
    } else {
      url <- paste0("https://raw.githubusercontent.com/alarm-redist/census-2020/",
                    "road/road-", year, "/", state, "_", year, ".csv")
      nd <- readr::read_csv(url, col_types = readr::cols(GEOID = "c"),
                            show_col_types = FALSE) |>
        dplyr::summarise(dplyr::across(
          c(tidyselect::starts_with("pop"), tidyselect::starts_with("vap"),
            tidyselect::all_of(c("ndv", "nrv"))), sum
        ))
    }

    if (state == "MT") {
      geom <- state_geom |>
        dplyr::filter(.data$STUSPS10 == .env$state) |>
        sf::st_transform(alarm_epsg(state)) |>
        sf::st_geometry()
    } else {
      geom <- sf::st_geometry(maps[[paste0(state, "_2020")]])
    }

    ref <- paste0("cd_", year)
    nd[[ref]] <- 1L
    nd <- sf::st_sf(nd, geometry = geom)
    map <- suppressWarnings(redist::redist_map(
      nd, existing_plan = ref, pop_tol = 0.005, adj = list(integer())
    ))
    map <- map |>
      dplyr::mutate(GEOID = censable::match_fips(.env$state),
                    state = .env$state, .before = dplyr::everything())
    attr(map, "analysis_name") <- paste0(state, "_", year)
    maps[[paste0(state, "_", year)]] <- map
  }
}

usethis::use_data(maps, internal = TRUE, overwrite = TRUE, compress = "xz")
