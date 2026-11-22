make_state_map_one <- function(state, year = 2020L, geometry = TRUE, epsg = alarm_epsg(state)) {
  map <- maps[[paste0(censable::match_abb(state), "_", year)]]
  if (geometry) {
    map <- sf::st_transform(map, epsg)
  } else {
    map$geometry <- sf::st_sfc(sf::st_polygon(), crs = epsg)
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
