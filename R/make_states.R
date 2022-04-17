make_state_map_one <- function(state, geometry = TRUE, epsg = alarm_epsg(state)) {

    nd <- geomander::get_alarm(state = "ND", geometry = geometry, epsg = epsg)

    nd <- nd %>%
        summarise(across(starts_with("pop"), sum),
                  across(starts_with("vap"), sum),
                  across(matches("_(dem|rep)_"), sum),
                  across(matches("^a[dr]v_"), sum),
                  geometry = sf::st_union(geometry))

    redist::redist_map(nd, pop_col = pop, ndists = 1, pop_tol = 0.01) %>%
        suppressWarnings() # avoid adjacency warning
}


make_state_plans_one <- function(state) {

    m <- matrix(1, nrow = 1, ncol = 5000)
    map <- make_state_map_one(state)

    redist:::new_redist_plans(m, map, algorithm = "Single", wgt = NULL)

    # TODO: Add summary stats once calc_plan_stats() done.

}
