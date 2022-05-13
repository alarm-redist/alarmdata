#' Add a reference plan to a set of plans
#'
#' Facilitates comparing an existing (i.e., non-simulated) redistricting plan to a set of simulated plans.
#'
#' @param ref_plan An integer vector containing the reference plan. It will be renumbered to `1..ndists`.
#' @param plans A `redist_plans` object.
#' @param map A `redist_map` object. Only required if the `redist_plans` object includes summary statistics.
#' @param calc_polsby A logical value indicating whether a Polsby-Popper compactness score should be calculated for the reference plan. Defaults to `FALSE`.
#' @param name A human-readable name for the reference plan. Defaults to the name of `ref_plan`.
#'
#' @return A modified `redist_plans` object containing the reference plan. Includes summary statistics if the original `redist_plans` object had them as well.
#' @export
alarm_add_plan <- function(ref_plan, plans, map = NULL, calc_polsby = FALSE, name = NULL) {
    # redist_plans object already has summary statistics, so they must be calculated for ref_plan as well
    if ("comp_polsby" %in% names(plans)) {
        if (is.null(map)) {
            cli_abort("{.arg map} must be a {.cls redist_map} in order to
                      calculate summary statistics for the provided reference plan.")
        }
        ref_redist_plan <- redist::redist_plans(redist::redist.sink.plan(ref_plan),
                                                map, algorithm=attr(plans, "algorithm"))
        print(ref_redist_plan)
        ref_plan_stats <- calc_plan_stats(ref_redist_plan, map, calc_polsby)
        print(ref_plan_stats)
        rbind(ref_plan_stats, plans)
    } else { # just add reference
        redist::add_reference(plans, ref_plan, name)
    }

}
