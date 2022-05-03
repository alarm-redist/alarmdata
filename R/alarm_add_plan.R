#' This function facilitates comparing an existing (i.e., non-simulated) redistricting plan to a set of simulated plans.
#'
#' @param ref_plan An integer vector containing the reference plan. It will be renumbered to \code{1..ndists}
#' @param plans A \code{redist_plans} object.
#' @param map A \code{redist_map} object. Only required if the \code{redist_plans} object includes summary statistics.
#' @param name A human-readable name for the reference plan. Defaults to the name of \code{ref_plan}.
#' @return A modified \code{redist_plans} object containing the reference plan. Includes summary statistics if the original \code{redist_plans} object had them as well.
alarm_add_plan <- function(ref_plan, plans, map = NULL, name = NULL) {

    if("comp_polsby" %in% colnames(plans)) {
        # redist_plans object already has summary statistics, so they must be calculated for ref_plan as well
        if(is.null(map)) {
            # Without a redist_map object, summary statistics cannot be calculated
            stop("A redist_map object must be passed in as an argument for 'map' in order to calculate summary statistics for the provided reference map.")
        }
        ref_redist_plan <- redist::redist_plans(ref_plan, map, algorithm = "Reference", wgt = NULL)
        ref_plan_stats <- calc_plan_stats(ref_redist_plan, map)
        redist:::rbind.redist_plans(plans, ref_plan_stats)
    } else {
        redist::add_reference(plans, ref_plan, name)
    }

}
