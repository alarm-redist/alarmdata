alarm_add_plan <- function(ref_plan, plans, map = NULL, name = NULL) {

    if("comp_polsby" %in% colnames(plans)) {
        # redist_plans object already has summary statistics, so they must be calculated for ref_plan as well
        if(is.null(map)) {
            # Without a redist_map, summary statistics cannot be calculated
            stop("A redist_map object must be passed in as an argument for 'map' in order to calculate summary statistics for the provided reference map.")
        }
        ref_plan_stats <- calc_plan_stats(ref_redist_plan, map)
        redist:::rbind.redist_plans(plans, ref_plan_stats)
    } else {
        redist::add_reference(plans, ref_plan_stats, name)
    }

}
