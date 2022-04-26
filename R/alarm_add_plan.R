alarm_add_plan <- function(plans, ref_plan, name = NULL) {

    if("comp_polsby" %in% colnames(plans)) {
        # redist_plans object already has summary statistics, so we must calculate them for ref_plan as well
        ref_plan_stats <- calc_plan_stats(ref_plan)
    }
    redist::add_reference(plans, ref_plan_stats)

}
