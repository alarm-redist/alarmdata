alarm_add_plan <- function(plans, ref_plan, name = NULL) {

    if("comp_polsby" %in% colnames(plans)) {
        # redist_plans object already has summary statistics, so we must calculate them for ref_plan as well
        # TODO: Create a redist_plans object, ref_redist_plan, from the integer vector ref_plan
        ref_plan_stats <- calc_plan_stats(ref_redist_plan)
        redist:::rbind.redist_plans(plans, ref_plan_stats)
    } else {
        redist::add_reference(plans, ref_plan_stats)
    }

}
