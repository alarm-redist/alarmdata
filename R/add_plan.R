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
    if (!inherits(plans, "redist_plans"))
        cli_abort("{.arg plans} must be a {.cls redist_plans}")
    if (isTRUE(attr(plans, "partial")))
        cli_abort("Reference plans not supported for partial plans objects")
    if (!is.numeric(ref_plan))
        cli_abort("{.arg ref_plan} must be numeric")
    if (length(ref_plan) != nrow(redist::get_plans_matrix(plans)))
        cli_abort("{.arg ref_plan} must have the same number of precincts as {.arg plans}")

    if (is.null(name)) {
        ref_str = deparse(substitute(ref_plan))
        if (stringr::str_detect(ref_str, stringr::fixed("$"))) {
            name = strsplit(ref_str, "$", fixed = TRUE)[[1]][2]
        }
        else {
            name = ref_str
        }
    }
    else if (!is.character(name)) {
            cli_abort("{.arg name} must be a {.cls chr}")
    }

    if (name %in% levels(plans$draw)) {
        cli_abort("Reference plan name already exists")
    }

    if ("comp_polsby" %in% names(plans)) {
        if (is.null(map)) {
            cli_abort("{.arg map} must be a {.cls redist_map} in order to calculate summary statistics for the provided reference plan.")
        }

        ref_redist_plan <- redist::redist_plans(redist::redist.sink.plan(ref_plan),
                                                map, algorithm=attr(plans, "algorithm"))
        ref_plan_stats <- calc_plan_stats(ref_redist_plan, map, calc_polsby)

        ref_plan_stats$draw <- name
        attr(ref_plan_stats, "resampled") <- attr(plans, "resampled")
        attr(ref_plan_stats, "compactness") <- attr(plans, "compactness")
        attr(ref_plan_stats, "constraints") <- attr(plans, "constraints")

        if (is.null(attr(plans, "ndists"))) {
            attr(plans, "ndists") <- max(as.matrix(plans)[, 1])
        }
        attr(ref_plan_stats, "ndists") <- attr(plans, "ndists")

        new_plans <- rbind(ref_plan_stats, plans)
        m <- redist::get_plans_matrix(ref_plan_stats)
        colnames(m)[1] <- name
        attr(new_plans, "plans") <- cbind(m, redist::get_plans_matrix(plans))

        new_plans

    } else { # just add reference
        redist::add_reference(plans, ref_plan, name)
    }

}
