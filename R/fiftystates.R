#' Download maps and plans from the 50-State Simulation Project
#'
#' These functions will download [redist_map] and [redist_plans] objects for the
#' 50-State Simulation Project from the ALARM Project's Dataverse.
#'
#' Every decade following the Census, states and municipalities must redraw
#' districts for Congress, state houses, city councils, and more. The goal of
#' the 50-State Simulation Project is to enable researchers, practitioners, and
#' the general public to use cutting-edge redistricting simulation analysis to
#' evaluate enacted congressional districts.
#'
#' Evaluating a redistricting plan requires analysts to take into account each
#' state’s redistricting rules and particular political geography. Comparing the
#' partisan bias of a plan for Texas with the bias of a plan for New York, for
#' example, is likely misleading. Comparing a state’s current plan to a past
#' plan is also problematic because of demographic and political changes over
#' time. Redistricting simulations generate an ensemble of alternative
#' redistricting plans within a given state which are tailored to its
#' redistricting rules. Unlike traditional evaluation methods, therefore,
#' simulations are able to directly account for the state’s political geography
#' and redistricting criteria.
#'
#' @template state
#'
#'
#' @name alarm_50state
NULL

#' @rdname alarm_50state
#' @export
alarm_50state_map = function(state) {
}

#' @rdname alarm_50state
#' @export
alarm_50state_plans = function(state) {
}
