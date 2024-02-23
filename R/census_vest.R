#' Download Joined VEST and Census Data
#'
#' Downloads Census data joined with VEST's election data. All are re-tabulated from
#' precincts collected by VEST to 2020 Census geographies.
#'
#' @templateVar state TRUE
#' @param geometry If `TRUE` (default is `FALSE`), include `sf` geometry from Census Bureau TIGER Lines with the data.
#' @param epsg A numeric EPSG code to use as the coordinate system. Default is `alarm_epsg(state)`.
#' @template state
#'
#' @returns tibble with Census and election data
#' @export
#'
#' @examples
#' alarm_census_vest("DE", geometry = FALSE)
alarm_census_vest <- function(state, geometry = FALSE, epsg = alarm_epsg(state)) {
    geomander::get_alarm(state = state, geometry = geometry, epsg = epsg)
}
