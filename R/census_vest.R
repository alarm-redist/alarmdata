#' Download Joined VEST and Census Data
#'
#' Downloads Census data joined with VEST's election data. All are re-tabulated from
#' precincts collected by VEST to 2020 Census geographies.
#'
#' @templateVar state TRUE
#' @param geometry Default is TRUE. Should `sf` geometry be included with the data?
#' @param file file path to save csv to, without
#' @param epsg numeric EPSG code to planarize to. Default is `alarm_epsg(state)`.
#' @template state
#'
#' @return tibble with Census and election data
#' @export
#'
#' @examples
#' alarm_census_vest('DE', geometry = FALSE)
alarm_census_vest <- function(state, geometry = TRUE, file = tempfile(fileext = '.csv'), epsg = alarm_epsg(state)) {
    geomander::get_alarm(state = state, geometry = geometry, file = file, epsg = epsg)
}
