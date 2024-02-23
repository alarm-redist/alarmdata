#' Suggested EPSG Codes
#'
#' Provides suggested EPSG codes for each of the 50 states.
#' One of the NAD83 (HARN) coordinate systems for each state.
#'
#' @templateVar state TRUE
#' @template state
#'
#' @returns A numeric EPSG code
#' @export
#'
#' @examples
#' alarm_epsg("NY")
alarm_epsg <- function(state) {

    epsg <- list(AL = 2759L, AK = 3338L, AZ = 2762L, AR = 2764L, CA = 3311L,
        CO = 2773L, CT = 2775L, DE = 2776L, FL = 2777L, GA = 2780L,
        HI = 2784L, ID = 2788L, IL = 2790L, IN = 2792L, IA = 2794L,
        KS = 2796L, KY = 2798L, LA = 2800L, ME = 2802L, MD = 2804L,
        MA = 2805L, MI = 2808L, MN = 2811L, MS = 2813L, MO = 2816L,
        MT = 2818L, NE = 2819L, NV = 2821L, NH = 2823L, NJ = 2824L,
        NM = 2826L, NY = 2829L, NC = 3358L, ND = 2832L, OH = 2834L,
        OK = 2836L, OR = 2838L, PA = 3362L, RI = 2840L, SC = 3360L,
        SD = 2841L, TN = 2843L, TX = 2845L, UT = 2850L, VT = 2852L,
        VA = 2853L, WA = 2855L, WV = 2857L, WI = 2860L, WY = 2863L)

    abb <- censable::match_abb(state)
    if (length(abb) != 1) {
        cli_abort(c("{.arg state}could not be matched to a single state.",
            "x" = "Please make {.arg state} correspond to the name, abbreviation, or FIPS of one state."))
    }

    epsg[[abb]]
}
