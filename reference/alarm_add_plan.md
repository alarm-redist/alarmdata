# Add a reference plan to a set of plans

Facilitates comparing an existing (i.e., non-simulated) redistricting
plan to a set of simulated plans.

## Usage

``` r
alarm_add_plan(
  plans,
  ref_plan,
  map = NULL,
  name = NULL,
  calc_polsby = FALSE,
  GEOID = "GEOID",
  year = 2020
)
```

## Arguments

- plans:

  A `redist_plans` object.

- ref_plan:

  An `integer` vector containing the reference plan, a block assignment
  file as a `tibble` or `data.frame`, or an `sf` object where each row
  corresponds to a district.

- map:

  A `redist_map` object. Only required if the `redist_plans` object
  includes summary statistics.

- name:

  A human-readable name for the reference plan. Defaults to the name of
  `ref_plan`. If `ref_plan` is a `tibble` or `data.frame`, it should be
  the name of the column of `ref_plan` that identifies districts.

- calc_polsby:

  A logical value indicating whether a Polsby-Popper compactness score
  should be calculated for the reference plan. Defaults to `FALSE`.

- GEOID:

  character. If `ref_plan` is a `tibble` or `data.frame`, then it should
  correspond to the column of `ref_plan` that identifies block `GEOID`s.
  If `ref_plan` is an `sf` object, then it should correspond to the
  column of `ref_plan` that identifies district numbers. Ignored when
  `ref_plan` is numeric. Default is `'GEOID'`.

- year:

  the decade to request if passing a `tibble` to `ref_plan`, either
  `2010` or `2020`. Default is `2020`.

## Value

A modified `redist_plans` object containing the reference plan. Includes
summary statistics if the original `redist_plans` object had them as
well.

## Examples

``` r
if (FALSE) { # Sys.getenv("DATAVERSE_KEY") != ""
# requires Harvard Dataverse API key
map <- alarm_50state_map("WY")
pl <- alarm_50state_plans("WY")
pl_new <- alarm_add_plan(pl, ref_plan = c(1), map, name = "example")

# download and load a comparison plan
url <- paste0("https://github.com/PlanScore/Redistrict2020/raw/main/files/",
  "NM-2021-10/Congressional_Concept_A.zip")
tf <- tempfile(fileext = ".zip")
utils::download.file(url, tf)
utils::unzip(tf, exdir = dirname(tf))
baf <- readr::read_csv(file = paste0(dirname(tf), "/Congressional Concept A.csv"),
                       col_types = "ci")
names(baf) <- c("GEOID", "concept_a")
# Add it to the plans object
map_nm <- alarm_50state_map("NM")
plans_nm <- alarm_50state_plans("NM", stats = FALSE)
alarm_add_plan(plans_nm, baf, map = map_nm, name = "concept_a")
}
```
