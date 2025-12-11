# Download maps and plans from the 50-State Simulation Project

These functions will download
[redist_map](http://alarm-redist.org/redist/reference/redist_map.md) and
[redist_plans](http://alarm-redist.org/redist/reference/redist_plans.md)
objects for the 50-State Simulation Project from the ALARM Project's
Dataverse. `alarm_50state_doc()` will download documentation for a
particular state and show it in a browser. `alarm_50state_stats` will
download just the summary statistics for a state.

## Usage

``` r
alarm_50state_map(state, year = 2020, refresh = FALSE)

alarm_50state_plans(
  state,
  stats = TRUE,
  year = 2020,
  refresh = FALSE,
  compress = "xz"
)

alarm_50state_stats(state, year = 2020, refresh = FALSE)

alarm_50state_doc(state, year = 2020)
```

## Arguments

- state:

  A state name, abbreviation, FIPS code, or ANSI code.

- year:

  The redistricting cycle to download. Currently only `2020` and `2010`
  are available.

- refresh:

  If `TRUE`, ignore the cache and download again.

- stats:

  If `TRUE` (the default), download summary statistics for each plan.

- compress:

  The compression level used for caching
  [redist_plans](http://alarm-redist.org/redist/reference/redist_plans.md)
  objects.

## Value

For `alarm_50state_map()`, a
[redist_map](http://alarm-redist.org/redist/reference/redist_map.md).
For `alarm_50state_plans()`, a
[redist_plans](http://alarm-redist.org/redist/reference/redist_plans.md).
For `alarm_50state_doc()`, invisibly returns the path to the HTML
documentation, and also loads an HTML file into the viewer or web
browser. For `alarm_50state_stats()`, a
[tibble](https://dplyr.tidyverse.org/reference/reexports.html).

## Details

Every decade following the Census, states and municipalities must redraw
districts for Congress, state houses, city councils, and more. The goal
of the 50-State Simulation Project is to enable researchers,
practitioners, and the general public to use cutting-edge redistricting
simulation analysis to evaluate enacted congressional districts.

Evaluating a redistricting plan requires analysts to take into account
each state’s redistricting rules and particular political geography.
Comparing the partisan bias of a plan for Texas with the bias of a plan
for New York, for example, is likely misleading. Comparing a state’s
current plan to a past plan is also problematic because of demographic
and political changes over time. Redistricting simulations generate an
ensemble of alternative redistricting plans within a given state which
are tailored to its redistricting rules. Unlike traditional evaluation
methods, therefore, simulations are able to directly account for the
state’s political geography and redistricting criteria.

## Examples

``` r
if (FALSE) { # Sys.getenv("DATAVERSE_KEY") != ""

# requires Harvard Dataverse API key
alarm_50state_map("WA")
alarm_50state_plans("WA", stats = FALSE)
alarm_50state_stats("WA")
alarm_50state_doc("WA")

map <- alarm_50state_map("WY")
pl <- alarm_50state_plans("WY")
}
```
