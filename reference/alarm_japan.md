# Download maps and plans from the Japan 47-Prefecture Simulation Project

These functions will download
[redist_map](http://alarm-redist.org/redist/reference/redist_map.md) and
[redist_plans](http://alarm-redist.org/redist/reference/redist_plans.md)
objects for the Japan 47-Prefecture Simulation Project from the ALARM
Project's Dataverse. `alarm_japan_doc()` will download documentation for
a particular prefecture and show it in a browser. `alarm_japan_stats`
will download just the summary statistics for a prefecture

## Usage

``` r
alarm_japan_map(pref, year = 2022, refresh = FALSE)

alarm_japan_plans(
  pref,
  stats = TRUE,
  year = 2022,
  refresh = FALSE,
  compress = "xz"
)

alarm_japan_stats(pref, year = 2022, refresh = FALSE)

alarm_japan_doc(pref, year = 2022)
```

## Arguments

- pref:

  A prefecture name

- year:

  The redistricting cycle to download. Currently only `2022` is
  available.

- refresh:

  If `TRUE`, ignore the cache and download again.

- stats:

  If `TRUE` (the default), download summary statistics for each plan.

- compress:

  The compression level used for caching
  [redist_plans](http://alarm-redist.org/redist/reference/redist_plans.md)
  objects.

## Value

For `alarm_japan_map()`, a
[redist_map](http://alarm-redist.org/redist/reference/redist_map.md).
For `alarm_japan_plans()`, a
[redist_plans](http://alarm-redist.org/redist/reference/redist_plans.md).
For `alarm_japan_doc()`, invisibly returns the path to the HTML
documentation, and also loads an HTML file into the viewer or web
browser. For `alarm_japan_stats()`, a
[tibble](https://dplyr.tidyverse.org/reference/reexports.html).

## Details

The goal of the 47-Prefecture Simulation Project is to generate and
analyze redistricting plans for the single-member districts of the House
of Representatives of Japan using a redistricting simulation algorithm.
In this project, we analyzed the partisan bias of the 2022 redistricting
for 25 prefectures subject to redistricting. Our simulations are
designed to comply with the that the Council abides by.

## Examples

``` r
if (FALSE) { # Sys.getenv("DATAVERSE_KEY") != ""

# requires Harvard Dataverse API key
alarm_japan_map("miyagi")
alarm_japan_plans("miyagi", stats = FALSE)
alarm_japan_stats("miyagi")
alarm_japan_doc("miyagi")

map <- alarm_japan_map("miyagi")
pl <- alarm_japan_plans("miyagi")
}
```
