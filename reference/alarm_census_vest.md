# Download Joined VEST and Census Data

Downloads Census data joined with VEST's election data. All are
re-tabulated from precincts collected by VEST to 2020 Census
geographies.

## Usage

``` r
alarm_census_vest(state, geometry = FALSE, epsg = alarm_epsg(state))
```

## Arguments

- state:

  A state name, abbreviation, FIPS code, or ANSI code.

- geometry:

  If `TRUE` (default is `FALSE`), include `sf` geometry from Census
  Bureau TIGER Lines with the data.

- epsg:

  A numeric EPSG code to use as the coordinate system. Default is
  `alarm_epsg(state)`.

## Value

tibble with Census and election data

## Examples

``` r
alarm_census_vest("DE", geometry = FALSE)
#> Data sourced from the ALARM Project
#> <https://github.com/alarm-redist/census-2020>.
#> This message is displayed once per session.
#> # A tibble: 412 × 44
#>    GEOID20     state county    vtd     pop pop_hisp pop_white pop_black pop_aian
#>    <chr>       <chr> <chr>     <chr> <dbl>    <dbl>     <dbl>     <dbl>    <dbl>
#>  1 10001001-28 DE    Kent Cou… 001-…  2203      209       749      1080       14
#>  2 10001001-29 DE    Kent Cou… 001-… 11966      651      7465      2933       46
#>  3 10001001-30 DE    Kent Cou… 001-…  5543      300      3878       911       15
#>  4 10001001-31 DE    Kent Cou… 001-…   537       20       417        76        2
#>  5 10001001-32 DE    Kent Cou… 001-…  6420      958      2182      2570       42
#>  6 10001001-33 DE    Kent Cou… 001-…   866       68       510       204        7
#>  7 10001001-34 DE    Kent Cou… 001-…   464       53       195       130        1
#>  8 10001002-28 DE    Kent Cou… 002-…  4366      322      2697      1007        5
#>  9 10001002-29 DE    Kent Cou… 002-…  8882      647      4801      2561      121
#> 10 10001002-30 DE    Kent Cou… 002-…   604       27       446        85        6
#> # ℹ 402 more rows
#> # ℹ 35 more variables: pop_asian <dbl>, pop_nhpi <dbl>, pop_other <dbl>,
#> #   pop_two <dbl>, vap <dbl>, vap_hisp <dbl>, vap_white <dbl>, vap_black <dbl>,
#> #   vap_aian <dbl>, vap_asian <dbl>, vap_nhpi <dbl>, vap_other <dbl>,
#> #   vap_two <dbl>, pre_16_dem_cli <dbl>, pre_16_rep_tru <dbl>,
#> #   gov_16_dem_car <dbl>, gov_16_rep_bon <dbl>, uss_18_dem_car <dbl>,
#> #   uss_18_rep_arl <dbl>, atg_18_dem_jen <dbl>, atg_18_rep_pep <dbl>, …
```
