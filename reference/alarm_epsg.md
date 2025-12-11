# Suggested EPSG Codes

Provides suggested EPSG codes for each of the 50 states. One of the
NAD83 (HARN) coordinate systems for each state.

## Usage

``` r
alarm_epsg(state)
```

## Arguments

- state:

  A state name, abbreviation, FIPS code, or ANSI code.

## Value

A numeric EPSG code

## Examples

``` r
alarm_epsg("NY")
#> [1] 2829
```
