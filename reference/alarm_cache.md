# Work with the the `alarmdata` cache

Functions to inspect and clear the cache. If the cache is not enabled,
uses a temporary directory.

## Usage

``` r
alarm_cache_size()

alarm_cache_clear(force = FALSE)

alarm_cache_path()
```

## Arguments

- force:

  FALSE by default. Asks the user to confirm if interactive. Does not
  clear cache if force is FALSE and not interactive.

## Value

For `alarm_cache_size()`, the size in bytes, invisibly

For `alarm_cache_clear()`, the path to the cache, invisibly.

For `alarm_cache_path()`, the path to the cache

## Examples

``` r
alarm_cache_size()
#> 2.8 Mb

alarm_cache_clear()

alarm_cache_path()
#> [1] "/tmp/RtmpIZuyCf"
```
