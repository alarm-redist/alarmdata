# alarmdata 0.2.5

* `alarm_50state_map()`, `alarm_50state_plans()`, `alarm_50state_stats()`, and
  `alarm_50state_doc()` gain a `callais` argument to download the post-Callais
  2020 simulations (race-based constraints removed) from the Callais Dataverse.
  Only the re-simulated states differ; all other states fall back to the
  standard 2020 data.

# alarmdata 0.2.2

* Update maintainer
* Bump minimum R version per CRAN NOTE

# alarmdata 0.2.2

* Adds support for using `sf` objects as input to `alarm_add_plans()`

# alarmdata 0.2.1

* Adds support for 2010 50-state simulation data.
* Prepare for CRAN release

# alarmdata 0.1.0

* Initial package release
* Functionality to download 50-state simulation data, 2020 Census + VEST data,
  and add additional plans to simulation objects
