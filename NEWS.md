# alarmdata 0.2.5

* `alarm_50state_map()`, `alarm_50state_plans()`, `alarm_50state_stats()`, and
  `alarm_50state_doc()` gain a `vra` argument (default `TRUE`). Setting
  `vra = FALSE` downloads simulations with any VRA constraints removed.

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
