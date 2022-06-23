# Test Environments
* local R installation (Windows 11), R 4.2.0
* local R installation (macOS), R 4.1.2
* macos-latest (on GitHub Actions), (release)
* windows-latest (on GitHub Actions), (release)
* ubuntu-latest (on GitHub Actions), (release)
* ubuntu-latest (on GitHub Actions), (old release)
* ubuntu-latest (on GitHub Actions), (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* Examples are \dontrun in the `alarm_50state_*` functions since these functions
download data from the Harvard Dataverse, which generally takes many seconds.
