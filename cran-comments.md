# Resubmission Changes

* Description links have been corrected to include author names before the links. The 
arXiv link as been updated to the published `doi`.

* TODO what is the examples comment?

* The ALARM Project was incorrectly named as the copyright holder in the initial submission. 
However, this is  the name of the research group that the authors belong to. 
This has been corrected to name the authors of the package, rather than the group.

# Test Environments
* local R installation (Windows 11), R 4.2.0
* local R installation (macOS), R 4.2.0
* macos-latest (on GitHub Actions), (release)
* windows-latest (on GitHub Actions), (release)
* ubuntu-latest (on GitHub Actions), (release)
* ubuntu-latest (on GitHub Actions), (old release)
* ubuntu-latest (on GitHub Actions), (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission of a new release.
* Failures on automated checks have been fixed
* Some examples are \dontrun in the `alarm_50state_*` functions since these functions
download data from the Harvard Dataverse, which generally takes many seconds.
