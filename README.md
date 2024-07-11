
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FMAtools

<!-- badges: start -->

[![Lifecycle:stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](%5Bhttps://lifecycle.r-lib.org/articles/stages.html#stable%5D)
[![CRANstatus](https://www.r-pkg.org/badges/version/FMAAnalystFunctions)](https://CRAN.R-project.org/package=FMAAnalystFunctions)
<!-- badges: end -->

A collection of utility functions for use by the NOAA Fisheries AFSC FMA
Analyst Team.

## Installation

You might need to set up a githubPAT:

``` r
# install.packages("devtools")
devtools::install_github("Alaska-Fisheries-Monitoring-Analytics/FMAtools")
```

## `gdrive_` functions

If you intend to use the `gdrive_` functions of this package, you must
first authorize the `googledrive::` package to access your NOAA
account’s Google Drive. Run the following line to open a browser window
to do this, which must be done only once. **Make sure to check the box granting the package access to your files!** You will otherwise get 403 errors for insufficient permissions!

``` r
# Authorize the googledrive:: package to access your NOAA Google Drive
googledrive::drive_auth()
```
