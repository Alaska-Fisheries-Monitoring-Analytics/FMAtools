
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FMAtools

<!-- badges: start -->

[![Lifecycle:stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](%5Bhttps://lifecycle.r-lib.org/articles/stages.html#stable%5D)
[![CRANstatus](https://www.r-pkg.org/badges/version/FMAAnalystFunctions)](https://CRAN.R-project.org/package=FMAAnalystFunctions)
<!-- badges: end -->

A collection of utility functions for use by the NOAA Fisheries AFSC FMA
Analyst Team.

## Installation

Install or update your package by running:
``` r
devtools::install_github("Alaska-Fisheries-Monitoring-Analytics/FMAtools")
```
If you have not yet, you'll need to set up a githubPAT. [Set up a classic GitHub token](https://github.com/settings/tokens) with a 90-day expiration (or regenerate an exisiting one), copy the token to your clipboard, and then add it locally in R by running:
``` r
gitcreds::gitcreds_set()
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

## Querying databases using `db_query()`

Use the `db_query()` function as a shortcut to both perform a query and connect to a database. The `dsn` arugment of the function, which is `channel_afsc` by default, is used to reference your local `.Renviron` file where you can store your database connection method. Other `dsn` you may want to assign are `channel_cas` and `channel_akfin`. Please use these aliases to be consistent with the rest of the team.

You can edit your `.Renviron` file using:

``` r
usethis::edit_r_environ()
```
ensuring that your `dsn` is defined with your desired database connection method. For example, your .Renviron script may look like:
``` r
AFSCid = <USERNAME>
AFSCpw = <PASSWORD>
channel_afsc = "library(odbc); dbConnect(drv = odbc::odbc(), dsn = 'AFSC', UID = Sys.getenv('AFSCid'), PWD = Sys.getenv('AFSCpw'))"
```
Also, ensure that you have a `tnsnames.ora` file in your `C:/Oracle` folder. If you don't have one or if it's out of date, create a text document and paste the contents of [this page](https://docs.google.com/document/d/1O_msTtMN8D5sz_7LHzu7zep7kpYxNoK7gg4IqFVJY8E/edit?tab=t.0). Unless otherwise specified, R will look for your oracle configuration file in `C:Oracle`. 

You can test your connection leaving the `query` argument blank, which will simply query the date time. 
``` r
FMAtools::db_query(dsn = "channel_afsc")
```
If you have multiple database connections, ensure that you have separate unique names in your `.Renviron` (e.g., `channel_afsc` and `channel_akro`) pointing to a corresponding entry in `tnsnames.ora` and that the `dsn` argument of `db_query()` is correctly specified for the database you're querying.  

