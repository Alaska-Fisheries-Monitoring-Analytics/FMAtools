
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FMAtools

**ARCHIVED:** This package is now developed within the `noaa-afsc` enterprise repository: [noaa-afsc/FMAtools](https://github.com/noaa-afsc/FMAtools)

Download it by running:

``` r
pak::pak("noaa-afsc/FMAtools")
```

A collection of utility functions for use by the NOAA Fisheries AFSC FMA
Analyst Team.

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

