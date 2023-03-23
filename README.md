
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FMA Analyst Functions

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/FMAAnalystFunctions)](https://CRAN.R-project.org/package=FMAAnalystFunctions)
<!-- badges: end -->

The goal of FMA Analyst Functions is to create a collection of utility
functions for use by the NOAA Fisheries AFSC FMA Analyst Team.

## Installation

You might need to set up a githubPAT:

``` r
#set config
usethis::use_git_config(user.name = "YourName", user.email = "your@mail.com")

#Go to github page to generate token - BE SURE TO COPY and SAVE the token somewhere you can find it.
usethis::create_github_token()

#click on Token and paste your PAT/new token into pop-up that follows...
credentials::set_github_pat()
```

After that, you can install the development version of
`FMAAnalystFunctions` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("Alaska-Fisheries-Monitoring-Analytics/FMAAnalystFunctions")
```

If you have previously set up a githubPAT simply do this:

``` r
credentials::set_github_pat() # enter your Token if necessary, usually not
devtools::install_github("Alaska-Fisheries-Monitoring-Analytics/FMAAnalystFunctions")
```

Note that the credentials may or may not be necessary.
