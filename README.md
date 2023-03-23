
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

This is a private repo, you must first obtain permission from Jason
Jannot to access this repo. Assuming that permission is granted you will
need to set up a githubPAT:

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
devtools::install_github("jjannot-NOAA/FMAAnalystFunctions")
```

If you have previously set up a githubPAT simply do this:

``` r
credentials::set_github_pat() # enter your Token if necessary, usually not
devtools::install_github("jjannot-NOAA/FMAAnalystFunctions")
```

<!-- ## Example -->
<!-- This is a basic example which shows you how to solve a common problem: -->
<!-- ```{r example} -->
<!-- library(FMAAnalystFunctions) -->
<!-- ## basic example code -->
<!-- ``` -->
<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->
<!-- ```{r cars} -->
<!-- summary(cars) -->
<!-- ``` -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo = FALSE} -->
<!-- plot(pressure) -->
<!-- ``` -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
