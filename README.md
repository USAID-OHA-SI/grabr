# grabr <img src="man/figures/logo.png" align="right" height="120" />

OHA/SI API package

<!-- badges: start -->
[![R-CMD-check](https://github.com/USAID-OHA-SI/grabr/workflows/R-CMD-check/badge.svg)](https://github.com/USAID-OHA-SI/grabr/actions)
[![grabr status badge](https://usaid-oha-si.r-universe.dev/badges/grabr)](https://usaid-oha-si.r-universe.dev/grabr)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![:name status badge](https://usaid-oha-si.r-universe.dev/badges/:name)](https://usaid-oha-si.r-universe.dev/)
<!-- badges: end -->


## Overview

This package provides a series of base functions useful to the OHA/SI team. These function extend the utility functions in glamr, focusing primarily on API utility functions.


## Installation

`grabr` is not on CRAN, so you will have to install it directly from [rOpenSci](https://usaid-oha-si.r-universe.dev/packages) or [GitHub](https://github.com/USAID-OHA-SI/) using the code found below.

``` r
## SETUP

 #install from rOpenSci
    install.packages('grabr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
    
  #alt: install from GitHub using pak
    #install.packages("pak")
    #pak::pak("USAID-OHA-SI/grabr")
    
  #load the package
    library(grabr)

## LIST TYPES OF STYLES INCLUDED WITH PACKAGE
  ls("package:grabr")
```


---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*

