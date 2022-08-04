<!-- badges: start -->
<!-- badges: end -->

<img src='man/figures/logo.png' align="right" height="120" />

# grabr
OHA/SI API package

## Overview

This package provides a series of base functions useful to the OHA/SI team. These function extend the utility functions in glamr, focusing primarily on API utility functions.


## Installation

`grabr` is not on CRAN, so you will have to install it directly from GitHub using `remotes`.

If you do not have `remotes` installed, you will have to run the `install.packages("remotes")` line in the code below as well.

``` r
## SETUP

  #install package with vignettes
    install.packages("remotes")
    remotes::install_github("USAID-OHA-SI/grabr", build_vignettes = TRUE)
    
  #load the package
    library(grabr)

## LIST TYPES OF STYLES INCLUDED WITH PACKAGE
  ls(package:grabr)
```


---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*

