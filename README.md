
<!-- README.md is generated from README.Rmd. Please edit that file -->
bedshear
========

bedshear is an R package that let's you calculate bed shear stress from combined waves and tides, and wave orbital velocity.

Installation
------------

You can install the development version of bedshear from github using:

``` r
# install.packages("devtools")
devtools::install_github("r4ecology/rcdo", dependencies = TRUE)
```

This depends on Rcpp, so please check you can compile C++ files that rely on the math library.
