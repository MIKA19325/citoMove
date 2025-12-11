
<!-- README.md is generated from README.Rmd. Please edit that file -->

# citoMove

The goal of citoMove is to provide an user-friendly R package for
analyzing movement data with DNNs.

## Installation

You can install the development version of citoMove from
[GitHub](https://github.com/) via:

``` r
# install.packages("devtools")
devtools::install_github("citoverse/citoMove")
```

In case you have installation problems of citoMove, please follow the
instructions given in the [cito package
vignette](https://cran.r-project.org/web/packages/cito/vignettes/A-Introduction_to_cito.html)

## Example

This is a basic example which shows you how to solve a common problem:

``` r

DNNmod = citoMove::fitDeepSSF(formula = case_ ~ z, data = Move1FEnv, lr = 1.1)

cito::conditionalEffects(DNNmod)
```
