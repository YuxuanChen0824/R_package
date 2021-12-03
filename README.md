<!-- badges: start -->
[![R-CMD-check](https://github.com/YuxuanChen0824/R_package/workflows/R-CMD-check/badge.svg)](https://github.com/YuxuanChen0824/R_package/actions)

[![codecov](https://codecov.io/gh/YuxuanChen0824/R_package/branch/main/graph/badge.svg?token=QTFT7GWAF8)](https://codecov.io/gh/YuxuanChen0824/R_package)
<!-- badges: end -->

# Package.v1

## Overview

Package.v1 conducts linear regression analysis on data, providing a list of functions that helps your fit linear model, do prediction, 
conduct general linear hypothesis testing, obtain four types of residuals with one line code, DFFIT calculation, and Cook's distance:

* `linear_model` fits linear model according to the formula you entered and returns various results like coefficient estimates, t value, p value, r square
* `predict_mock` returns estimated response values for new observations based on the linear model fitted on training dataset
* `GLH` conduct general linear hypothesis test based on the null hypothesis you provide and return the F statistics with the p value
* `residuals` calculate four types of residuals: nonstandardized, standardized, external studentized, and internal studentized. 
Results are returned as a carefully labelled data frame
* `dffit` calculate the DFFIT for each observation and results can be used for influence diagnosis
* `Cooks` calculate the Cook's distance for each observation 

## Installation
You can install the package with following code:

`devtools::install_github("YuxuanChen0824/R_package", build_vignettes = T)`

## Usage

For detailed tutorial, please run `browseVignettes("package.v1")` after installing the package. Below only simple example formats are provided:

```
# fit model
linear_model(y ~ x, data)

# prediction
predict_mock(mod, newobs)

# GLH
GLH(mod, contrast_matrix, rhs_constant)

# residuals
residuals(mod)

# DFFIT
dffit(mod)

# Cook's distance
Cooks(mod)
```
## Getting Help

For detailed usage of each functions and corresponding toy examples, please use `?linear_mod` `?predict_mock` `?GLH` `?residuals` `?dffit` and `?Cooks` to access help pages.

