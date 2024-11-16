# MOreg

<!-- badges: start -->
[![R-CMD-check](https://github.com/MaryOwusuBonsu/MOreg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MaryOwusuBonsu/MOreg/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of MOreg is to provide a simple function for fitting linear regression models in R.



## Installation

You can install the development version of MOreg from GitHub using:

```r
devtools::install_github('MaryOwusuBonsu/MOreg')
```


## Example

This is a basic example which shows you how to fit a linear regression model using the `MOreg` package:

```r
library(MOreg)

# Sample data
Y <- c(2.5, 6.8, 8.7, 9)
X <- c(15, 25, 30, 29)

# Fit linear model
model <- MOreg(Y, X)
print(model)
```

## Output

The function returns a list with the following components:

- `beta`: Estimated regression coefficients
- `residuals`: Differences between observed and fitted values
- `sigma_sq_hat`: Estimated variance of the residuals
- `std_err`: Standard errors of the regression coefficients
- `tval`: t-values for the regression coefficients
- `pval`: p-values for the regression coefficients

