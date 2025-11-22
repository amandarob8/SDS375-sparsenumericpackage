
# sparsevectorspackage

[![R-CMD-check](https://github.com/amandarob8/SDS375-sparsenumericpackage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/amandarob8/SDS375-sparsenumericpackage/actions/workflows/R-CMD-check.yaml)

[Visit the package website](https://your-username.github.io/SDS375-sparsenumericpackage/)


An R package for storing and working with sparse numeric vectors using
an S4 class.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("amandarob8/sparsevectorspackage")
```

## Example

``` r
library(sparsevectorspackage)

x <- c(0, 5, 0, 2, 0, 3)
sx <- as(x, "sparse_numeric")
as(sx, "numeric")
```
