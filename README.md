# Misc Machine Learning

[![R-CMD-check](https://github.com/vlyubchich/MML/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vlyubchich/MML/actions/workflows/R-CMD-check.yaml)

This is an R package with miscellaneous functions for machine-learning tasks. 
To get this package from GitHub, run
```r
devtools::install_github("vlyubchich/MML")
library(MML)
```

This package is not currently on CRAN.

The package includes the functions for

* Quantile regression
    * `gof_qr()` to calculate goodness-of-fit measures for quantile regression
    * `partial_qrf()` to calculate partial dependence plots from quantile random forest
