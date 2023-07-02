# pRecipe <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->
[![R badge](https://img.shields.io/badge/build%20with-%E2%99%A5%20and%20R-blue)](https://github.com/MiRoVaGo/pRecipe)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/pRecipe)](https://cran.r-project.org/package=pRecipe)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/pRecipe)](https://cran.r-project.org/package=pRecipe)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/grand-total/pRecipe)](https://CRAN.R-project.org/package=pRecipe)
[![license](https://img.shields.io/badge/license-GPL3-lightgrey.svg)](https://choosealicense.com/)
<!-- badges: end -->

## What is `pRecipe`?

An open-access tool/framework to download, validate, visualize, and analyze multi-source precipitation data across various spatio-temporal scales. Ultimately providing the hydrology science community with the tools for consistent and reproducible analysis regarding precipitation (Vargas Godoy and Markonis, 2023).

## Features

* Homogenized database currently encompassing 27 different precipitation data products.
* Exploratory data analysis.
* Spatiotemporal analysis.
* Time series analysis.
* Simple and aesthetic data visualization.

## Installation

```r
# Get the latest version of pRecipe

if (!require('devtools')) {install.packages('devtools'); library(devtools)}

install_github('MiRoVaGo/pRecipe')

# Alternatively, the latest CRAN release

install.packages("pRecipe")

#

library(pRecipe)
?`pRecipe-package`
```

## References

Vargas Godoy, M.R. and Markonis, Y., 2023. pRecipe: A global precipitation climatology toolbox and database. Environmental Modelling & Software, 165, p.105711.
