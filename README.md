
<!-- README.md is generated from README.Rmd. Please edit that file -->

# weartime

<!-- badges: start -->

[![R build
status](https://github.com/muschellij2/weartime/workflows/R-CMD-check/badge.svg)](https://github.com/muschellij2/weartime/actions)
[![R-CMD-check](https://github.com/muschellij2/weartime/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/muschellij2/weartime/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of weartime is to implement the wear time algorithm from
<https://github.com/shaheen-syed/CNN-Non-Wear-Time-Algorithm/>. This
algorithm is used to determine the wear time of a device, such as an
accelerometer, that is worn by a person. The algorithm is based on a
convolutional neural network (CNN) that is trained to determine if a
window of data is wear time or non-wear time. The algorithm is trained
on the ActiGraph GT3X+ accelerometer data, but can be used on other
accelerometer data as well.

## Installation

You can install the released version of weartime from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("weartime")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("muschellij2/weartime")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(weartime)
## basic example code
```
