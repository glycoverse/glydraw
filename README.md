<!-- README.md is generated from README.Rmd. Please edit that file -->

# glydraw

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![CRAN status](https://www.r-pkg.org/badges/version/glydraw)](https://CRAN.R-project.org/package=glydraw) [![R-CMD-check](https://github.com/glycoverse/glydraw/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/glycoverse/glydraw/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/glycoverse/glydraw/graph/badge.svg)](https://app.codecov.io/gh/glycoverse/glydraw)

<!-- badges: end -->

The goal of glydraw is to draw glycan structures with glycan nomenclature.

## Installation

You can install the latest release of glydraw from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("glycoverse/glydraw@*release")
```

Or install the development version:

``` r
remotes::install_github("glycoverse/glydraw")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(glydraw)
# basic example code
draw_cartoon("Man(a1-3)[Man(a1-4)]Man(a1-3)GlcNAc(a1-4)GlcNAc(a1-")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%"/>

``` r
# If you want to hide the annotation (default add annotation):
draw_cartoon("Man(a1-3)[Man(a1-4)]Man(a1-3)GlcNAc(a1-4)GlcNAc(a1-", annotate = FALSE)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%"/>

``` r
# If you want to draw structure vertically (default horizontally):
draw_cartoon("Man(a1-3)[Man(a1-4)]Man(a1-3)GlcNAc(a1-4)GlcNAc(a1-", orien = 'V')
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%"/>

It is necessary to declare that pictures displayed in RStudio is different from those saved with `save_cartoon()`, because the size need to be dynamically adjusted by `save_cartoon()`. You can check the correct glycan pictures after saved by `save_cartoon()`.
