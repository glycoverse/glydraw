# glydraw

The goal of glydraw is to draw glycan structures with glycan
nomenclature.

## Installation

You can install the latest release of glydraw from
[GitHub](https://github.com/) with:

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

![](reference/figures/README-unnamed-chunk-2-1.png)

``` r
# If you want to hide the annotation (default add annotation):
draw_cartoon("Man(a1-3)[Man(a1-4)]Man(a1-3)GlcNAc(a1-4)GlcNAc(a1-", annotate = FALSE)
```

![](reference/figures/README-unnamed-chunk-3-1.png)

``` r
# If you want to draw structure vertically (default horizontally):
draw_cartoon("Man(a1-3)[Man(a1-4)]Man(a1-3)GlcNAc(a1-4)GlcNAc(a1-", orien = 'V')
```

![](reference/figures/README-unnamed-chunk-4-1.png)

It is necessary to declare that pictures displayed in RStudio is
different from those saved with
[`save_cartoon()`](https://glycoverse.github.io/glydraw/reference/save_cartoon.md),
because the size need to be dynamically adjusted by
[`save_cartoon()`](https://glycoverse.github.io/glydraw/reference/save_cartoon.md).
You can check the correct glycan pictures after saved by
[`save_cartoon()`](https://glycoverse.github.io/glydraw/reference/save_cartoon.md).
