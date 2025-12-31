# Draw the image based on the coordinates

Draw the image based on the coordinates

## Usage

``` r
draw_cartoon(
  structure,
  point_size = 0.15,
  annotate = TRUE,
  orien = c("H", "V")
)
```

## Arguments

- structure:

  A
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  scalar, or a string or any glycan structure text nomenclatures.

- point_size:

  The glycan size.

- annotate:

  Add annotation or not.

- orien:

  The orientation of glycan structure.

## Value

ggplot2 object

## Examples

``` r
draw_cartoon("Gal(b1-3)GalNAc(a1-")
```
