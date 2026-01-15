# Draw a Symbol Nomenclature For Glycan (SNFG)

Draw a Symbol Nomenclature For Glycan (SNFG)

## Usage

``` r
draw_cartoon(
  structure,
  mono_size = 0.2,
  show_linkage = TRUE,
  orient = c("H", "V")
)
```

## Arguments

- structure:

  A
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  scalar, or a string or any glycan structure text nomenclatures.

- mono_size:

  Sizes of the monosaccharide. Default to 0.2. Setting this to large
  might make the residue overlap with linkage annotations.

- show_linkage:

  Show linkage annotation or not. Default is TRUE.

- orient:

  The orientation of glycan structure. "H" for horizontal, "V" for
  vertical. Default is "H"

## Value

a ggplot2 object

## Examples

``` r
draw_cartoon("Gal(b1-3)GalNAc(a1-")
#> Error: RStudio not running
```
