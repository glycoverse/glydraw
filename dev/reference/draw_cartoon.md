# Draw a Symbol Nomenclature For Glycan (SNFG)

Draw a Symbol Nomenclature For Glycan (SNFG)

## Usage

``` r
draw_cartoon(
  structure,
  show_linkage = TRUE,
  orient = c("H", "V"),
  highlight = NULL
)
```

## Arguments

- structure:

  A
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  scalar, or a string or any glycan structure text nomenclatures.

- show_linkage:

  Show linkage annotation or not. Default is TRUE.

- orient:

  The orientation of glycan structure. "H" for horizontal, "V" for
  vertical. Default is "H"

- highlight:

  highlight specified monosaccharides.

## Value

a ggplot2 object

## Examples

``` r
if (FALSE) { # \dontrun{
draw_cartoon("Gal(b1-3)GalNAc(a1-")
} # }
```
