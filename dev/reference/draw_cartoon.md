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
  scalar, or a string of any glycan structure text nomenclatures
  supported by
  [`glyparse::auto_parse()`](https://glycoverse.github.io/glyparse/reference/auto_parse.html).

- show_linkage:

  Show linkage annotation or not. Default is TRUE.

- orient:

  The orientation of glycan structure. "H" for horizontal, "V" for
  vertical. Default is "H"

- highlight:

  An integer vector specifying the node indices to highlight. This
  argument is applicable only when `structure` is a
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).
  Note that for a
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html),
  the node indices correspond exactly to the monosaccharides in its
  printed IUPAC nomenclature. For example, given
  `glyrepr::as_glycan_structure("Gal(b1-3)[GlcNAc(b1-6)]GalNAc(a1-")`,
  setting `highlight = c(1, 3)` will highlight the "Gal" and "GalNAc"
  nodes.

## Value

a ggplot2 object

## Examples

``` r
if (FALSE) { # \dontrun{
draw_cartoon("Gal(b1-3)GalNAc(a1-")
} # }
```
