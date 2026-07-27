# Construct a glycan grob

`glycanGrob()` prepares the complete drawing specification for one
glycan as a grid grob. It is the low-level drawing primitive used by
[`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md).

## Usage

``` r
glycanGrob(
  structure,
  ...,
  show_linkage = TRUE,
  orient = c("left", "right", "up", "down"),
  highlight = NULL,
  style = style_glydraw()
)
```

## Arguments

- structure:

  A
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  scalar, or a string of any glycan structure text nomenclatures
  supported by
  [`glyparse::auto_parse()`](https://glycoverse.github.io/glyparse/reference/auto_parse.html).

- ...:

  Ignored.

- show_linkage:

  Show glycosidic linkage annotations or not. Default is TRUE.
  Substituent annotations are always shown.

- orient:

  Direction in which the glycan extends from its reducing end: one of
  `"left"`, `"right"`, `"up"`, or `"down"`. Defaults to `"left"`.

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

- style:

  A
  [`style_glydraw()`](https://glycoverse.github.io/glydraw/dev/reference/style_glydraw.md)
  object that controls the cartoon's visual appearance.

## Value

A `glycanGrob` object inheriting from
[`grid::gTree()`](https://rdrr.io/r/grid/grid-defunct.html).

## Examples

``` r
grob <- glycanGrob("Gal(b1-3)GalNAc(a1-")
grid::grid.draw(grob)
```
