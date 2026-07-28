# Draw a Symbol Nomenclature For Glycan (SNFG)

Draw a Symbol Nomenclature For Glycan (SNFG)

## Usage

``` r
draw_cartoon(
  structure,
  ...,
  show_linkage = TRUE,
  orient = c("left", "right", "up", "down"),
  highlight = NULL,
  style = style_glydraw(),
  red_end = NULL
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

- red_end:

  Reducing-end annotation. `NULL`, the default, uses `red_end` from
  `style`. A non-`NULL` value overrides `style$red_end`. Ignored when
  `style$red_end_length` is `0`. To annotate an amino-acid sequence, tag
  its single glycosite as, for example, `"ABC<site>D</site>EFG"`.

## Value

a ggplot2 object

## Examples

``` r
draw_cartoon("Gal(b1-3)GalNAc(a1-")

draw_cartoon(
  "Gal(b1-3)GalNAc(a1-",
  style = style_glydraw(font_family = "serif")
)
```
