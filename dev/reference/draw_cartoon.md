# Draw a Symbol Nomenclature For Glycan (SNFG)

Draw a Symbol Nomenclature For Glycan (SNFG)

## Usage

``` r
draw_cartoon(
  structure,
  ...,
  show_linkage = TRUE,
  orient = c("H", "V"),
  fuc_orient = c("flex", "up"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL,
  highlight = NULL,
  style = NULL
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

  The orientation of glycan structure. "H" for horizontal, "V" for
  vertical. Default is "H"

- fuc_orient:

  Fuc-like triangle orientation. `"flex"` points non-reducing Fuc-like
  residues toward their rendered linkage direction, while `"up"` draws
  all Fuc-like triangles pointing upward. Reducing-end Fuc-like residues
  always point upward. Defaults to `"flex"`.

- red_end:

  Reducing-end annotation. The default `""` keeps the current
  reducing-end line. Use `"~"` to add a wavy reducing end, or any other
  string to draw that string at the reducing end.

- edge_linewidth:

  Numeric scalar controlling the linewidth of linkage lines. Defaults to
  the current value, `0.8`.

- node_linewidth:

  Numeric scalar controlling the linewidth of node borders. Defaults to
  the current value, `0.8`.

- node_size:

  Numeric scalar used as a multiplier for the default node size.
  Defaults to `1`, which keeps the current size. Linkage annotations are
  moved farther from larger nodes, and are hidden with a warning when
  `node_size` is too large to leave enough annotation space. Values
  larger than `2` are rejected because residues overlap.

- colors:

  Optional named character vector of custom monosaccharide fill colors.
  Names must be supported monosaccharide names, such as `"Gal"` or
  `"GlcNAc"`. User-provided colors overwrite the default SNFG colors,
  while unprovided monosaccharides keep their default colors. Defaults
  to `NULL`.

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

  A `glydraw_style` object that supplies rendering options. Explicitly
  supplied rendering arguments override it.

## Value

a ggplot2 object

## Examples

``` r
draw_cartoon("Gal(b1-3)GalNAc(a1-")
```
