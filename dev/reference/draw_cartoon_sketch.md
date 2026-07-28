# Draw a sketch-style glycan cartoon

`draw_cartoon_sketch()` uses
[ggsketch::ggsketch-package](https://orijitghosh.github.io/ggsketch/reference/ggsketch-package.html)
geoms to give a glycan cartoon hand-drawn strokes and patterned residue
fills. Its glycan layout, annotations, orientation, and sizing are
otherwise identical to
[`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md).

## Usage

``` r
draw_cartoon_sketch(
  structure,
  ...,
  show_linkage = TRUE,
  orient = c("left", "right", "up", "down"),
  highlight = NULL,
  style = style_glydraw(),
  roughness = 1,
  bowing = 1,
  n_passes = 2L,
  seed = NULL,
  fill_style = "pencil_shade",
  hachure_angle = 45,
  hachure_gap = 0.03,
  fill_weight = 0.5,
  medium = NULL
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

  Show glycosidic linkage annotations or not. Defaults to `TRUE`.
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

- roughness:

  Non-negative roughness of the hand-drawn strokes. Zero produces
  straight strokes. Hex circle outlines are automatically softened to
  keep their curved borders smooth.

- bowing:

  Non-negative multiplier controlling how much strokes bow.

- n_passes:

  Number of times each sketch stroke is drawn.

- seed:

  An optional integer seed for reproducible sketch strokes. When `NULL`,
  `ggsketch` uses `getOption("ggsketch.seed", 1L)`.

- fill_style:

  Residue fill pattern. Defaults to `"pencil_shade"`. See
  [`ggsketch::geom_sketch_polygon()`](https://orijitghosh.github.io/ggsketch/reference/geom_sketch_polygon.html)
  for the available styles.

- hachure_angle:

  Angle of patterned fill lines in degrees.

- hachure_gap:

  Gap between patterned fill lines as a proportion of the node diameter.
  Defaults to `0.03`.

- fill_weight:

  Stroke weight of patterned fill lines.

- medium:

  Optional drawing medium for linkage and reducing-end strokes. See
  [`ggsketch::sketch_media()`](https://orijitghosh.github.io/ggsketch/reference/sketch_media.html)
  for the available media.

## Value

A `glydraw_cartoon` ggplot2 object.

## Details

When `style_glydraw(font_family = "")` is used, the default, sketch
cartoons prefer an installed handwriting font that contains Greek alpha,
Greek beta, and decimal digits so all linkage labels use one font. An
explicitly selected `font_family` is used unchanged.

## Examples

``` r
if (requireNamespace("ggsketch", quietly = TRUE)) {
  draw_cartoon_sketch("Gal(b1-3)GalNAc(a1-", seed = 1)
}
```
