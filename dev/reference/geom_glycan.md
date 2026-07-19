# Draw glycans at ggplot2 positions

`geom_glycan()` draws one glycan cartoon for each data row. Each cartoon
is anchored at its mapped `x` and `y` position and retains the
structure-derived dimensions and appearance used by
[`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md).
The optional `size` aesthetic scales the complete cartoon uniformly,
including nodes, lines, text, and spacing, without changing their
relative appearance. Like points and text, the cartoons do not expand
the position scales beyond their anchor coordinates. Use scale expansion
or explicit coordinate limits when the cartoons need more room around
the panel edges. Unlike standalone cartoons returned by
[`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md),
cartoons in this layer have no output border or background.

## Usage

``` r
geom_glycan(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  angle = 0,
  show_linkage = TRUE,
  orient = c("H", "V"),
  fuc_orient = c("flex", "up"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL,
  highlight = NULL,
  style = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  The `x`, `y`, and `structure` aesthetics are required.

- data:

  The data to be displayed in this layer. When `NULL`, the default, the
  data is inherited from the plot data.

- stat:

  The statistical transformation to use on the data for this layer.
  Defaults to `"identity"`.

- position:

  A position adjustment to use on the data for this layer. Defaults to
  `"identity"`.

- ...:

  Other arguments passed to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- angle:

  Rotation in degrees. Like
  [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html),
  this can be supplied as an aesthetic or a fixed layer value. It
  rotates each completed cartoon around its mapped position
  independently of `orient`. Defaults to `0`.

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

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  Logical. Should this layer be included in the legends?

- inherit.aes:

  If `FALSE`, overrides the default aesthetics rather than combining
  with them.

## Value

A ggplot2 layer that can be added to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Aesthetics

`geom_glycan()` understands the following aesthetics. `x`, `y`, and
`structure` are required; the remaining aesthetics are optional:

- `x`

- `y`

- `structure`, containing glycan structure strings or
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  values

- `size`, an optional whole-cartoon scale multiplier that defaults to
  `1`. Mapped values are transformed by ggplot2's size scale; use
  [`ggplot2::scale_size_identity()`](https://ggplot2.tidyverse.org/reference/scale_identity.html)
  when the mapped values are literal multipliers. This is distinct from
  `node_size`, which changes residue size within the cartoon.

- `hjust`, an optional horizontal justification that defaults to `0.5`.
  Numeric `0` aligns the cartoon content's left bound with `x`, and `1`
  aligns its right bound. Use
  [`hjust_red_end()`](https://glycoverse.github.io/glydraw/dev/reference/hjust_red_end.md)
  to anchor a vertical cartoon at its reducing end.

- `vjust`, an optional vertical justification that defaults to `0.5`.
  Numeric `0` aligns the cartoon content's bottom bound with `y`,
  including the end of a reducing-end annotation line, and `1` aligns
  its top bound. Use
  [`vjust_red_end()`](https://glycoverse.github.io/glydraw/dev/reference/hjust_red_end.md)
  to anchor a horizontal cartoon at its reducing end.

- `angle`, an optional rotation in degrees that defaults to `0`.
  Rotation is applied after the cartoon is drawn, independently of
  `orient`.

## Examples

``` r
glycans <- data.frame(
  x = c(1, 3),
  y = c(1, 2),
  size = c(0.7, 1.1),
  structure = c(
    "Gal(b1-3)GalNAc(a1-",
    "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
  )
)

ggplot2::ggplot(
  glycans,
  ggplot2::aes(
    x = .data$x,
    y = .data$y,
    structure = .data$structure,
    size = .data$size
  )
) +
  geom_glycan() +
  ggplot2::scale_size_identity() +
  ggplot2::coord_cartesian(
    xlim = c(0, 4),
    ylim = c(0, 3),
    expand = FALSE
  )


# Bottom-align a row of vertical glycans with different heights.
ggplot2::ggplot(
  glycans,
  ggplot2::aes(x = .data$x, y = 1, structure = .data$structure)
) +
  geom_glycan(orient = "V", vjust = 0)
```
