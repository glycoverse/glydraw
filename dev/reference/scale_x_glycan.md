# Use glycan cartoons as axis labels

`scale_x_glycan()` and `scale_y_glycan()` are discrete position scales
that replace text tick labels with compact glycan cartoons. Mapped
discrete values and values returned by the scale's `labels` argument may
be glycan structure strings supported by
[`glyparse::auto_parse()`](https://glycoverse.github.io/glyparse/reference/auto_parse.html)
or
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
vectors. X-axis cartoons are vertical and bottom-aligned by default,
while y-axis cartoons are horizontal and right-aligned by default. The
cartoon orientation and alignment adapt to the displayed axis, including
when the axes are swapped by
[`ggplot2::coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html).

## Usage

``` r
scale_x_glycan(
  name = ggplot2::waiver(),
  ...,
  palette = seq_len,
  expand = ggplot2::waiver(),
  position = "bottom",
  sec.axis = ggplot2::waiver(),
  continuous.limits = NULL,
  size = 0.4,
  angle = 0,
  vjust = 0,
  nudge_x = 0,
  nudge_y = 0,
  show_linkage = TRUE,
  red_end = "",
  fuc_orient = c("flex", "up"),
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL
)

scale_y_glycan(
  name = ggplot2::waiver(),
  ...,
  palette = seq_len,
  expand = ggplot2::waiver(),
  position = "left",
  sec.axis = ggplot2::waiver(),
  continuous.limits = NULL,
  size = 0.4,
  angle = 0,
  hjust = 1,
  nudge_x = 0,
  nudge_y = 0,
  show_linkage = TRUE,
  red_end = "",
  fuc_orient = c("flex", "up"),
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL
)
```

## Arguments

- name:

  The name of the scale, displayed as the axis title. Use `NULL` to
  remove the title.

- ...:

  Arguments passed to
  [`ggplot2::scale_x_discrete()`](https://ggplot2.tidyverse.org/reference/scale_discrete.html)
  or
  [`ggplot2::scale_y_discrete()`](https://ggplot2.tidyverse.org/reference/scale_discrete.html),
  including `breaks` and `labels`.

- palette:

  A palette function that returns discrete position values.

- expand:

  Expansion applied to the discrete position scale.

- position:

  Position of the axis.

- sec.axis:

  A secondary axis specification.

- continuous.limits:

  Continuous limits used to position the discrete scale.

- size:

  Positive scalar that uniformly scales each axis-label cartoon.
  Defaults to `0.4`.

- angle:

  Rotation in degrees applied to each axis-label cartoon, independently
  of the cartoon orientation. Defaults to `0`.

- vjust:

  Vertical justification for x-axis cartoons. `0` aligns their bottom
  bounds, while `1` aligns their top bounds. Defaults to `0`.

- nudge_x:

  Horizontal adjustment of each cartoon, in millimetres. Positive values
  move cartoons to the right. Defaults to `0`.

- nudge_y:

  Vertical adjustment of each cartoon, in millimetres. Positive values
  move cartoons upward. Defaults to `0`.

- show_linkage:

  Whether to show glycosidic linkage annotations inside the cartoons.
  Defaults to `TRUE`.

- red_end:

  Reducing-end annotation passed to
  [`glycanGrob()`](https://glycoverse.github.io/glydraw/dev/reference/glycanGrob.md).
  Use `"~"` for a wave, or another string to display that text. Defaults
  to `""`.

- fuc_orient:

  Fuc-like triangle orientation passed to
  [`glycanGrob()`](https://glycoverse.github.io/glydraw/dev/reference/glycanGrob.md).

- edge_linewidth:

  Linkage linewidth passed to
  [`glycanGrob()`](https://glycoverse.github.io/glydraw/dev/reference/glycanGrob.md).

- node_linewidth:

  Node-border linewidth passed to
  [`glycanGrob()`](https://glycoverse.github.io/glydraw/dev/reference/glycanGrob.md).

- node_size:

  Node-size multiplier passed to
  [`glycanGrob()`](https://glycoverse.github.io/glydraw/dev/reference/glycanGrob.md).

- colors:

  Optional named character vector of monosaccharide fill colors passed
  to
  [`glycanGrob()`](https://glycoverse.github.io/glydraw/dev/reference/glycanGrob.md).

- hjust:

  Horizontal justification for y-axis cartoons. `0` aligns their left
  bounds, while `1` aligns their right bounds. Defaults to `1`.

## Value

A ggplot2 discrete position scale.

## Examples

``` r
glycans <- data.frame(
  structure = c(
    "Gal(b1-3)GalNAc(a1-",
    "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
  ),
  abundance = c(12, 8)
)

ggplot2::ggplot(glycans, ggplot2::aes(x = structure, y = abundance)) +
  ggplot2::geom_col() +
  scale_x_glycan()
```
