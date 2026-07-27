# Use glycan cartoons as legend labels

`guide_glycan()` is a legend guide that replaces the usual text labels
with glycan cartoons while retaining the legend keys drawn by the plot
layers. Mapped discrete values and values returned by the scale's
`labels` argument may be glycan structure strings supported by
[`glyparse::auto_parse()`](https://glycoverse.github.io/glyparse/reference/auto_parse.html)
or
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
vectors.

## Usage

``` r
guide_glycan(
  title = ggplot2::waiver(),
  theme = NULL,
  position = NULL,
  direction = NULL,
  override.aes = list(),
  nrow = NULL,
  ncol = NULL,
  reverse = FALSE,
  order = 0,
  size = 0.4,
  orient = c("left", "right", "up", "down"),
  hjust = 0,
  vjust = vjust_red_end(),
  show_linkage = TRUE,
  style = style_glydraw()
)
```

## Arguments

- title:

  A character string or expression indicating a title of guide. If
  `NULL`, the title is not shown. By default
  ([`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)),
  the name of the scale object or the name specified in
  [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) is used
  for the title.

- theme:

  A [`theme`](https://ggplot2.tidyverse.org/reference/theme.html) object
  to style the guide individually or differently from the plot's theme
  settings. The `theme` argument in the guide partially overrides, and
  is combined with, the plot's theme. Arguments that apply to a single
  legend are respected, most of which have the `legend`-prefix.
  Arguments that apply to combined legends (the legend box) are ignored,
  including `legend.position`, `legend.justification.*`,
  `legend.location` and `legend.box.*`.

- position:

  A character string indicating where the legend should be placed
  relative to the plot panels. One of "top", "right", "bottom", "left",
  or "inside".

- direction:

  A character string indicating the direction of the guide. One of
  "horizontal" or "vertical".

- override.aes:

  A list specifying aesthetic parameters of legend key. See details and
  examples.

- nrow, ncol:

  The desired number of rows and column of legends respectively.

- reverse:

  logical. If `TRUE` the order of legends is reversed.

- order:

  positive integer less than 99 that specifies the order of this guide
  among multiple guides. This controls the order in which multiple
  guides are displayed, not the contents of the guide itself. If 0
  (default), the order is determined by a secret algorithm.

- size:

  Positive scalar that uniformly scales each legend-label cartoon.
  Defaults to `0.4`.

- orient:

  Direction in which the glycan extends from its reducing end: one of
  `"left"`, `"right"`, `"up"`, or `"down"`. Defaults to `"left"`.

- hjust:

  Horizontal cartoon justification between `0` and `1`, or
  [`hjust_red_end()`](https://glycoverse.github.io/glydraw/dev/reference/hjust_red_end.md).
  It defaults to `0` for horizontal cartoons and
  [`hjust_red_end()`](https://glycoverse.github.io/glydraw/dev/reference/hjust_red_end.md)
  for vertical cartoons.

- vjust:

  Vertical cartoon justification between `0` and `1`, or
  [`vjust_red_end()`](https://glycoverse.github.io/glydraw/dev/reference/hjust_red_end.md).
  It defaults to
  [`vjust_red_end()`](https://glycoverse.github.io/glydraw/dev/reference/hjust_red_end.md)
  for horizontal cartoons and `0.5` for vertical cartoons.

- show_linkage:

  Whether to show glycosidic linkage annotations inside the cartoons.
  Defaults to `TRUE`.

- style:

  A
  [`style_glydraw()`](https://glycoverse.github.io/glydraw/dev/reference/style_glydraw.md)
  object that controls the cartoons' visual appearance.

## Value

A ggplot2 legend guide that draws glycan cartoons in place of text
labels.

## Examples

``` r
glycans <- data.frame(
  structure = c(
    "Gal(b1-3)GalNAc(a1-",
    "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
  ),
  abundance = c(12, 8)
)

ggplot2::ggplot(
  glycans,
  ggplot2::aes(
    x = .data$structure,
    y = .data$abundance,
    fill = .data$structure
  )
) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_discrete(guide = guide_glycan())
```
