# Create a glycan drawing style

`glydraw_style()` collects the rendering options shared by glydraw's
standalone drawings, grobs, ggplot2 layers, guides, and glycan scales.
Supply the result with `style =` to reuse a visual specification.
Explicit rendering arguments supplied to a drawing function override the
style.

## Usage

``` r
glydraw_style(
  show_linkage = TRUE,
  orient = c("left", "right", "up", "down"),
  fuc_orient = c("flex", "up"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  font_family = "",
  colors = NULL
)
```

## Arguments

- show_linkage:

  Whether to show glycosidic linkage annotations.

- orient:

  Direction in which the glycan extends from its reducing end: one of
  `"left"`, `"right"`, `"up"`, or `"down"`. Defaults to `"left"`.

- fuc_orient:

  Fuc-like triangle orientation: `"flex"` or `"up"`.

- red_end:

  Reducing-end annotation. Use `"~"` for a wave.

- edge_linewidth:

  Linewidth of glycosidic linkages.

- node_linewidth:

  Linewidth of node borders.

- node_size:

  Multiplier for the default node size.

- font_family:

  A length-one character string naming the font family used for linkage,
  substituent, and reducing-end text annotations. Portable choices are
  `"sans"`, `"serif"`, and `"mono"`. Other family names, such as
  installed system fonts, are graphics-device dependent. The default
  `""` uses the graphics device's default font.

- colors:

  Optional named character vector of monosaccharide fill-color
  overrides.

## Value

A `glydraw_style` object.

## Examples

``` r
vertical_style <- glydraw_style(orient = "up", show_linkage = FALSE)
draw_cartoon("Gal(b1-3)GalNAc(a1-", style = vertical_style)
```
