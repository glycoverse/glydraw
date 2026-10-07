# Fit glycan cartoons to the available plotting space

Use `auto_glycan_size()` as the `size` argument of
[`geom_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/geom_glycan.md),
[`geom_node_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/geom_node_glycan.md),
[`scale_x_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/scale_x_glycan.md),
[`scale_y_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/scale_x_glycan.md),
[`guide_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/guide_glycan.md),
or
[`anno_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/anno_glycan.md)
to fit complete cartoons uniformly. Nodes, linkage text, lines, and
spacing shrink together. Cartoons in the same panel, axis, legend, or
heatmap annotation slice share a scale factor, preserving their relative
residue sizes.

## Usage

``` r
auto_glycan_size(max_size = NULL)
```

## Arguments

- max_size:

  Optional positive upper limit for the whole-cartoon scale multiplier.
  `NULL` uses `1` for panel cartoons and `0.4` for axis, legend, and
  heatmap labels. Automatic sizing only shrinks from this limit.

## Value

An automatic sizing specification accepted by glycan plotting
interfaces. It is not a ggplot2 aesthetic value.

## Details

Panel cartoons fit inside the panel and between neighbouring anchors,
and occupy at most 30% of its width or height. Axis and heatmap labels
fit their row or column spacing; their reserved width or height is
initially limited to 20% of the graphics device. Their size is refined
when drawn in the actual annotation viewport. Legend cartoons fit a
device-based budget for the complete collection of labels. These limits
are layout heuristics; automatic sizing does not detect other plot
layers or move anchors. Anchors on a panel boundary still need scale
expansion, and coincident anchors still overlap. Extremely dense figures
may need a larger output device to keep linkage text readable.

Supply a numeric `size` instead to retain a fixed whole-cartoon
multiplier. A mapped numeric `size` aesthetic in a glycan layer also
uses fixed sizing; use
[`ggplot2::scale_size_identity()`](https://ggplot2.tidyverse.org/reference/scale_identity.html)
for literal multipliers.

## Examples

``` r
glycans <- data.frame(
  structure = c("Gal(b1-3)GalNAc(a1-", "Gal(b1-3)[GlcNAc(b1-6)]GalNAc(a1-"),
  value = c(1, 2)
)
ggplot2::ggplot(glycans, ggplot2::aes(structure, value)) +
  ggplot2::geom_col() +
  geom_glycan(
    ggplot2::aes(structure = structure),
    size = auto_glycan_size(max_size = 0.7),
    orient = "up",
    vjust = 0
  ) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.4)))
```
