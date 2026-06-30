# Save fixed-size glycan cartoon image to local device.

This function saves the glycan cartoon to a file, with a suitable size.

## Usage

``` r
save_cartoon(cartoon, file, ..., dpi = 300, scale = 1)
```

## Arguments

- cartoon:

  A ggplot2 object returned by
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/reference/draw_cartoon.md).

- file:

  File name of glycan cartoon.

- ...:

  Ignored.

- dpi:

  Deprecated and ignored. Use `scale` to change the output size.

- scale:

  Numeric output-size multiplier. The default `1` saves the cartoon at
  its natural fixed size; `2` saves the same cartoon with twice the
  pixel width and height.

## Why not `width` and `height`?

The familiar
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
interface uses `width`, `height`, and `dpi` because ordinary ggplot2
plots are drawn into a user-chosen device size. glydraw cartoons are
different: the natural width and height are calculated from the glycan
structure so residues, linkages, labels, and borders stay comparable
across different glycans. If users supplied arbitrary `width` and
`height`, glydraw would either distort that structure-derived layout or
need to guess how to reconcile one requested size with the other.

`dpi` is also not the right control here because changing it alters how
point- and inch-based ggplot2 elements are rasterized relative to the
fixed cartoon canvas. glydraw therefore keeps an internal fixed design
scale and uses `scale` as a single multiplier for the final pixel
dimensions. This preserves the cartoon's aspect ratio and relative
appearance while still allowing larger or smaller output files.

## Examples

``` r
if (FALSE) { # \dontrun{
cartoon <- draw_cartoon("Gal(b1-3)GalNAc(a1-")
save_cartoon(cartoon, "p1.png", scale = 2)
} # }
```
