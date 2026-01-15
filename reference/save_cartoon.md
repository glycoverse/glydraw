# Save fixed-size glycan cartoon image to local device.

In theory, you can just use
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
to save the cartoons plotted by
[`draw_cartoon()`](https://glycoverse.github.io/glydraw/reference/draw_cartoon.md).
However, you can have trouble finding the best sizes for each cartoon to
make them look alike. This function is designed to save the cartoons
with self-adjusted sizes, based on the size of the glycans, so that when
glycans with different sizes are put together, they will look alike.

## Usage

``` r
save_cartoon(cartoon, file, dpi = 300)
```

## Arguments

- cartoon:

  A ggplot2 object returned by
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/reference/draw_cartoon.md).

- file:

  File name of glycan cartoon.

- dpi:

  Dots per inch, default = 300.

## Examples

``` r
if (FALSE) { # \dontrun{
cartoon <- draw_cartoon("Gal(b1-3)GalNAc(a1-")
save_cartoon(cartoon, "p1.png", dpi = 300)
} # }
```
