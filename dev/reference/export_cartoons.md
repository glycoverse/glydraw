# Export all glycan structures to figures

This function calls
[`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md)
on each glycan structure in `x`, then calls
[`save_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/save_cartoon.md)
to save a figure for each of them. IUPAC-condensed nomenclatures are
used as file names. If `x` is a named character vector or named
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
vector, the vector names are used as file names.

## Usage

``` r
export_cartoons(
  x,
  dirname,
  file_ext = "png",
  dpi = 300,
  show_linkage = TRUE,
  orient = c("H", "V"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8
)
```

## Arguments

- x:

  A
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  vector, or a character vector of any glycan structure text
  nomenclatures supported by
  [`glyparse::auto_parse()`](https://glycoverse.github.io/glyparse/reference/auto_parse.html).

- dirname:

  Directory name to save the cartoons. If it does not exist, it is
  created.

- file_ext:

  File extension supported by
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
  Defaults to "png".

- dpi:

  Dots per inch. Defaults to 300.

- show_linkage:

  Show linkage annotation or not. Default is TRUE.

- orient:

  The orientation of glycan structure. "H" for horizontal, "V" for
  vertical. Default is "H"

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

## Value

The function returns the list of cartoons implicitly.

## Examples

``` r
export_cartoons(
  c(
    "Man(a1-3)Man(b1-4)GlcNAc(b1-",
    "Gal(b1-4)GlcNAc(b1-"
  ),
  "path/to/save"
)
#> ℹ Exporting 2 glycan cartoons.
```
