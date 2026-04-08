# Export all glycan structures to figures

This function calls
[`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md)
on each glycan structure in `x`, then calls
[`save_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/save_cartoon.md)
to save a figure for each of them. IUPAC-condensed nonmenclatures are
used as file names.

## Usage

``` r
export_cartoons(
  x,
  dirname,
  file_ext = "png",
  dpi = 300,
  show_linkage = TRUE,
  orient = c("H", "V")
)
```

## Arguments

- x:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html),
  a
  [`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  vector, or a character vector of any glycan structure text
  nomenclatures supported by
  [`glyparse::auto_parse()`](https://glycoverse.github.io/glyparse/reference/auto_parse.html).

- dirname:

  Directory name to save the cartoons.

- file_ext:

  File extention supported by
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
  Defaults to "png".

- dpi:

  Dots per inch. Defaults to 300.

- show_linkage:

  Show linkage annotation or not. Default is TRUE.

- orient:

  The orientation of glycan structure. "H" for horizontal, "V" for
  vertical. Default is "H"

## Value

The function returns the list of cartoons implicitly.

## Examples

``` r
if (FALSE) { # \dontrun{
library(glyexp)
export_cartoons(real_experiment, "path/to/save")
} # }
```
