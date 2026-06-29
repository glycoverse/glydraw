# Export all glycan structures to figures

Draw and save one cartoon for each glycan structure in `x`.

## Usage

``` r
export_cartoons(
  x,
  dirname,
  ...,
  file_ext = "png",
  dpi = 300,
  scale = 1,
  show_linkage = TRUE,
  orient = c("H", "V"),
  fuc_orient = c("flex", "up"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1
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

- ...:

  Ignored.

- file_ext:

  File extension supported by
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
  Defaults to "png".

- dpi:

  Deprecated and ignored. Use `scale` to change the output size.

- scale:

  Numeric output-size multiplier passed to
  [`save_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/save_cartoon.md).

- show_linkage:

  Show glycosidic linkage annotations or not. Default is TRUE.
  Substituent annotations are always shown.

- orient:

  The orientation of glycan structure. "H" for horizontal, "V" for
  vertical. Default is "H"

- fuc_orient:

  Fuc triangle orientation. `"flex"` points non-reducing Fuc residues
  toward their rendered linkage direction, while `"up"` draws all Fuc
  triangles pointing upward. Reducing-end Fuc residues always point
  upward. Defaults to `"flex"`.

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

## Value

The function returns the list of cartoons implicitly.

## File names

IUPAC-condensed nomenclatures are used as file names. If `x` is a named
character vector or named
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
vector, the vector names are used as file names.

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
export_cartoons(
  c(
    "Man(a1-3)Man(b1-4)GlcNAc(b1-",
    "Gal(b1-4)GlcNAc(b1-"
  ),
  "path/to/save"
)
#> ℹ Exporting 2 glycan cartoons.
```
