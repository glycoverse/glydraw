# Draw glycans as ggraph nodes

`geom_node_glycan()` is a
[ggraph](https://ggraph.data-imaginist.com/reference/ggraph.html) node
layer backed by
[`geom_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/geom_glycan.md).
It supplies the node layout's `x` and `y` columns as the default
position aesthetics and supports ggraph's `filter` aesthetic.

## Usage

``` r
geom_node_glycan(
  mapping = NULL,
  data = NULL,
  position = "identity",
  show.legend = NA,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  The `structure` aesthetic is required. The `x` and `y` aesthetics
  default to the node coordinates calculated by ggraph.

- data:

  The node data to display. When `NULL`, the default, the layer uses the
  node data calculated by
  [`ggraph::ggraph()`](https://ggraph.data-imaginist.com/reference/ggraph.html).

- position:

  A position adjustment. Defaults to `"identity"`.

- show.legend:

  Logical. Should this layer be included in legends?

- ...:

  Arguments passed to
  [`geom_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/geom_glycan.md),
  including fixed aesthetics, drawing controls, and `style`.

## Value

A ggplot2 layer that can be added to a
[`ggraph::ggraph()`](https://ggraph.data-imaginist.com/reference/ggraph.html)
plot.

## Aesthetics

`geom_node_glycan()` understands the same aesthetics as
[`geom_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/geom_glycan.md).
`structure` is required, while `x` and `y` default to the node layout
coordinates. It additionally supports `filter`, a logical aesthetic that
selects which nodes are drawn.

## Examples

``` r
if (requireNamespace("ggraph", quietly = TRUE)) {
  graph <- igraph::make_ring(2)
  igraph::V(graph)$glycan <- c(
    "GalNAc(a1-",
    "Gal(b1-3)GalNAc(a1-"
  )

  ggraph::ggraph(graph, layout = "linear") +
    ggraph::geom_edge_link() +
    geom_node_glycan(ggplot2::aes(structure = .data$glycan))
}
```
