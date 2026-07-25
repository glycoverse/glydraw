#' Draw glycans as ggraph nodes
#'
#' `geom_node_glycan()` is a [ggraph][ggraph::ggraph] node layer backed by
#' [geom_glycan()]. It supplies the node layout's `x` and `y` columns as the
#' default position aesthetics and supports ggraph's `filter` aesthetic.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. The
#'   `structure` aesthetic is required. The `x` and `y` aesthetics default to
#'   the node coordinates calculated by ggraph.
#' @param data The node data to display. When `NULL`, the default, the layer
#'   uses the node data calculated by [ggraph::ggraph()].
#' @param position A position adjustment. Defaults to `"identity"`.
#' @param show.legend Logical. Should this layer be included in legends?
#' @param ... Arguments passed to [geom_glycan()], including fixed aesthetics
#'   and glycan rendering options.
#'
#' @section Aesthetics:
#' `geom_node_glycan()` understands the same aesthetics as [geom_glycan()].
#' `structure` is required, while `x` and `y` default to the node layout
#' coordinates. It additionally supports `filter`, a logical aesthetic that
#' selects which nodes are drawn.
#'
#' @returns A ggplot2 layer that can be added to a [ggraph::ggraph()] plot.
#'
#' @examples
#' if (requireNamespace("ggraph", quietly = TRUE)) {
#'   graph <- igraph::make_ring(2)
#'   igraph::V(graph)$glycan <- c(
#'     "GalNAc(a1-",
#'     "Gal(b1-3)GalNAc(a1-"
#'   )
#'
#'   ggraph::ggraph(graph, layout = "linear") +
#'     ggraph::geom_edge_link() +
#'     geom_node_glycan(ggplot2::aes(structure = .data$glycan))
#' }
#' @export
geom_node_glycan <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  show.legend = NA,
  ...
) {
  rlang::check_installed("ggraph", reason = "to use `geom_node_glycan()`")
  mapping <- .add_node_position_aesthetics(mapping)

  geom_glycan(
    mapping = mapping,
    data = data,
    stat = ggraph::StatFilter,
    position = position,
    show.legend = show.legend,
    inherit.aes = FALSE,
    ...
  )
}

.add_node_position_aesthetics <- function(mapping) {
  defaults <- ggplot2::aes(x = .data$x, y = .data$y)
  mapping <- c(
    as.list(mapping),
    defaults[!names(defaults) %in% names(mapping)]
  )
  class(mapping) <- class(defaults)
  mapping
}
