# Glycan grob construction and conversion helpers shared by grid and ggplot2
# drawing interfaces.

#' Construct a glycan grob
#'
#' `glycanGrob()` prepares the complete drawing specification for one glycan as
#' a grid grob. It is the low-level drawing primitive used by [draw_cartoon()].
#'
#' @inheritParams draw_cartoon
#'
#' @returns A `glycanGrob` object inheriting from [grid::gTree()].
#'
#' @examples
#' grob <- glycanGrob("Gal(b1-3)GalNAc(a1-")
#' grid::grid.draw(grob)
#' @export
glycanGrob <- function(
  structure,
  ...,
  show_linkage = TRUE,
  orient = c("H", "V"),
  fuc_orient = c("flex", "up"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL,
  highlight = NULL
) {
  checkmate::assert_number(edge_linewidth, lower = 0)
  checkmate::assert_number(node_linewidth, lower = 0)
  .validate_node_size(node_size)
  colors <- .validate_custom_colors(colors)
  fuc_orient <- rlang::arg_match(fuc_orient)
  inputs <- .prepare_cartoon_inputs(structure, highlight, orient, red_end)
  structure <- inputs$structure
  coor <- inputs$coor
  highlight <- inputs$highlight
  orient <- inputs$orient
  show_linkage <- .resolve_linkage_visibility(show_linkage, node_size)

  gly_list <- .cartoon_residue_data(structure, coor, highlight, fuc_orient)
  polygon_coor <- .residue_polygon_data(
    gly_list,
    .default_node_point_size * node_size
  )
  filled_color <- .resolve_residue_fill_colors(polygon_coor, colors)
  annotation_data <- .cartoon_text_annotation_data(
    structure,
    coor,
    orient,
    red_end,
    highlight,
    node_size = node_size
  )
  connect_df <- .cartoon_segment_data(
    structure,
    coor,
    annotation_data$reducing_info$segment,
    gly_list
  )

  grid::gTree(
    connect_df = connect_df,
    polygon_coor = polygon_coor,
    filled_color = filled_color,
    annotation_data = annotation_data,
    show_linkage = show_linkage,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    cl = "glycanGrob"
  )
}

#' Convert a glycan grob to a cartoon plot
#'
#' @param grob A `glycanGrob` object returned by [glycanGrob()].
#'
#' @returns A `glydraw_cartoon` ggplot object with fixed-size metadata.
#' @noRd
.glycan_grob_to_plot <- function(grob) {
  checkmate::assert_class(grob, "glycanGrob")

  .assemble_cartoon_plot(
    grob$connect_df,
    grob$polygon_coor,
    grob$filled_color,
    grob$annotation_data,
    grob$show_linkage,
    grob$edge_linewidth,
    grob$node_linewidth
  )
}

#' Populate the drawing content of a glycan grob
#'
#' @param x A `glycanGrob` object.
#'
#' @returns `x` with its ggplot-backed grid drawing added as a child grob.
#' @noRd
#' @importFrom grid makeContent
#' @export
makeContent.glycanGrob <- function(x) {
  plot <- .glycan_grob_to_plot(x) |>
    .strip_cartoon_class()
  child <- ggplot2::ggplotGrob(plot)

  grid::setChildren(x, grid::gList(child))
}
