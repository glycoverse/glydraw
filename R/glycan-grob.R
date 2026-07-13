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
  border_px <- grob$glydraw_border_px
  if (is.null(border_px)) {
    border_px <- .default_cartoon_border_px
  }
  background <- grob$glydraw_background
  if (is.null(background)) {
    background <- TRUE
  }

  .assemble_cartoon_plot(
    grob$connect_df,
    grob$polygon_coor,
    grob$filled_color,
    grob$annotation_data,
    grob$show_linkage,
    grob$edge_linewidth,
    grob$node_linewidth,
    border_px = border_px,
    background = background
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
  scale <- x$glydraw_scale
  if (is.null(scale)) {
    scale <- 1
  }
  hjust <- x$glydraw_hjust
  if (is.null(hjust)) {
    hjust <- 0.5
  }
  vjust <- x$glydraw_vjust
  if (is.null(vjust)) {
    vjust <- 0.5
  }

  plot <- .glycan_grob_to_plot(x)
  size_px <- attr(plot, "glydraw_size_px")
  if (!isTRUE(all.equal(scale, 1))) {
    child <- .scaled_glycan_raster_grob(plot, scale, x$name)
  } else {
    child <- plot |>
      .strip_cartoon_class() |>
      ggplot2::ggplotGrob()
  }
  child <- .justify_glycan_child(
    child,
    size_px,
    scale,
    hjust,
    vjust
  )

  grid::setChildren(x, grid::gList(child))
}

#' Render a uniformly scaled glycan raster grob
#'
#' @param plot A `glydraw_cartoon` ggplot object.
#' @param scale Positive numeric whole-cartoon scale multiplier.
#' @param name String used to name the raster grob.
#'
#' @returns A raster grob whose physical width and height are the cartoon's
#'   natural dimensions multiplied by `scale`.
#' @noRd
.scaled_glycan_raster_grob <- function(plot, scale, name) {
  size_px <- attr(plot, "glydraw_size_px")
  raster <- .render_cartoon_raster(plot, scale = scale)

  grid::rasterGrob(
    raster,
    width = grid::unit(
      size_px[["width"]] / .default_cartoon_dpi * scale,
      "in"
    ),
    height = grid::unit(
      size_px[["height"]] / .default_cartoon_dpi * scale,
      "in"
    ),
    interpolate = TRUE,
    name = paste0(name, ".scaled")
  )
}

#' Justify a rendered glycan child around its panel anchor
#'
#' @param child Rendered grid grob containing one glycan cartoon.
#' @param size_px Named numeric vector with the cartoon's natural `width` and
#'   `height` in pixels.
#' @param scale Positive numeric whole-cartoon scale multiplier.
#' @param hjust Numeric horizontal justification.
#' @param vjust Numeric vertical justification.
#'
#' @returns `child` with a viewport that offsets the complete cartoon from its
#'   centered panel anchor. Centered cartoons are returned unchanged.
#' @noRd
.justify_glycan_child <- function(child, size_px, scale, hjust, vjust) {
  horizontally_centered <- isTRUE(all.equal(hjust, 0.5))
  vertically_centered <- isTRUE(all.equal(vjust, 0.5))
  if (horizontally_centered && vertically_centered) {
    return(child)
  }

  width_in <- size_px[["width"]] / .default_cartoon_dpi * scale
  height_in <- size_px[["height"]] / .default_cartoon_dpi * scale
  child$vp <- grid::viewport(
    x = grid::unit(0.5, "npc") +
      grid::unit((0.5 - hjust) * width_in, "in"),
    y = grid::unit(0.5, "npc") +
      grid::unit((0.5 - vjust) * height_in, "in")
  )
  child
}
