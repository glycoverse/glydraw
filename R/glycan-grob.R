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
  orient = c("left", "right", "up", "down"),
  highlight = NULL,
  style = style_glydraw(),
  red_end = NULL
) {
  orient <- rlang::arg_match(orient, error_call = NULL)
  checkmate::assert_class(style, "glydraw_style")
  red_end <- .resolve_red_end(red_end, style)
  style <- .make_glydraw_style(
    fuc_orient = style$fuc_orient,
    red_end = red_end,
    edge_linewidth = style$edge_linewidth,
    node_linewidth = style$node_linewidth,
    node_size = style$node_size,
    font_family = style$font_family,
    colors = style$colors,
    red_end_length = style$red_end_length
  )
  inputs <- .prepare_cartoon_inputs(
    structure,
    highlight,
    orient,
    style$red_end
  )
  structure <- inputs$structure
  coor <- inputs$coor
  highlight <- inputs$highlight
  orient <- inputs$orient
  show_linkage <- .resolve_linkage_visibility(
    show_linkage,
    style$node_size
  )

  gly_list <- .cartoon_residue_data(
    structure,
    coor,
    highlight,
    style$fuc_orient
  )
  polygon_coor <- .residue_polygon_data(
    gly_list,
    .default_node_point_size * style$node_size
  )
  filled_color <- .resolve_residue_fill_colors(polygon_coor, style$colors)
  annotation_data <- .cartoon_text_annotation_data(
    structure,
    coor,
    orient,
    style$red_end,
    highlight,
    node_size = style$node_size,
    show_linkage = show_linkage,
    red_end_length = style$red_end_length
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
    reducing_end_coor = c(
      x = unname(coor[nrow(coor), "x"]),
      y = unname(coor[nrow(coor), "y"])
    ),
    filled_color = filled_color,
    annotation_data = annotation_data,
    show_linkage = show_linkage,
    edge_linewidth = style$edge_linewidth,
    node_linewidth = style$node_linewidth,
    font_family = style$font_family,
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
    grob$font_family,
    border_px = border_px,
    background = background
  )
}

#' Populate the drawing content of a glycan grob
#'
#' @param x A `glycanGrob` object.
#'
#' @returns `x` with its native grid drawing added as a child grob.
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
  reducing_end_coor <- x$reducing_end_coor
  if (is.null(reducing_end_coor)) {
    reducing_end_coor <- c(x = 0, y = 0)
  }

  layout <- .cartoon_grid_layout(x)
  if (
    isTRUE(all.equal(scale, 1)) &&
      !layout$background &&
      layout$border_px == 0
  ) {
    child <- .cartoon_grid_gtable(x, layout, x$name)
  } else {
    child <- .cartoon_grid_grob(x, layout, scale, x$name)
  }
  child <- .justify_glycan_child(
    child,
    layout,
    scale,
    hjust,
    vjust,
    reducing_end_coor
  )

  grid::setChildren(x, grid::gList(child))
}

#' Justify a rendered glycan child around its panel anchor
#'
#' @param child Rendered grid grob containing one glycan cartoon.
#' @param layout Grid layout metadata from `.cartoon_grid_layout()`.
#' @param scale Positive numeric whole-cartoon scale multiplier.
#' @param hjust Numeric horizontal justification.
#' @param vjust Numeric vertical justification.
#' @param reducing_end_coor Named numeric vector containing the reducing-end
#'   residue's `x` and `y` coordinates.
#'
#' @returns `child` with a viewport that offsets the complete cartoon from its
#'   centered panel anchor. Centered cartoons are returned unchanged.
#' @noRd
.justify_glycan_child <- function(
  child,
  layout,
  scale,
  hjust,
  vjust,
  reducing_end_coor
) {
  horizontally_centered <- isTRUE(all.equal(hjust, 0.5))
  vertically_centered <- isTRUE(all.equal(vjust, 0.5))
  if (horizontally_centered && vertically_centered) {
    child$glydraw_justification_offset <- c(x = 0, y = 0)
    return(child)
  }

  offset <- .cartoon_grid_justification_offset(
    layout,
    scale,
    hjust,
    vjust,
    reducing_end_coor
  )
  child$vp <- grid::viewport(
    x = grid::unit(0.5, "npc") +
      grid::unit(offset[["x"]], "in"),
    y = grid::unit(0.5, "npc") +
      grid::unit(offset[["y"]], "in")
  )
  child$glydraw_justification_offset <- offset
  child
}

#' Calculate a glycan justification offset from its unexpanded coordinates
#'
#' @param layout Grid layout metadata from `.cartoon_grid_layout()`.
#' @param scale Positive numeric whole-cartoon scale multiplier.
#' @param hjust Numeric horizontal justification.
#' @param vjust Numeric vertical justification.
#' @param reducing_end_coor Named numeric vector containing the reducing-end
#'   residue's `x` and `y` coordinates.
#'
#' @returns A named numeric vector `c(x, y)` containing offsets in inches.
#' @noRd
.cartoon_grid_justification_offset <- function(
  layout,
  scale,
  hjust,
  vjust,
  reducing_end_coor = c(x = 0, y = 0)
) {
  x_anchor <- .normalized_cartoon_anchor(
    layout$data_ranges$x,
    layout$panel_ranges$x,
    hjust,
    red_end_coordinate = if (.is_red_end_justification(hjust, "hjust")) {
      reducing_end_coor[["x"]]
    }
  )
  y_anchor <- .normalized_cartoon_anchor(
    layout$data_ranges$y,
    layout$panel_ranges$y,
    vjust,
    red_end_coordinate = if (.is_red_end_justification(vjust, "vjust")) {
      reducing_end_coor[["y"]]
    }
  )

  c(
    x = (0.5 - x_anchor) *
      layout$size_px[["width"]] /
      .default_cartoon_dpi *
      scale,
    y = (0.5 - y_anchor) *
      layout$size_px[["height"]] /
      .default_cartoon_dpi *
      scale
  )
}

#' Calculate a glycan justification offset from its unexpanded coordinates
#'
#' @param plot A `glydraw_cartoon` ggplot object.
#' @param size_px Named numeric vector with the rendered cartoon's `width` and
#'   `height` in pixels.
#' @param scale Positive numeric whole-cartoon scale multiplier.
#' @param hjust Numeric horizontal justification.
#' @param vjust Numeric vertical justification.
#' @param reducing_end_coor Named numeric vector containing the reducing-end
#'   residue's `x` and `y` coordinates.
#'
#' @returns A named numeric vector `c(x, y)` containing offsets in inches.
#' @noRd
.glycan_justification_offset <- function(
  plot,
  size_px,
  scale,
  hjust,
  vjust,
  reducing_end_coor = c(x = 0, y = 0)
) {
  built <- ggplot2::ggplot_build(plot)
  x_anchor <- .normalized_cartoon_anchor(
    built$layout$panel_scales_x[[1]]$range$range,
    built$layout$panel_params[[1]]$x.range,
    hjust,
    red_end_coordinate = if (.is_red_end_justification(hjust, "hjust")) {
      reducing_end_coor[["x"]]
    }
  )
  y_anchor <- .normalized_cartoon_anchor(
    built$layout$panel_scales_y[[1]]$range$range,
    built$layout$panel_params[[1]]$y.range,
    vjust,
    red_end_coordinate = if (.is_red_end_justification(vjust, "vjust")) {
      reducing_end_coor[["y"]]
    }
  )

  c(
    x = (0.5 - x_anchor) *
      size_px[["width"]] /
      .default_cartoon_dpi *
      scale,
    y = (0.5 - y_anchor) *
      size_px[["height"]] /
      .default_cartoon_dpi *
      scale
  )
}

#' Normalize a raw cartoon anchor within its expanded panel range
#'
#' @param data_range Numeric two-element unexpanded data range.
#' @param panel_range Numeric two-element expanded panel range.
#' @param justification Numeric justification value.
#' @param red_end_coordinate Optional numeric reducing-end coordinate. When
#'   supplied, it is used instead of calculating an anchor from
#'   `justification`.
#'
#' @returns Numeric scalar between the panel edges for justification values
#'   between zero and one.
#' @noRd
.normalized_cartoon_anchor <- function(
  data_range,
  panel_range,
  justification,
  red_end_coordinate = NULL
) {
  anchor <- if (is.null(red_end_coordinate)) {
    data_range[[1]] + justification * diff(data_range)
  } else {
    red_end_coordinate
  }
  (anchor - panel_range[[1]]) / diff(panel_range)
}
