#' Use glycan cartoons as axis labels
#'
#' `scale_x_glycan()` and `scale_y_glycan()` are discrete position scales that
#' replace text tick labels with compact glycan cartoons. The discrete values,
#' or the values returned by the scale's `labels` argument, must be glycan
#' structure strings supported by [glyparse::auto_parse()]. X-axis cartoons are
#' vertical and bottom-aligned by default, while y-axis cartoons are horizontal
#' and right-aligned by default.
#'
#' @param name The name of the scale, displayed as the axis title. Use `NULL`
#'   to remove the title.
#' @param ... Arguments passed to [ggplot2::scale_x_discrete()] or
#'   [ggplot2::scale_y_discrete()], including `breaks` and `labels`.
#' @param palette A palette function that returns discrete position values.
#' @param expand Expansion applied to the discrete position scale.
#' @param position Position of the axis.
#' @param sec.axis A secondary axis specification.
#' @param continuous.limits Continuous limits used to position the discrete
#'   scale.
#' @param size Positive scalar that uniformly scales each axis-label cartoon.
#'   Defaults to `0.4`.
#' @param vjust Vertical justification for x-axis cartoons. `0` aligns their
#'   bottom bounds, while `1` aligns their top bounds. Defaults to `0`.
#' @param hjust Horizontal justification for y-axis cartoons. `0` aligns their
#'   left bounds, while `1` aligns their right bounds. Defaults to `1`.
#' @param nudge_x Horizontal adjustment of each cartoon, in millimetres.
#'   Positive values move cartoons to the right. Defaults to `0`.
#' @param nudge_y Vertical adjustment of each cartoon, in millimetres. Positive
#'   values move cartoons upward. Defaults to `0`.
#' @param show_linkage Whether to show glycosidic linkage annotations inside
#'   the cartoons. Defaults to `TRUE`.
#' @param red_end Reducing-end annotation passed to [glycanGrob()]. Use `"~"`
#'   for a wave, or another string to display that text. Defaults to `""`.
#' @param fuc_orient Fuc-like triangle orientation passed to [glycanGrob()].
#' @param edge_linewidth Linkage linewidth passed to [glycanGrob()].
#' @param node_linewidth Node-border linewidth passed to [glycanGrob()].
#' @param node_size Node-size multiplier passed to [glycanGrob()].
#' @param colors Optional named character vector of monosaccharide fill colors
#'   passed to [glycanGrob()].
#'
#' @returns A ggplot2 discrete position scale.
#'
#' @examples
#' glycans <- data.frame(
#'   structure = c(
#'     "Gal(b1-3)GalNAc(a1-",
#'     "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
#'   ),
#'   abundance = c(12, 8)
#' )
#'
#' ggplot2::ggplot(glycans, ggplot2::aes(x = structure, y = abundance)) +
#'   ggplot2::geom_col() +
#'   scale_x_glycan()
#' @export
scale_x_glycan <- function(
  name = ggplot2::waiver(),
  ...,
  palette = seq_len,
  expand = ggplot2::waiver(),
  position = "bottom",
  sec.axis = ggplot2::waiver(),
  continuous.limits = NULL,
  size = 0.4,
  vjust = 0,
  nudge_x = 0,
  nudge_y = 0,
  show_linkage = TRUE,
  red_end = "",
  fuc_orient = c("flex", "up"),
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL
) {
  guide <- .new_glycan_axis_guide(
    orient = "V",
    size = size,
    hjust = 0.5,
    vjust = vjust,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    show_linkage = show_linkage,
    red_end = red_end,
    fuc_orient = fuc_orient,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size,
    colors = colors
  )

  ggplot2::scale_x_discrete(
    name = name,
    ...,
    palette = palette,
    expand = expand,
    guide = guide,
    position = position,
    sec.axis = sec.axis,
    continuous.limits = continuous.limits
  )
}

#' @rdname scale_x_glycan
#' @export
scale_y_glycan <- function(
  name = ggplot2::waiver(),
  ...,
  palette = seq_len,
  expand = ggplot2::waiver(),
  position = "left",
  sec.axis = ggplot2::waiver(),
  continuous.limits = NULL,
  size = 0.4,
  hjust = 1,
  nudge_x = 0,
  nudge_y = 0,
  show_linkage = TRUE,
  red_end = "",
  fuc_orient = c("flex", "up"),
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL
) {
  guide <- .new_glycan_axis_guide(
    orient = "H",
    size = size,
    hjust = hjust,
    vjust = 0.5,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    show_linkage = show_linkage,
    red_end = red_end,
    fuc_orient = fuc_orient,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size,
    colors = colors
  )

  ggplot2::scale_y_discrete(
    name = name,
    ...,
    palette = palette,
    expand = expand,
    guide = guide,
    position = position,
    sec.axis = sec.axis,
    continuous.limits = continuous.limits
  )
}

#' Create a ggplot2 guide that draws glycan cartoons as axis labels
#'
#' @param orient Glycan drawing orientation.
#' @param size Positive whole-cartoon scale multiplier.
#' @param hjust Horizontal glycan justification.
#' @param vjust Vertical glycan justification.
#' @param nudge_x Horizontal adjustment in millimetres.
#' @param nudge_y Vertical adjustment in millimetres.
#' @param show_linkage Whether to draw glycosidic linkage annotations.
#' @param red_end Reducing-end annotation.
#' @param fuc_orient Fuc-like triangle orientation.
#' @param edge_linewidth Linkage linewidth.
#' @param node_linewidth Node-border linewidth.
#' @param node_size Node-size multiplier.
#' @param colors Named monosaccharide fill-color overrides.
#'
#' @returns A ggplot2 `GuideGlycanAxis` object.
#' @noRd
.new_glycan_axis_guide <- function(
  orient,
  size,
  hjust,
  vjust,
  nudge_x,
  nudge_y,
  show_linkage,
  red_end,
  fuc_orient,
  edge_linewidth,
  node_linewidth,
  node_size,
  colors
) {
  orient <- rlang::arg_match(orient, c("H", "V"))
  .validate_output_scale(size)
  checkmate::assert_number(hjust, lower = 0, upper = 1)
  checkmate::assert_number(vjust, lower = 0, upper = 1)
  checkmate::assert_number(nudge_x, finite = TRUE)
  checkmate::assert_number(nudge_y, finite = TRUE)
  checkmate::assert_number(edge_linewidth, lower = 0)
  checkmate::assert_number(node_linewidth, lower = 0)
  checkmate::assert_string(red_end, na.ok = FALSE)
  .validate_node_size(node_size)
  colors <- .validate_custom_colors(colors)
  fuc_orient <- rlang::arg_match(fuc_orient, c("flex", "up"))
  show_linkage <- .resolve_linkage_visibility(show_linkage, node_size)

  ggplot2::new_guide(
    title = ggplot2::waiver(),
    theme = NULL,
    check.overlap = FALSE,
    angle = ggplot2::waiver(),
    n.dodge = 1,
    minor.ticks = FALSE,
    cap = "none",
    glycan_orient = orient,
    glycan_size = size,
    glycan_hjust = hjust,
    glycan_vjust = vjust,
    glycan_nudge_x = nudge_x,
    glycan_nudge_y = nudge_y,
    glycan_show_linkage = show_linkage,
    glycan_red_end = red_end,
    glycan_fuc_orient = fuc_orient,
    glycan_edge_linewidth = edge_linewidth,
    glycan_node_linewidth = node_linewidth,
    glycan_node_size = node_size,
    glycan_colors = colors,
    available_aes = c("x", "y"),
    order = 0,
    position = ggplot2::waiver(),
    name = "axis",
    super = GuideGlycanAxis
  )
}

#' Build glycan axis-label grobs
#'
#' @param key Guide key data supplied by ggplot2.
#' @param elements Guide theme elements supplied by ggplot2.
#' @param params Guide parameters supplied by ggplot2.
#'
#' @returns A list containing one `glycan_axis_labels` grob.
#' @noRd
.build_glycan_axis_labels <- function(key, elements, params) {
  if (".type" %in% names(key)) {
    key <- key[key$.type == "major", , drop = FALSE]
  }
  if (nrow(key) == 0) {
    return(list(grid::nullGrob()))
  }

  labels <- as.character(key$.label)
  positions <- key[[params$aes]]
  children <- purrr::map2(
    labels,
    positions,
    .new_glycan_axis_label,
    params = params
  )

  list(
    grid::gTree(
      children = rlang::exec(grid::gList, !!!children),
      cl = "glycan_axis_labels"
    )
  )
}

#' Construct and position a glycan axis-label grob
#'
#' @param structure A glycan structure string used as an axis label.
#' @param position Numeric axis position in the guide viewport.
#' @param params Guide parameters supplied by ggplot2.
#'
#' @returns A positioned, rendered `glycanGrob`.
#' @noRd
.new_glycan_axis_label <- function(structure, position, params) {
  grob <- glycanGrob(
    structure,
    show_linkage = params$glycan_show_linkage,
    orient = params$glycan_orient,
    red_end = params$glycan_red_end,
    fuc_orient = params$glycan_fuc_orient,
    edge_linewidth = params$glycan_edge_linewidth,
    node_linewidth = params$glycan_node_linewidth,
    node_size = params$glycan_node_size,
    colors = params$glycan_colors
  )
  grob$glydraw_scale <- params$glycan_size
  grob$glydraw_hjust <- params$glycan_hjust
  grob$glydraw_vjust <- params$glycan_vjust
  grob$glydraw_nudge_x <- params$glycan_nudge_x
  grob$glydraw_nudge_y <- params$glycan_nudge_y
  grob$glydraw_border_px <- 0
  grob$glydraw_background <- FALSE
  grob$glydraw_expand <- FALSE
  grob$glydraw_axis_vertical <- identical(params$glycan_orient, "V")
  grob <- grid::makeContent(grob)
  grob$vp <- .glycan_axis_label_viewport(
    position,
    vertical = params$vertical,
    hjust = params$glycan_hjust,
    vjust = params$glycan_vjust,
    nudge_x = params$glycan_nudge_x,
    nudge_y = params$glycan_nudge_y
  )
  grob
}

#' Position a glycan label in an axis guide viewport
#'
#' @param position Numeric axis position in the guide viewport.
#' @param vertical Whether the axis guide is vertical.
#' @param hjust Horizontal glycan justification.
#' @param vjust Vertical glycan justification.
#' @param nudge_x Horizontal adjustment in millimetres.
#' @param nudge_y Vertical adjustment in millimetres.
#'
#' @returns A grid viewport aligned with the requested cartoon justification.
#' @noRd
.glycan_axis_label_viewport <- function(
  position,
  vertical,
  hjust,
  vjust,
  nudge_x,
  nudge_y
) {
  if (vertical) {
    return(
      grid::viewport(
        x = grid::unit(hjust, "npc") + grid::unit(nudge_x, "mm"),
        y = grid::unit(position, "native") + grid::unit(nudge_y, "mm"),
        clip = "off"
      )
    )
  }

  grid::viewport(
    x = grid::unit(position, "native") + grid::unit(nudge_x, "mm"),
    y = grid::unit(vjust, "npc") + grid::unit(nudge_y, "mm"),
    clip = "off"
  )
}

#' Calculate a glycan-axis-label extent
#'
#' @param x A `glycan_axis_labels` grob.
#' @param dimension Either `"width"` or `"height"`.
#'
#' @returns A grid unit describing the largest label extent.
#' @noRd
.glycan_axis_label_extent <- function(x, dimension) {
  if (length(x$children) == 0) {
    return(grid::unit(0, "pt"))
  }

  extent <- switch(
    dimension,
    width = grid::grobWidth,
    height = grid::grobHeight
  )
  extents <- purrr::map(
    x$children,
    ~ extent(.x$children[[1]])
  )
  rlang::exec(grid::unit.pmax, !!!extents)
}

#' Measure glycan axis-label widths
#'
#' @param x A `glycan_axis_labels` grob.
#'
#' @returns A grid unit describing the widest glycan axis label.
#' @noRd
#' @exportS3Method grid::widthDetails
widthDetails.glycan_axis_labels <- function(x) {
  .glycan_axis_label_extent(x, "width")
}

#' Measure glycan axis-label heights
#'
#' @param x A `glycan_axis_labels` grob.
#'
#' @returns A grid unit describing the tallest glycan axis label.
#' @noRd
#' @exportS3Method grid::heightDetails
heightDetails.glycan_axis_labels <- function(x) {
  .glycan_axis_label_extent(x, "height")
}

#' ggplot2 guide that draws glycan cartoons as axis labels
#'
#' @noRd
GuideGlycanAxis <- ggplot2::ggproto(
  "GuideGlycanAxis",
  ggplot2:::GuideAxis,
  params = c(
    ggplot2:::GuideAxis$params,
    list(
      glycan_orient = "H",
      glycan_size = 0.4,
      glycan_hjust = 0.5,
      glycan_vjust = 0.5,
      glycan_nudge_x = 0,
      glycan_nudge_y = 0,
      glycan_show_linkage = FALSE,
      glycan_red_end = "",
      glycan_fuc_orient = "flex",
      glycan_edge_linewidth = 0.8,
      glycan_node_linewidth = 0.8,
      glycan_node_size = 1,
      glycan_colors = character()
    )
  ),
  build_labels = .build_glycan_axis_labels
)
