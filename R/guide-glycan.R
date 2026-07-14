# ggplot2 legend guide that replaces text labels with glycan cartoons.

#' Use glycan cartoons as legend labels
#'
#' `guide_glycan()` is a legend guide that replaces the usual text labels with
#' glycan cartoons while retaining the legend keys drawn by the plot layers.
#' The values returned by the scale's `labels` argument must therefore be
#' glycan structure strings supported by [glyparse::auto_parse()].
#'
#' @inheritParams ggplot2::guide_legend
#' @param size Positive scalar that uniformly scales each legend-label cartoon.
#'   Defaults to `0.4`.
#' @param orient Glycan drawing orientation, either `"H"` for horizontal or
#'   `"V"` for vertical. Defaults to `"H"`.
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
#' @returns A ggplot2 legend guide that draws glycan cartoons in place of text
#'   labels.
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
#' ggplot2::ggplot(
#'   glycans,
#'   ggplot2::aes(
#'     x = .data$structure,
#'     y = .data$abundance,
#'     fill = .data$structure
#'   )
#' ) +
#'   ggplot2::geom_col() +
#'   ggplot2::scale_fill_discrete(guide = guide_glycan())
#' @export
guide_glycan <- function(
  title = ggplot2::waiver(),
  theme = NULL,
  position = NULL,
  direction = NULL,
  override.aes = list(),
  nrow = NULL,
  ncol = NULL,
  reverse = FALSE,
  order = 0,
  size = 0.4,
  orient = c("H", "V"),
  show_linkage = TRUE,
  red_end = "",
  fuc_orient = c("flex", "up"),
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL
) {
  .validate_output_scale(size)
  orient <- rlang::arg_match(orient)
  fuc_orient <- rlang::arg_match(fuc_orient)
  checkmate::assert_number(edge_linewidth, lower = 0)
  checkmate::assert_number(node_linewidth, lower = 0)
  checkmate::assert_string(red_end, na.ok = FALSE)
  .validate_node_size(node_size)
  colors <- .validate_custom_colors(colors)
  show_linkage <- .resolve_linkage_visibility(show_linkage, node_size)

  if (!is.null(position)) {
    position <- rlang::arg_match(
      position,
      c("top", "right", "bottom", "left", "inside")
    )
  }

  ggplot2::new_guide(
    title = title,
    theme = theme,
    direction = direction,
    override.aes = ggplot2:::rename_aes(override.aes),
    nrow = nrow,
    ncol = ncol,
    reverse = reverse,
    order = order,
    position = position,
    glycan_size = size,
    glycan_orient = orient,
    glycan_show_linkage = show_linkage,
    glycan_red_end = red_end,
    glycan_fuc_orient = fuc_orient,
    glycan_edge_linewidth = edge_linewidth,
    glycan_node_linewidth = node_linewidth,
    glycan_node_size = node_size,
    glycan_colors = colors,
    available_aes = "any",
    name = "legend",
    super = GuideGlycan
  )
}

#' Build glycan cartoon legend labels
#'
#' @param key Guide key data supplied by ggplot2.
#' @param elements Guide theme elements supplied by ggplot2.
#' @param params Guide parameters supplied by ggplot2.
#'
#' @returns A list containing one `glycan_legend_label` grob per scale label.
#' @noRd
.build_glycan_legend_labels <- function(key, elements, params) {
  if (nrow(key) == 0 || length(key$.label) == 0) {
    return(rep(list(grid::nullGrob()), nrow(key)))
  }
  position <- elements$text_position
  gap <- switch(
    position,
    left = elements$spacing_x,
    right = elements$spacing_x,
    top = elements$spacing_y,
    bottom = elements$spacing_y
  )
  if (!grid::is.unit(gap)) {
    gap <- grid::unit(gap, "cm")
  }

  purrr::map(
    as.character(key$.label),
    .new_glycan_legend_label,
    params = params,
    gap = gap,
    position = position
  )
}

#' Construct one glycan cartoon legend label
#'
#' @param structure A glycan structure string used as a legend label.
#' @param params Guide parameters supplied by ggplot2.
#' @param gap Grid unit separating the legend key from its cartoon label.
#' @param position Position of the cartoon label relative to its legend key.
#'
#' @returns A measured `glycan_legend_label` grob containing one rendered
#'   `glycanGrob`.
#' @noRd
.new_glycan_legend_label <- function(structure, params, gap, position) {
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
  grob$name <- "guide.glycan.label"
  grob$glydraw_scale <- params$glycan_size
  grob$glydraw_orient <- params$glycan_orient
  grob$glydraw_border_px <- 0
  grob$glydraw_background <- FALSE
  grob$glydraw_hjust <- 0
  grob <- grid::makeContent(grob)
  child <- grob$children[[1]]
  child_width <- grid::grobWidth(child)
  child_height <- grid::grobHeight(child)
  grob$vp <- switch(
    position,
    left = grid::viewport(
      x = grid::unit(0, "npc"),
      y = grid::unit(0.5, "npc"),
      just = c("center", "center"),
      width = child_width,
      height = child_height
    ),
    right = grid::viewport(
      x = gap,
      y = grid::unit(0.5, "npc"),
      just = c("center", "center"),
      width = child_width,
      height = child_height
    ),
    top = grid::viewport(
      x = grid::unit(0, "npc"),
      y = gap,
      just = c("center", "bottom"),
      width = child_width,
      height = child_height
    ),
    bottom = grid::viewport(
      x = grid::unit(0, "npc"),
      y = grid::unit(0, "npc"),
      just = c("center", "bottom"),
      width = child_width,
      height = child_height
    )
  )

  label <- grid::gTree(
    children = grid::gList(grob),
    name = "guide.label",
    cl = "glycan_legend_label"
  )
  label$glydraw_structure <- structure
  label$glydraw_gap <- gap
  label$glydraw_position <- position
  label
}

#' Measure a glycan legend label
#'
#' @param x A `glycan_legend_label` grob.
#' @param dimension Either `"width"` or `"height"`.
#'
#' @returns The requested dimension of the rendered glycan cartoon.
#' @noRd
.glycan_legend_label_extent <- function(x, dimension) {
  glycan <- x$children[[1]]
  child <- glycan$children[[1]]
  gap <- x$glydraw_gap
  position <- x$glydraw_position

  switch(
    dimension,
    width = grid::grobWidth(child) +
      if (position %in% c("left", "right")) gap else grid::unit(0, "pt"),
    height = grid::grobHeight(child) +
      if (position %in% c("top", "bottom")) gap else grid::unit(0, "pt")
  )
}

#' Measure glycan legend label widths
#'
#' @param x A `glycan_legend_label` grob.
#'
#' @returns The rendered cartoon width as a grid unit.
#' @noRd
#' @exportS3Method grid::widthDetails
widthDetails.glycan_legend_label <- function(x) {
  .glycan_legend_label_extent(x, "width")
}

#' Measure glycan legend label heights
#'
#' @param x A `glycan_legend_label` grob.
#'
#' @returns The rendered cartoon height as a grid unit.
#' @noRd
#' @exportS3Method grid::heightDetails
heightDetails.glycan_legend_label <- function(x) {
  .glycan_legend_label_extent(x, "height")
}

#' Build fixed-size legend keys beside glycan labels
#'
#' @param decor Legend-key decorations prepared from plot layers.
#' @param grobs Guide grobs prepared by ggplot2.
#' @param elements Guide theme elements supplied by ggplot2.
#' @param params Guide parameters supplied by ggplot2.
#'
#' @returns A list of legend-key grobs whose viewports retain the dimensions
#'   reported by their layer glyphs instead of filling the glycan-label rows.
#' @noRd
.build_fixed_glycan_legend_keys <- function(decor, grobs, elements, params) {
  keys <- ggplot2:::GuideLegend$build_decor(
    decor = decor,
    grobs = grobs,
    elements = elements,
    params = params
  )
  just <- elements$key_just
  if (is.null(just)) {
    just <- c(0.5, 0.5)
  }

  purrr::map(keys, function(key) {
    key$vp <- grid::viewport(
      x = just[[1]],
      y = just[[2]],
      just = just,
      width = grid::unit(attr(key, "width"), "cm"),
      height = grid::unit(attr(key, "height"), "cm")
    )
    key
  })
}

#' ggplot2 guide that draws glycan cartoons as legend labels
#'
#' @noRd
GuideGlycan <- ggplot2::ggproto(
  "GuideGlycan",
  ggplot2:::GuideLegend,
  params = c(
    ggplot2:::GuideLegend$params,
    list(
      glycan_size = 0.4,
      glycan_orient = "H",
      glycan_show_linkage = TRUE,
      glycan_red_end = "",
      glycan_fuc_orient = "flex",
      glycan_edge_linewidth = 0.8,
      glycan_node_linewidth = 0.8,
      glycan_node_size = 1,
      glycan_colors = character()
    )
  ),
  build_labels = .build_glycan_legend_labels,
  build_decor = .build_fixed_glycan_legend_keys
)
