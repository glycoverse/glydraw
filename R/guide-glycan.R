# ggplot2 legend guide that replaces text labels with glycan cartoons.

#' Use glycan cartoons as legend labels
#'
#' `guide_glycan()` is a legend guide that replaces the usual text labels with
#' glycan cartoons while retaining the legend keys drawn by the plot layers.
#' Mapped discrete values and values returned by the scale's `labels` argument
#' may be glycan structure strings supported by [glyparse::auto_parse()] or
#' [glyrepr::glycan_structure()] vectors.
#'
#' @inheritParams ggplot2::guide_legend
#' @param size Positive scalar that uniformly scales each legend-label cartoon.
#'   Defaults to `0.4`.
#' @param orient Glycan drawing orientation, either `"H"` for horizontal or
#'   `"V"` for vertical. Defaults to `"H"`.
#' @param hjust Horizontal cartoon justification between `0` and `1`, or
#'   [hjust_red_end()]. It defaults to `0` for horizontal cartoons and
#'   [hjust_red_end()] for vertical cartoons.
#' @param vjust Vertical cartoon justification between `0` and `1`, or
#'   [vjust_red_end()]. It defaults to [vjust_red_end()] for horizontal
#'   cartoons and `0.5` for vertical cartoons.
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
#' @param style A `glydraw_style` object that supplies rendering options.
#'   Explicitly supplied rendering arguments override it.
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
  hjust = 0,
  vjust = vjust_red_end(),
  show_linkage = TRUE,
  red_end = "",
  fuc_orient = c("flex", "up"),
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL,
  style = NULL
) {
  hjust_is_missing <- missing(hjust)
  vjust_is_missing <- missing(vjust)
  style <- .resolve_glydraw_style(
    style = style,
    show_linkage = show_linkage,
    orient = orient,
    fuc_orient = fuc_orient,
    red_end = red_end,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size,
    colors = colors,
    .supplied = c(
      show_linkage = !missing(show_linkage),
      orient = !missing(orient),
      fuc_orient = !missing(fuc_orient),
      red_end = !missing(red_end),
      edge_linewidth = !missing(edge_linewidth),
      node_linewidth = !missing(node_linewidth),
      node_size = !missing(node_size),
      colors = !missing(colors)
    )
  )
  .validate_output_scale(size)
  orient <- style$orient
  if (hjust_is_missing && identical(orient, "V")) {
    hjust <- hjust_red_end()
  }
  if (vjust_is_missing && identical(orient, "V")) {
    vjust <- 0.5
  }
  .validate_red_end_justification_orientation(hjust, vjust, orient)
  .validate_glycan_justification_scalar(hjust, "hjust")
  .validate_glycan_justification_scalar(vjust, "vjust")
  show_linkage <- .resolve_linkage_visibility(
    style$show_linkage,
    style$node_size
  )

  if (!is.null(position)) {
    position <- rlang::arg_match(
      position,
      c("top", "right", "bottom", "left", "inside")
    )
  }

  names(override.aes) <- ggplot2::standardise_aes_names(names(override.aes))
  duplicated_aes <- unique(names(override.aes)[duplicated(names(override.aes))])
  if (length(duplicated_aes) > 0L) {
    cli::cli_warn(
      "Duplicated aesthetics after name standardisation: {.field {duplicated_aes}}"
    )
  }

  ggplot2::new_guide(
    title = title,
    theme = theme,
    direction = direction,
    override.aes = override.aes,
    nrow = nrow,
    ncol = ncol,
    reverse = reverse,
    order = order,
    position = position,
    glycan_size = size,
    glycan_orient = orient,
    glycan_hjust = hjust,
    glycan_vjust = vjust,
    glycan_show_linkage = show_linkage,
    glycan_red_end = style$red_end,
    glycan_fuc_orient = style$fuc_orient,
    glycan_edge_linewidth = style$edge_linewidth,
    glycan_node_linewidth = style$node_linewidth,
    glycan_node_size = style$node_size,
    glycan_colors = style$colors,
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
  grob$glydraw_hjust <- params$glycan_hjust
  grob$glydraw_vjust <- params$glycan_vjust
  grob$glydraw_border_px <- 0
  grob$glydraw_background <- FALSE
  grob <- grid::makeContent(grob)
  child <- grob$children[[1]]
  child_width <- grid::grobWidth(child)
  child_height <- .glycan_legend_child_height(child)
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

#' Measure a vertically justified glycan child
#'
#' @param child Rendered child grob containing its justification offset.
#'
#' @returns The height needed to contain the child around its centered legend
#'   anchor.
#' @noRd
.glycan_legend_child_height <- function(child) {
  offset <- child$glydraw_justification_offset
  if (is.null(offset)) {
    offset <- c(x = 0, y = 0)
  }

  grid::grobHeight(child) + grid::unit(2 * abs(offset[["y"]]), "in")
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
    height = .glycan_legend_child_height(child) +
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

#' Set up glycan legend theme elements
#'
#' @param params Guide parameters supplied by ggplot2.
#' @param elements Guide theme elements supplied by ggplot2.
#' @param theme Complete plot theme supplied by ggplot2.
#'
#' @returns Guide legend theme elements with vertical key spacing inherited
#'   from `legend.key.spacing` when `legend.key.spacing.y` is not set.
#' @noRd
.setup_glycan_legend_elements <- function(params, elements, theme) {
  has_vertical_spacing <- !is.null(theme[["legend.key.spacing.y"]]) ||
    (!is.null(params$theme) &&
      !is.null(params$theme[["legend.key.spacing.y"]]))

  elements <- ggplot2::GuideLegend$setup_elements(
    params = params,
    elements = elements,
    theme = theme
  )
  if (identical(params$direction, "vertical") && !has_vertical_spacing) {
    elements$spacing_y <- elements$spacing_x
  }
  elements
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
  keys <- ggplot2::GuideLegend$build_decor(
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
  ggplot2::GuideLegend,
  params = c(
    ggplot2::GuideLegend$params,
    list(
      glycan_size = 0.4,
      glycan_orient = "H",
      glycan_hjust = 0,
      glycan_vjust = .vjust_red_end,
      glycan_show_linkage = TRUE,
      glycan_red_end = "",
      glycan_fuc_orient = "flex",
      glycan_edge_linewidth = 0.8,
      glycan_node_linewidth = 0.8,
      glycan_node_size = 1,
      glycan_colors = character()
    )
  ),
  setup_elements = .setup_glycan_legend_elements,
  build_labels = .build_glycan_legend_labels,
  build_decor = .build_fixed_glycan_legend_keys
)
