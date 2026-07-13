# ggplot2 layer helpers for positioning glycan grobs inside plot panels.

#' Draw glycans at ggplot2 positions
#'
#' `geom_glycan()` draws one fixed-size glycan cartoon for each data row. Each
#' cartoon is centered at its mapped `x` and `y` position and retains the
#' structure-derived dimensions and appearance used by [draw_cartoon()].
#' Like points and text, the fixed-size cartoons do not expand the position
#' scales beyond their center coordinates. Use scale expansion or explicit
#' coordinate limits when the cartoons need more room around the panel edges.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. The
#'   `x`, `y`, and `structure` aesthetics are required.
#' @param data The data to be displayed in this layer. When `NULL`, the default,
#'   the data is inherited from the plot data.
#' @param stat The statistical transformation to use on the data for this
#'   layer. Defaults to `"identity"`.
#' @param position A position adjustment to use on the data for this layer.
#'   Defaults to `"identity"`.
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather than
#'   combining with them.
#' @inheritParams draw_cartoon
#'
#' @section Aesthetics:
#' `geom_glycan()` requires the following aesthetics:
#'
#' - `x`
#' - `y`
#' - `structure`, containing glycan structure strings or
#'   [glyrepr::glycan_structure()] values
#'
#' @returns A ggplot2 layer that can be added to a [ggplot2::ggplot()] object.
#'
#' @examples
#' glycans <- data.frame(
#'   x = c(1, 3),
#'   y = c(1, 2),
#'   structure = c(
#'     "Gal(b1-3)GalNAc(a1-",
#'     "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
#'   )
#' )
#'
#' ggplot2::ggplot(
#'   glycans,
#'   ggplot2::aes(x = x, y = y, structure = structure)
#' ) +
#'   geom_glycan() +
#'   ggplot2::coord_cartesian(
#'     xlim = c(0, 4),
#'     ylim = c(0, 3),
#'     expand = FALSE
#'   )
#' @export
geom_glycan <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  show_linkage = TRUE,
  orient = c("H", "V"),
  fuc_orient = c("flex", "up"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL,
  highlight = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  checkmate::assert_number(edge_linewidth, lower = 0)
  checkmate::assert_number(node_linewidth, lower = 0)
  checkmate::assert_string(red_end, na.ok = FALSE)
  .validate_node_size(node_size)
  colors <- .validate_custom_colors(colors)
  orient <- rlang::arg_match(orient)
  fuc_orient <- rlang::arg_match(fuc_orient)
  show_linkage <- .resolve_linkage_visibility(show_linkage, node_size)

  ggplot2::layer(
    mapping = mapping,
    data = data,
    geom = GeomGlycan,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      show_linkage = show_linkage,
      orient = orient,
      fuc_orient = fuc_orient,
      red_end = red_end,
      edge_linewidth = edge_linewidth,
      node_linewidth = node_linewidth,
      node_size = node_size,
      colours = colors,
      highlight = highlight,
      na.rm = na.rm,
      ...
    )
  )
}

#' Draw a panel of positioned glycan grobs
#'
#' @param data Data frame prepared by ggplot2 with `x`, `y`, and `structure`
#'   columns.
#' @param panel_params Panel scale parameters supplied by ggplot2.
#' @param coord Coordinate system supplied by ggplot2.
#' @param show_linkage Logical scalar passed to [glycanGrob()].
#' @param orient Drawing orientation passed to [glycanGrob()].
#' @param fuc_orient Fuc-like triangle orientation passed to [glycanGrob()].
#' @param red_end Reducing-end annotation passed to [glycanGrob()].
#' @param edge_linewidth Linkage linewidth passed to [glycanGrob()].
#' @param node_linewidth Node-border linewidth passed to [glycanGrob()].
#' @param node_size Node-size multiplier passed to [glycanGrob()].
#' @param colours Custom monosaccharide colors passed to [glycanGrob()].
#' @param highlight Highlight indices passed to [glycanGrob()].
#' @param na.rm Logical scalar handled by ggplot2 before panel drawing.
#'
#' @returns A grid gTree containing one positioned `glycanGrob` per data row.
#' @noRd
.draw_glycan_panel <- function(
  data,
  panel_params,
  coord,
  show_linkage = TRUE,
  orient = "H",
  fuc_orient = "flex",
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colours = NULL,
  highlight = NULL,
  na.rm = FALSE
) {
  if (nrow(data) == 0) {
    return(grid::nullGrob())
  }

  coordinates <- coord$transform(data, panel_params)
  grobs <- purrr::pmap(
    list(
      structure = coordinates$structure,
      x = coordinates$x,
      y = coordinates$y,
      index = seq_len(nrow(coordinates))
    ),
    function(structure, x, y, index) {
      glycanGrob(
        structure,
        show_linkage = show_linkage,
        orient = orient,
        fuc_orient = fuc_orient,
        red_end = red_end,
        edge_linewidth = edge_linewidth,
        node_linewidth = node_linewidth,
        node_size = node_size,
        colors = colours,
        highlight = highlight
      ) |>
        .position_glycan_grob(x, y, index)
    }
  )

  grid::grobTree(
    children = rlang::exec(grid::gList, !!!grobs),
    name = "geom_glycan"
  )
}

#' Position a glycan grob inside a ggplot2 panel
#'
#' @param grob A `glycanGrob` object.
#' @param x Numeric transformed horizontal panel position.
#' @param y Numeric transformed vertical panel position.
#' @param index Integer row index used to create a unique grob name.
#'
#' @returns The input grob with a panel-positioning viewport and unique name.
#' @noRd
.position_glycan_grob <- function(grob, x, y, index) {
  grob$name <- paste0("geom_glycan.", index)
  grob$vp <- grid::viewport(
    x = grid::unit(x, "native"),
    y = grid::unit(y, "native"),
    just = c("center", "center")
  )
  grob
}

#' ggplot2 geom for glycan grobs
#'
#' @noRd
GeomGlycan <- ggplot2::ggproto(
  "GeomGlycan",
  ggplot2::Geom,
  required_aes = c("x", "y", "structure"),
  default_aes = ggplot2::aes(),
  extra_params = "na.rm",
  draw_key = ggplot2::draw_key_blank,
  draw_panel = .draw_glycan_panel
)
