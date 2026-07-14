# ggplot2 layer helpers for positioning glycan grobs inside plot panels.

#' Draw glycans at ggplot2 positions
#'
#' `geom_glycan()` draws one glycan cartoon for each data row. Each cartoon is
#' anchored at its mapped `x` and `y` position and retains the structure-derived
#' dimensions and appearance used by [draw_cartoon()]. The optional `size`
#' aesthetic scales the complete cartoon uniformly, including nodes, lines,
#' text, and spacing, without changing their relative appearance. Like points
#' and text, the cartoons do not expand the position scales beyond their anchor
#' coordinates. Use scale expansion or explicit coordinate limits when the
#' cartoons need more room around the panel edges. Unlike standalone cartoons
#' returned by [draw_cartoon()], cartoons in this layer have no output border or
#' background.
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
#' @param angle Rotation in degrees. Like [ggplot2::geom_text()], this can be
#'   supplied as an aesthetic or a fixed layer value. It rotates each completed
#'   cartoon around its mapped position independently of `orient`. Defaults to
#'   `0`.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather than
#'   combining with them.
#' @inheritParams draw_cartoon
#'
#' @section Aesthetics:
#' `geom_glycan()` understands the following aesthetics. `x`, `y`, and
#' `structure` are required; the remaining aesthetics are optional:
#'
#' - `x`
#' - `y`
#' - `structure`, containing glycan structure strings or
#'   [glyrepr::glycan_structure()] values
#' - `size`, an optional whole-cartoon scale multiplier that defaults to `1`.
#'   Mapped values are transformed by ggplot2's size scale; use
#'   [ggplot2::scale_size_identity()] when the mapped values are literal
#'   multipliers. This is distinct from `node_size`, which changes residue size
#'   within the cartoon.
#' - `hjust`, an optional horizontal justification that defaults to `0.5`.
#'   `0` aligns the cartoon content's left bound with `x`, and `1` aligns its
#'   right bound.
#' - `vjust`, an optional vertical justification that defaults to `0.5`.
#'   `0` aligns the cartoon content's bottom bound with `y`, including the end
#'   of a reducing-end annotation line, and `1` aligns its top bound.
#' - `angle`, an optional rotation in degrees that defaults to `0`. Rotation is
#'   applied after the cartoon is drawn, independently of `orient`.
#'
#' @returns A ggplot2 layer that can be added to a [ggplot2::ggplot()] object.
#'
#' @examples
#' glycans <- data.frame(
#'   x = c(1, 3),
#'   y = c(1, 2),
#'   size = c(0.7, 1.1),
#'   structure = c(
#'     "Gal(b1-3)GalNAc(a1-",
#'     "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
#'   )
#' )
#'
#' ggplot2::ggplot(
#'   glycans,
#'   ggplot2::aes(
#'     x = .data$x,
#'     y = .data$y,
#'     structure = .data$structure,
#'     size = .data$size
#'   )
#' ) +
#'   geom_glycan() +
#'   ggplot2::scale_size_identity() +
#'   ggplot2::coord_cartesian(
#'     xlim = c(0, 4),
#'     ylim = c(0, 3),
#'     expand = FALSE
#'   )
#'
#' # Bottom-align a row of vertical glycans with different heights.
#' ggplot2::ggplot(
#'   glycans,
#'   ggplot2::aes(x = .data$x, y = 1, structure = .data$structure)
#' ) +
#'   geom_glycan(orient = "V", vjust = 0)
#' @export
geom_glycan <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  angle = 0,
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

  params <- rlang::list2(
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
  if (!missing(angle)) {
    params$angle <- angle
  }

  ggplot2::layer(
    mapping = mapping,
    data = data,
    geom = GeomGlycan,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' Draw a panel of positioned glycan grobs
#'
#' @param data Data frame prepared by ggplot2 with `x`, `y`, `structure`,
#'   `size`, `hjust`, `vjust`, and `angle` columns.
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
  .validate_glycan_sizes(coordinates$size)
  .validate_glycan_justification(coordinates$hjust, "hjust")
  .validate_glycan_justification(coordinates$vjust, "vjust")
  .validate_glycan_angles(coordinates$angle)
  grobs <- purrr::pmap(
    list(
      structure = coordinates$structure,
      x = coordinates$x,
      y = coordinates$y,
      size = coordinates$size,
      hjust = coordinates$hjust,
      vjust = coordinates$vjust,
      angle = coordinates$angle,
      index = seq_len(nrow(coordinates))
    ),
    function(structure, x, y, size, hjust, vjust, angle, index) {
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
        .position_glycan_grob(x, y, size, hjust, vjust, angle, index)
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
#' @param size Positive numeric whole-cartoon scale multiplier.
#' @param hjust Numeric horizontal justification.
#' @param vjust Numeric vertical justification.
#' @param angle Numeric rotation in degrees.
#' @param index Integer row index used to create a unique grob name.
#'
#' @returns The input grob with a panel-positioning viewport and unique name.
#' @noRd
.position_glycan_grob <- function(
  grob,
  x,
  y,
  size,
  hjust,
  vjust,
  angle,
  index
) {
  grob$name <- paste0("geom_glycan.", index)
  grob$glydraw_scale <- size
  grob$glydraw_hjust <- hjust
  grob$glydraw_vjust <- vjust
  grob$glydraw_angle <- angle
  grob$glydraw_border_px <- 0
  grob$glydraw_background <- FALSE
  grob$vp <- grid::viewport(
    x = grid::unit(x, "native"),
    y = grid::unit(y, "native"),
    just = c("center", "center"),
    angle = angle
  )
  grob
}

#' Validate mapped glycan size multipliers
#'
#' @param size Numeric vector of transformed `size` aesthetic values.
#'
#' @returns `size`, invisibly. Throws an error when values are not finite,
#'   missing, or strictly positive.
#' @noRd
.validate_glycan_sizes <- function(size) {
  checkmate::assert_numeric(size, any.missing = FALSE, finite = TRUE)
  if (any(size <= 0)) {
    cli::cli_abort(
      "The {.field size} aesthetic values must be larger than 0."
    )
  }
  invisible(size)
}

#' Validate mapped glycan justification values
#'
#' @param justification Numeric vector of transformed justification values.
#' @param aesthetic String identifying the aesthetic for error messages.
#'
#' @returns `justification`, invisibly. Throws an error when values are not
#'   finite numeric values.
#' @noRd
.validate_glycan_justification <- function(justification, aesthetic) {
  checkmate::assert_numeric(
    justification,
    any.missing = FALSE,
    finite = TRUE,
    .var.name = aesthetic
  )
  invisible(justification)
}

#' Validate mapped glycan rotation angles
#'
#' @param angle Numeric vector of rotation angles in degrees.
#'
#' @returns `angle`, invisibly. Throws an error when values are missing or not
#'   finite numeric values.
#' @noRd
.validate_glycan_angles <- function(angle) {
  checkmate::assert_numeric(angle, any.missing = FALSE, finite = TRUE)
  invisible(angle)
}

#' ggplot2 geom for glycan grobs
#'
#' @noRd
GeomGlycan <- ggplot2::ggproto(
  "GeomGlycan",
  ggplot2::Geom,
  required_aes = c("x", "y", "structure"),
  non_missing_aes = c("size", "hjust", "vjust", "angle"),
  default_aes = ggplot2::aes(size = 1, hjust = 0.5, vjust = 0.5, angle = 0),
  extra_params = "na.rm",
  draw_key = ggplot2::draw_key_blank,
  draw_panel = .draw_glycan_panel
)
