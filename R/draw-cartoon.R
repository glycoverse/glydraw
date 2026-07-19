#' Draw a Symbol Nomenclature For Glycan (SNFG)
#'
#' @param structure A [glyrepr::glycan_structure()] scalar,
#'   or a string of any glycan structure text nomenclatures supported by [glyparse::auto_parse()].
#' @param ... Ignored.
#' @param show_linkage Show glycosidic linkage annotations or not. Default is
#'   TRUE. Substituent annotations are always shown.
#' @param orient The orientation of glycan structure. "H" for horizontal, "V" for vertical.
#'   Default is "H"
#' @param fuc_orient Fuc-like triangle orientation. `"flex"` points
#'   non-reducing Fuc-like residues toward their rendered linkage direction,
#'   while `"up"` draws all Fuc-like triangles pointing upward. Reducing-end
#'   Fuc-like residues always point upward. Defaults to `"flex"`.
#' @param red_end Reducing-end annotation. The default `""` keeps the current
#'   reducing-end line. Use `"~"` to add a wavy reducing end, or any other
#'   string to draw that string at the reducing end.
#' @param edge_linewidth Numeric scalar controlling the linewidth of linkage
#'   lines. Defaults to the current value, `0.8`.
#' @param node_linewidth Numeric scalar controlling the linewidth of node
#'   borders. Defaults to the current value, `0.8`.
#' @param node_size Numeric scalar used as a multiplier for the default node
#'   size. Defaults to `1`, which keeps the current size. Linkage annotations
#'   are moved farther from larger nodes, and are hidden with a warning when
#'   `node_size` is too large to leave enough annotation space. Values larger
#'   than `2` are rejected because residues overlap.
#' @param colors Optional named character vector of custom monosaccharide fill
#'   colors. Names must be supported monosaccharide names, such as `"Gal"` or
#'   `"GlcNAc"`. User-provided colors overwrite the default SNFG colors, while
#'   unprovided monosaccharides keep their default colors. Defaults to `NULL`.
#' @param highlight An integer vector specifying the node indices to highlight.
#'   This argument is applicable only when `structure` is a [glyrepr::glycan_structure()].
#'   Note that for a [glyrepr::glycan_structure()], the node indices correspond exactly
#'   to the monosaccharides in its printed IUPAC nomenclature.
#'   For example, given `glyrepr::as_glycan_structure("Gal(b1-3)[GlcNAc(b1-6)]GalNAc(a1-")`,
#'   setting `highlight = c(1, 3)` will highlight the "Gal" and "GalNAc" nodes.
#'
#' @returns a ggplot2 object
#' @examples
#' draw_cartoon("Gal(b1-3)GalNAc(a1-")
#' @export
draw_cartoon <- function(
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
  glycanGrob(
    structure,
    ...,
    show_linkage = show_linkage,
    orient = orient,
    fuc_orient = fuc_orient,
    red_end = red_end,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size,
    colors = colors,
    highlight = highlight
  ) |>
    .glycan_grob_to_plot()
}

#' Print glycan cartoon
#'
#' @param x A ggplot2 object returned by [draw_cartoon()].
#' @param ... Ignored.
#' @param newpage Draw the plot on a new page.
#' @param vp A grid viewport object or viewport name.
#'
#' @return The original glycan cartoon, invisibly.
#' @export
print.glydraw_cartoon <- function(
  x,
  ...,
  newpage = is.null(vp),
  vp = NULL
) {
  raster <- .render_cartoon_raster(x)
  .draw_cartoon_raster(
    raster,
    size_px = attr(x, "glydraw_size_px"),
    newpage = newpage,
    vp = vp
  )
  invisible(x)
}
