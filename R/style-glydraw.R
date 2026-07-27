#' Glycan drawing styles
#'
#' Style constructors collect the rendering options shared by glydraw's
#' standalone drawings, grobs, ggplot2 layers, guides, and glycan scales.
#' `style_glydraw()` provides glydraw's default appearance, while the other
#' constructors provide presets matching common glycan-drawing conventions.
#' Supply a returned style with `style =` to reuse its visual specification.
#'
#' @param fuc_orient Fuc-like triangle orientation: `"flex"` or `"up"`.
#' @param red_end Reducing-end annotation. Use `"~"` for a wave or `NULL` to
#'   omit the reducing-end line and anomer annotation.
#' @param edge_linewidth Linewidth of glycosidic linkages.
#' @param node_linewidth Linewidth of node borders.
#' @param node_size Multiplier for the default node size.
#' @param font_family A length-one character string naming the font family used
#'   for linkage, substituent, and reducing-end text annotations. Portable
#'   choices are `"sans"`, `"serif"`, and `"mono"`. Other family names, such as
#'   installed system fonts, are graphics-device dependent. The default `""`
#'   uses the graphics device's default font.
#' @param colors A named character vector of SNFG colors in the format returned
#'   by [glydraw_colors()]. Names must be complete and match that palette.
#'
#' @returns A `glydraw_style` object.
#'
#' @examples
#' serif_style <- style_glydraw(font_family = "serif")
#' draw_cartoon("Gal(b1-3)GalNAc(a1-", style = serif_style)
#'
#' draw_cartoon("Gal(b1-3)GalNAc(a1-", style = style_glygen())
#' draw_cartoon("Gal(b1-3)GalNAc(a1-", style = style_snfg())
#' draw_cartoon("Gal(b1-3)GalNAc(a1-", style = style_glycoworkbench())
#' @describeIn style_glydraw Use glydraw's default style.
#' @export
style_glydraw <- function(
  fuc_orient = "flex",
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  font_family = "",
  colors = glydraw_colors()
) {
  .make_glydraw_style(
    fuc_orient = fuc_orient,
    red_end = red_end,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size,
    font_family = font_family,
    colors = colors
  )
}

#' @describeIn style_glydraw Use a GlyGen-style preset.
#' @export
style_glygen <- function(
  fuc_orient = "flex",
  red_end = "~",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  font_family = "arial",
  colors = glydraw_colors()
) {
  .make_glydraw_style(
    fuc_orient = fuc_orient,
    red_end = red_end,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size,
    font_family = font_family,
    colors = colors
  )
}

#' @describeIn style_glydraw Use a Symbol Nomenclature for Glycans preset.
#' @export
style_snfg <- function(
  fuc_orient = "up",
  red_end = "",
  edge_linewidth = 1.5,
  node_linewidth = 0.8,
  node_size = 1.15,
  font_family = "arial",
  colors = glydraw_colors()
) {
  .make_glydraw_style(
    fuc_orient = fuc_orient,
    red_end = red_end,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size,
    font_family = font_family,
    colors = colors
  )
}

#' @describeIn style_glydraw Use a GlycoWorkbench-style preset.
#' @export
style_glycoworkbench <- function(
  fuc_orient = "flex",
  red_end = "~",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  font_family = "arial",
  colors = c(
    glyWhite = "#FFFFFF",
    glyBlue = "#0000F0",
    glyGreen = "#5AC54B",
    glyYellow = "#FFFF54",
    glyOrange = "#F7EAD7",
    glyPink = "#FFFFFF",
    glyPurple = "#B726C1",
    glyLightBlue = "#EDFEFF",
    glyBrown = "#8F663B",
    glyRed = "#E53222"
  )
) {
  .make_glydraw_style(
    fuc_orient = fuc_orient,
    red_end = red_end,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size,
    font_family = font_family,
    colors = colors
  )
}

.make_glydraw_style <- function(
  fuc_orient,
  red_end,
  edge_linewidth,
  node_linewidth,
  node_size,
  font_family,
  colors
) {
  checkmate::assert_choice(fuc_orient, c("flex", "up"))
  checkmate::assert_string(red_end, na.ok = FALSE, null.ok = TRUE)
  checkmate::assert_number(edge_linewidth, lower = 0)
  checkmate::assert_number(node_linewidth, lower = 0)
  .validate_node_size(node_size)
  checkmate::assert_string(font_family, na.ok = FALSE)
  colors <- .validate_colors(colors)

  structure(
    list(
      fuc_orient = fuc_orient,
      red_end = red_end,
      edge_linewidth = edge_linewidth,
      node_linewidth = node_linewidth,
      node_size = node_size,
      font_family = font_family,
      colors = colors
    ),
    class = "glydraw_style"
  )
}
