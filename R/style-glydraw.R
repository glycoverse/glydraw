#' Create a glycan drawing style
#'
#' `style_glydraw()` collects the rendering options shared by glydraw's
#' standalone drawings, grobs, ggplot2 layers, guides, and glycan scales.
#' Supply the result with `style =` to reuse a visual specification.
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
#' @export
style_glydraw <- function(
  fuc_orient = c("flex", "up"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  font_family = "",
  colors = glydraw_colors()
) {
  fuc_orient <- rlang::arg_match(fuc_orient)
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
