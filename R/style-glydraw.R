#' Glycan drawing styles
#'
#' Style constructors collect the rendering options shared by glydraw's
#' standalone drawings, grobs, ggplot2 layers, guides, and glycan scales.
#' `style_glydraw()` provides glydraw's default appearance, while the other
#' constructors provide presets matching common glycan-drawing conventions.
#' Supply a returned style with `style =` to reuse its visual specification.
#'
#' @param fuc_orient Fuc-like triangle orientation: `"flex"` or `"up"`.
#' @param red_end Reducing-end annotation. Use `"~"` for a wave, any other
#'   string for custom text, or tag one amino-acid site as
#'   `"ABC<site>D</site>EFG"`. Ignored when `red_end_length` is `0`.
#' @param red_end_length Length of the reducing-end line in plot coordinate
#'   units. Set to `0` to omit the line and any `red_end` wave or custom text
#'   while retaining the axis-aligned core anomer annotation.
#' @param red_end_size Size of custom text passed to `red_end`. The `"~"` wave
#'   is not affected.
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
  red_end_length = 0.6,
  red_end_size = 6,
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
    colors = colors,
    red_end_length = red_end_length,
    red_end_size = red_end_size
  )
}

#' @describeIn style_glydraw Use a GlyGen-style preset.
#' @export
style_glygen <- function(
  fuc_orient = "flex",
  red_end = "~",
  red_end_length = 1,
  red_end_size = 6,
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
    colors = colors,
    red_end_length = red_end_length,
    red_end_size = red_end_size
  )
}

#' @describeIn style_glydraw Use a Symbol Nomenclature for Glycans preset.
#' @export
style_snfg <- function(
  fuc_orient = "up",
  red_end = "",
  red_end_length = 1,
  red_end_size = 6,
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
    colors = colors,
    red_end_length = red_end_length,
    red_end_size = red_end_size
  )
}

#' @describeIn style_glydraw Use a GlycoWorkbench-style preset.
#' @export
style_glycoworkbench <- function(
  fuc_orient = "flex",
  red_end = "~",
  red_end_length = 1,
  red_end_size = 6,
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
    colors = colors,
    red_end_length = red_end_length,
    red_end_size = red_end_size
  )
}

.make_glydraw_style <- function(
  fuc_orient,
  red_end,
  edge_linewidth,
  node_linewidth,
  node_size,
  font_family,
  colors,
  red_end_length,
  red_end_size
) {
  checkmate::assert_choice(fuc_orient, c("flex", "up"))
  if (is.null(red_end)) {
    cli::cli_abort(c(
      "{.arg red_end} in a glycan style cannot be {.code NULL}.",
      "i" = paste(
        "Set {.arg red_end_length} to 0 to omit the reducing-end line and",
        "{.arg red_end} decoration while retaining the anomer annotation."
      )
    ))
  }
  checkmate::assert_string(red_end, na.ok = FALSE)
  .parse_reducing_end_aa_sequence(red_end)
  checkmate::assert_number(edge_linewidth, lower = 0)
  checkmate::assert_number(node_linewidth, lower = 0)
  .validate_node_size(node_size)
  checkmate::assert_string(font_family, na.ok = FALSE)
  checkmate::assert_number(red_end_length, lower = 0)
  checkmate::assert_number(red_end_size, lower = 0)
  colors <- .validate_colors(colors)

  structure(
    list(
      fuc_orient = fuc_orient,
      red_end = red_end,
      edge_linewidth = edge_linewidth,
      node_linewidth = node_linewidth,
      node_size = node_size,
      font_family = font_family,
      colors = colors,
      red_end_length = red_end_length,
      red_end_size = red_end_size
    ),
    class = "glydraw_style"
  )
}

.resolve_red_end <- function(red_end, style) {
  if (is.null(red_end)) {
    return(style$red_end)
  }
  checkmate::assert_string(red_end, na.ok = FALSE)
  red_end
}
