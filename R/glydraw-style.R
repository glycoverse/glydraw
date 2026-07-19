#' Create a glycan drawing style
#'
#' `glydraw_style()` collects the rendering options shared by glydraw's
#' standalone drawings, grobs, ggplot2 layers, guides, and glycan scales.
#' Supply the result with `style =` to reuse a visual specification. Explicit
#' rendering arguments supplied to a drawing function override the style.
#'
#' @param show_linkage Whether to show glycosidic linkage annotations.
#' @param orient Glycan drawing orientation: `"H"` for horizontal or `"V"` for
#'   vertical.
#' @param fuc_orient Fuc-like triangle orientation: `"flex"` or `"up"`.
#' @param red_end Reducing-end annotation. Use `"~"` for a wave.
#' @param edge_linewidth Linewidth of glycosidic linkages.
#' @param node_linewidth Linewidth of node borders.
#' @param node_size Multiplier for the default node size.
#' @param colors Optional named character vector of monosaccharide fill-color
#'   overrides.
#'
#' @returns A `glydraw_style` object.
#'
#' @examples
#' vertical_style <- glydraw_style(orient = "V", show_linkage = FALSE)
#' draw_cartoon("Gal(b1-3)GalNAc(a1-", style = vertical_style)
#' @export
glydraw_style <- function(
  show_linkage = TRUE,
  orient = c("H", "V"),
  fuc_orient = c("flex", "up"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL
) {
  checkmate::assert_flag(show_linkage)
  orient <- rlang::arg_match(orient)
  fuc_orient <- rlang::arg_match(fuc_orient)
  checkmate::assert_string(red_end, na.ok = FALSE)
  checkmate::assert_number(edge_linewidth, lower = 0)
  checkmate::assert_number(node_linewidth, lower = 0)
  .validate_node_size(node_size)
  colors <- .validate_custom_colors(colors)

  structure(
    list(
      show_linkage = show_linkage,
      orient = orient,
      fuc_orient = fuc_orient,
      red_end = red_end,
      edge_linewidth = edge_linewidth,
      node_linewidth = node_linewidth,
      node_size = node_size,
      colors = colors
    ),
    class = "glydraw_style"
  )
}

#' Resolve rendering arguments against an optional drawing style
#'
#' @param style A `glydraw_style` object or `NULL`.
#' @param .supplied Named logical vector indicating explicitly supplied values.
#' @param ... Rendering values.
#'
#' @returns A validated `glydraw_style` object.
#' @noRd
.resolve_glydraw_style <- function(style = NULL, ..., .supplied) {
  values <- rlang::list2(...)
  if (!is.null(style)) {
    if (!inherits(style, "glydraw_style")) {
      cli::cli_abort("{.arg style} must be a {.cls glydraw_style} object.")
    }
    style <- do.call(glydraw_style, unclass(style))
  } else {
    style <- glydraw_style()
  }

  values <- values[.supplied]
  do.call(glydraw_style, utils::modifyList(unclass(style), values))
}
