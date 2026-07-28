#' Draw a sketch-style glycan cartoon
#'
#' `draw_cartoon_sketch()` uses [ggsketch::ggsketch-package] geoms to give a
#' glycan cartoon hand-drawn strokes and patterned residue fills. Its glycan
#' layout, annotations, orientation, and sizing are otherwise identical to
#' [draw_cartoon()].
#'
#' Sketch cartoons always use a handwriting font, ignoring `font_family` in
#' `style`. They prefer an installed sketch-style font that contains Greek
#' alpha, Greek beta, and decimal digits so all linkage labels use one font.
#'
#' @inheritParams draw_cartoon
#' @param show_linkage Show glycosidic linkage annotations or not. Defaults to
#'   `TRUE`. Substituent annotations are always shown.
#' @param roughness Non-negative roughness of the hand-drawn strokes. Zero
#'   produces straight strokes. Hex circle outlines are automatically softened
#'   to keep their curved borders smooth.
#' @param bowing Non-negative multiplier controlling how much strokes bow.
#' @param n_passes Number of times each sketch stroke is drawn.
#' @param seed An optional integer seed for reproducible sketch strokes. When
#'   `NULL`, `ggsketch` uses `getOption("ggsketch.seed", 1L)`.
#' @param fill_style Residue fill pattern. Defaults to `"pencil_shade"`. See
#'   [ggsketch::geom_sketch_polygon()] for the available styles.
#' @param hachure_angle Angle of patterned fill lines in degrees.
#' @param hachure_gap Gap between patterned fill lines as a proportion of the
#'   node diameter. Defaults to `0.03`.
#' @param fill_weight Stroke weight of patterned fill lines.
#' @param medium Optional drawing medium for linkage and reducing-end strokes.
#'   See [ggsketch::sketch_media()] for the available media.
#'
#' @returns A `glydraw_cartoon` ggplot2 object.
#'
#' @examples
#' if (requireNamespace("ggsketch", quietly = TRUE)) {
#'   draw_cartoon_sketch("Gal(b1-3)GalNAc(a1-", seed = 1)
#' }
#' @export
draw_cartoon_sketch <- function(
  structure,
  ...,
  show_linkage = TRUE,
  orient = c("left", "right", "up", "down"),
  highlight = NULL,
  style = style_glydraw(),
  roughness = 1,
  bowing = 1,
  n_passes = 2L,
  seed = NULL,
  fill_style = "pencil_shade",
  hachure_angle = 45,
  hachure_gap = 0.03,
  fill_weight = 0.5,
  medium = NULL,
  red_end = NULL
) {
  rlang::check_installed(
    "ggsketch",
    version = "2.0.0",
    reason = "to draw sketch-style glycan cartoons"
  )
  grob <- glycanGrob(
    structure,
    ...,
    show_linkage = show_linkage,
    orient = orient,
    highlight = highlight,
    style = style,
    red_end = red_end
  )
  sketch <- list(
    roughness = roughness,
    bowing = bowing,
    n_passes = n_passes,
    seed = seed,
    fill_style = fill_style,
    hachure_angle = hachure_angle,
    hachure_gap = hachure_gap,
    fill_weight = fill_weight,
    medium = medium
  )
  .glycan_grob_to_sketch_plot(grob, sketch)
}
