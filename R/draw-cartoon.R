#' Draw a Symbol Nomenclature For Glycan (SNFG)
#'
#' @param structure A [glyrepr::glycan_structure()] scalar,
#'   or a string of any glycan structure text nomenclatures supported by [glyparse::auto_parse()].
#' @param ... Ignored.
#' @param show_linkage Show glycosidic linkage annotations or not. Default is
#'   TRUE. Substituent annotations are always shown.
#' @param orient Direction in which the glycan extends from its reducing end:
#'   one of `"left"`, `"right"`, `"up"`, or `"down"`. Defaults to `"left"`.
#' @param style A [style_glydraw()] object that controls the cartoon's visual
#'   appearance.
#' @param red_end Reducing-end annotation. `NULL`, the default, uses `red_end`
#'   from `style`. A non-`NULL` value overrides `style$red_end`. Ignored when
#'   `style$red_end_length` is `0`. To annotate an amino-acid sequence, tag its
#'   single glycosite as, for example, `"ABC<site>D</site>EFG"`.
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
#' draw_cartoon(
#'   "Gal(b1-3)GalNAc(a1-",
#'   style = style_glydraw(font_family = "serif")
#' )
#' @export
draw_cartoon <- function(
  structure,
  ...,
  show_linkage = TRUE,
  orient = c("left", "right", "up", "down"),
  highlight = NULL,
  style = style_glydraw(),
  red_end = NULL
) {
  glycanGrob(
    structure,
    ...,
    show_linkage = show_linkage,
    orient = orient,
    highlight = highlight,
    style = style,
    red_end = red_end
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
