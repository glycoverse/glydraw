#' Save fixed-size glycan cartoon image to local device.
#'
#' This function saves the glycan cartoon to a file,
#' with a suitable size.
#'
#' @param cartoon A ggplot2 object returned by [draw_cartoon()].
#' @param file File name of glycan cartoon.
#' @param ... Ignored.
#' @param dpi Deprecated and ignored. Use `scale` to change the output size.
#' @param scale Numeric output-size multiplier. The default `1` saves the
#'   cartoon at its natural fixed size; `2` saves the same cartoon with twice
#'   the pixel width and height.
#'
#' @details
#' # Why not `width` and `height`?
#'
#' The familiar [ggplot2::ggsave()] interface uses `width`, `height`, and `dpi`
#' because ordinary ggplot2 plots are drawn into a user-chosen device size.
#' glydraw cartoons are different: the natural width and height are calculated
#' from the glycan structure so residues, linkages, labels, and borders stay
#' comparable across different glycans. If users supplied arbitrary `width` and
#' `height`, glydraw would either distort that structure-derived layout or need
#' to guess how to reconcile one requested size with the other.
#'
#' `dpi` is also not the right control here because changing it alters how
#' point- and inch-based ggplot2 elements are rasterized relative to the fixed
#' cartoon canvas. glydraw therefore keeps an internal fixed design scale and
#' uses `scale` as a single multiplier for the final pixel dimensions. This
#' preserves the cartoon's aspect ratio and relative appearance while still
#' allowing larger or smaller output files.
#'
#' @returns Path of the saved cartoon file.
#'
#' @examples
#' cartoon <- draw_cartoon("Gal(b1-3)GalNAc(a1-")
#' save_cartoon(cartoon, tempfile(fileext = ".png"))
#' @export
save_cartoon <- function(cartoon, file, ..., dpi = 300, scale = 1) {
  checkmate::assert_class(cartoon, "glydraw_cartoon")
  if (!missing(dpi)) {
    .warn_ignored_dpi()
  }
  .validate_output_scale(scale)
  file_ext <- tools::file_ext(file)
  bg_color <- ifelse(
    file_ext == "jpeg" | file_ext == "jpg",
    "white",
    "transparent"
  )
  size <- attr(cartoon, "glydraw_size_px")
  panel_size <- attr(cartoon, "glydraw_panel_size_px")
  checkmate::assert_numeric(size, names = "strict", any.missing = FALSE)
  checkmate::assert_names(names(size), identical.to = c("width", "height"))
  checkmate::assert_numeric(panel_size, names = "strict", any.missing = FALSE)
  checkmate::assert_names(
    names(panel_size),
    identical.to = c("width", "height")
  )
  border_px <- (size - panel_size) / 2
  render_dpi <- .default_cartoon_dpi
  cartoon <- cartoon |>
    .add_plot_border(border_px[["width"]] / render_dpi * 72) |>
    .set_fixed_panel_size(panel_size, dpi = render_dpi) |>
    .strip_cartoon_class()

  ggplot2::ggsave(
    filename = file,
    plot = cartoon,
    width = size[["width"]] * scale,
    height = size[["height"]] * scale,
    units = "px",
    dpi = render_dpi * scale,
    bg = bg_color
  )
}
