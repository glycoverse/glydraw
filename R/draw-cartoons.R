#' Draw a grid of SNFG glycan cartoons
#'
#' @param structure A [glyrepr::glycan_structure()] vector or a character
#'   vector of glycan structure strings supported by [glyparse::auto_parse()].
#' @param nrow,ncol Positive integer numbers of rows and columns. When both
#'   are `NULL`, the number of columns is `ceiling(sqrt(length(structure)))`
#'   and the number of rows is calculated to fit all structures. When only
#'   one is supplied, the other is calculated automatically.
#' @param max_n Maximum permitted input length, a positive integer. Defaults
#'   to 36. Longer inputs produce an error rather than being truncated.
#' @inheritParams draw_cartoon
#'
#' @details
#' Cartoons fill the grid from left to right, then top to bottom, preserving
#' input order and duplicates. Unused cells are blank. All cells have the same
#' size, large enough for the largest cartoon, and residue sizes are consistent
#' across cells. Drawing arguments are applied to every glycan; in particular,
#' `highlight` uses the same node indices for each structure.
#'
#' @returns A ggplot2 object of class `glydraw_cartoon`, which can be saved
#'   with [save_cartoon()].
#' @examples
#' draw_cartoons(c("Gal(b1-3)GalNAc(a1-", "Man(a1-3)Man(b1-"))
#' @export
draw_cartoons <- function(
  structure,
  ...,
  show_linkage = TRUE,
  orient = c("left", "right", "up", "down"),
  highlight = NULL,
  style = style_glydraw(),
  red_end = NULL,
  nrow = NULL,
  ncol = NULL,
  max_n = 36
) {
  checkmate::assert_count(max_n, positive = TRUE)
  checkmate::assert_count(nrow, positive = TRUE, null.ok = TRUE)
  checkmate::assert_count(ncol, positive = TRUE, null.ok = TRUE)
  if (!is.character(structure) && !glyrepr::is_glycan_structure(structure)) {
    cli::cli_abort(
      "{.arg structure} must be a character or glycan_structure vector."
    )
  }
  n <- length(structure)
  if (n == 0L || n > max_n) {
    cli::cli_abort(
      "{.arg structure} must contain between 1 and {.val {max_n}} glycans; got {.val {n}}."
    )
  }
  if (is.null(nrow) && is.null(ncol)) {
    ncol <- ceiling(sqrt(n))
  }
  if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
  }
  if (is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  }
  if (nrow * ncol < n) {
    cli::cli_abort(
      "{.arg nrow} * {.arg ncol} must provide space for all {.val {n}} glycans."
    )
  }
  grobs <- lapply(seq_len(n), function(i) {
    glycanGrob(
      structure[[i]],
      ...,
      show_linkage = show_linkage,
      orient = orient,
      highlight = highlight,
      style = style,
      red_end = red_end
    )
  })
  sizes <- vapply(
    grobs,
    function(grob) {
      .cartoon_grid_layout(grob)$size_px
    },
    c(width = 0, height = 0)
  )
  cell_size <- apply(sizes, 1, max)
  size <- cell_size * c(ncol, nrow)
  plot <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(
      xlim = c(0, ncol),
      ylim = c(0, nrow),
      expand = FALSE
    ) +
    ggplot2::theme_void()
  for (i in seq_len(n)) {
    col <- (i - 1) %% ncol
    row <- nrow - 1 - (i - 1) %/% ncol
    plot <- plot +
      ggplot2::annotation_custom(
        grobs[[i]],
        xmin = col,
        xmax = col + 1,
        ymin = row,
        ymax = row + 1
      )
  }
  plot <- .set_fixed_panel_size(plot, size, .default_cartoon_dpi)
  attr(plot, "glydraw_panel_size_px") <- size
  attr(plot, "glydraw_size_px") <- size
  attr(plot, "glydraw_font_family") <- style$font_family
  structure(plot, class = c("glydraw_cartoon", class(plot)))
}
