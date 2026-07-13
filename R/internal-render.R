# Internal raster rendering helpers used by print methods and fixed-size display
# of glydraw cartoons.

#' Render a glycan cartoon to a fixed-size raster
#'
#' @param cartoon A `glydraw_cartoon` ggplot object with attribute
#'   `glydraw_size_px`, a named numeric vector containing `width` and `height`.
#' @param dpi Numeric dots per inch used by the cartoon size metadata.
#' @param bg Background color passed to `ggplot2::ggsave()`.
#' @param scale Positive numeric multiplier applied to the raster pixel
#'   dimensions and rendering resolution.
#'
#' @return A `nativeRaster` matrix read from a temporary PNG file. Matrix
#'   dimensions match `glydraw_size_px * scale` within device rounding.
#' @noRd
.render_cartoon_raster <- function(
  cartoon,
  dpi = 300,
  bg = "transparent",
  scale = 1
) {
  .validate_output_scale(scale)
  size <- attr(cartoon, "glydraw_size_px")
  checkmate::assert_numeric(size, names = "strict", any.missing = FALSE)
  checkmate::assert_names(names(size), identical.to = c("width", "height"))

  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)

  ggplot2::ggsave(
    filename = file,
    plot = .strip_cartoon_class(cartoon),
    width = size[["width"]] * scale,
    height = size[["height"]] * scale,
    units = "px",
    dpi = dpi * scale,
    bg = bg
  )
  png::readPNG(file, native = TRUE)
}

#' Draw a fixed-size cartoon raster on the current graphics device
#'
#' @param raster A `nativeRaster` matrix.
#' @param size_px A named numeric vector with `width` and `height` in pixels.
#' @param dpi Numeric dots per inch used to convert pixels to inches.
#' @param newpage Logical scalar; `TRUE` starts a new grid page before drawing.
#' @param vp `NULL`, a grid viewport object, or a viewport name string.
#'
#' @return The input `raster`, invisibly. The raster is drawn scaled down if the
#'   current viewport is smaller than the cartoon's physical size.
#' @noRd
.draw_cartoon_raster <- function(
  raster,
  size_px,
  dpi = 300,
  newpage = TRUE,
  vp = NULL
) {
  if (newpage) {
    grid::grid.newpage()
  }
  pushed_viewport <- FALSE
  if (!is.null(vp)) {
    if (is.character(vp)) {
      grid::seekViewport(vp)
    } else {
      grid::pushViewport(vp)
      pushed_viewport <- TRUE
    }
  }
  if (pushed_viewport) {
    on.exit(grid::upViewport(), add = TRUE)
  }

  viewport_size <- .current_viewport_size_inches()
  cartoon_size <- size_px / dpi
  scale <- min(
    1,
    viewport_size[["width"]] / cartoon_size[["width"]],
    viewport_size[["height"]] / cartoon_size[["height"]]
  )
  if (!is.finite(scale) || scale <= 0) {
    scale <- 1
  }

  grid::grid.raster(
    raster,
    width = grid::unit(cartoon_size[["width"]] * scale, "in"),
    height = grid::unit(cartoon_size[["height"]] * scale, "in"),
    interpolate = TRUE
  )
  invisible(raster)
}

#' Get the current grid viewport size in inches
#'
#' @return A named numeric vector with `width` and `height` in inches. Falls
#'   back to `grDevices::dev.size("in")` when grid viewport conversion is not
#'   finite or positive.
#' @noRd
.current_viewport_size_inches <- function() {
  size <- c(
    width = grid::convertWidth(grid::unit(1, "npc"), "in", valueOnly = TRUE),
    height = grid::convertHeight(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  )
  if (any(!is.finite(size)) || any(size <= 0)) {
    device_size <- grDevices::dev.size("in")
    size <- c(width = device_size[[1]], height = device_size[[2]])
  }
  size
}
