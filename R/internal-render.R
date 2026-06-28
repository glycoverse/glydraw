#' Render a glycan cartoon to a fixed-size raster image
#'
#' @param cartoon A glydraw cartoon.
#' @param dpi Dots per inch used by the cartoon size metadata.
#' @param bg Background color passed to [ggplot2::ggsave()].
#'
#' @return A native raster matrix.
#' @noRd
.render_cartoon_raster <- function(cartoon, dpi = 300, bg = "transparent") {
  size <- attr(cartoon, "glydraw_size_px")
  checkmate::assert_numeric(size, names = "strict", any.missing = FALSE)
  checkmate::assert_names(names(size), identical.to = c("width", "height"))

  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)

  ggplot2::ggsave(
    filename = file,
    plot = .strip_glydraw_class(cartoon),
    width = size[["width"]],
    height = size[["height"]],
    units = "px",
    dpi = dpi,
    bg = bg
  )
  png::readPNG(file, native = TRUE)
}

#' Draw a fixed-size cartoon raster to the current graphics device
#'
#' @param raster A native raster matrix.
#' @param size_px A named numeric vector with `width` and `height` in pixels.
#' @param dpi Dots per inch used by the cartoon size metadata.
#' @param newpage Draw on a new grid page.
#' @param vp A grid viewport object or viewport name.
#'
#' @return The raster object, invisibly.
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

  viewport_size <- .current_viewport_size_in()
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

#' Get the current grid viewport size
#'
#' @return A named numeric vector with `width` and `height` in inches.
#' @noRd
.current_viewport_size_in <- function() {
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
