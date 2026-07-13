test_that("geom_glycan maps structures to x and y positions", {
  data <- data.frame(
    x = c(1, 3),
    y = c(1, 2),
    structure = c(
      "Gal(b1-3)GalNAc(a1-",
      "Gal(b1-4)GlcNAc(b1-"
    )
  )

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      structure = .data$structure
    )
  ) +
    geom_glycan()
  built <- ggplot2::ggplot_build(plot)

  expect_equal(built$data[[1]]$structure, data$structure)
  expect_equal(built$data[[1]]$size, rep(1, nrow(data)))
  expect_equal(built$data[[1]]$hjust, rep(0.5, nrow(data)))
  expect_equal(built$data[[1]]$vjust, rep(0.5, nrow(data)))
  expect_setequal(GeomGlycan$required_aes, c("x", "y", "structure"))
})

test_that("geom_glycan draws one configured glycan grob per row", {
  rplots <- "Rplots.pdf"
  unlink(rplots)
  on.exit(unlink(rplots), add = TRUE)
  data <- data.frame(
    x = c(1, 3),
    y = c(1, 2),
    structure = c(
      "Gal(b1-3)GalNAc(a1-",
      "Gal(b1-4)GlcNAc(b1-"
    )
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      structure = .data$structure
    )
  ) +
    geom_glycan(
      show_linkage = FALSE,
      edge_linewidth = 1.1,
      node_linewidth = 0.3,
      colors = c(Gal = "#123456")
    )

  layer <- ggplot2::layer_grob(plot)[[1]]

  expect_s3_class(layer, "gTree")
  expect_length(layer$children, 2)
  purrr::walk(layer$children, function(grob) {
    expect_s3_class(grob, "glycanGrob")
    expect_false(grob$show_linkage)
    expect_equal(grob$edge_linewidth, 1.1)
    expect_equal(grob$node_linewidth, 0.3)
    expect_contains(grob$filled_color, "#123456")
    expect_equal(grob$glydraw_scale, 1)
    expect_s3_class(grob$vp, "viewport")
  })
  expect_no_error(ggplot2::ggplotGrob(plot))
})

test_that("geom_glycan requires structure, x, and y aesthetics", {
  data <- data.frame(x = 1, y = 1)
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$x, y = .data$y)
  ) +
    geom_glycan()

  expect_error(
    ggplot2::ggplot_build(plot),
    "missing aesthetics: structure"
  )
})

test_that("geom_glycan uses size as a whole-cartoon scale multiplier", {
  rplots <- "Rplots.pdf"
  unlink(rplots)
  on.exit(unlink(rplots), add = TRUE)
  data <- data.frame(
    x = c(1, 3),
    y = c(1, 1),
    size = c(0.5, 1.5),
    structure = rep("Gal(b1-3)GalNAc(a1-", 2)
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      structure = .data$structure,
      size = .data$size
    )
  ) +
    geom_glycan() +
    ggplot2::scale_size_identity()

  layer <- ggplot2::layer_grob(plot)[[1]]
  scales <- purrr::map_dbl(layer$children, "glydraw_scale")
  content <- purrr::map(layer$children, grid::makeContent)
  rasters <- purrr::map(content, ~ .x$children[[1]])
  widths <- purrr::map_dbl(rasters, ~ as.numeric(.x$width))
  heights <- purrr::map_dbl(rasters, ~ as.numeric(.x$height))
  corner_pixels <- purrr::map_int(
    rasters,
    ~ unclass(.x$raster)[[1]]
  )

  expect_equal(unname(scales), data$size)
  purrr::walk(rasters, expect_s3_class, "rastergrob")
  expect_equal(unname(corner_pixels), rep(0L, nrow(data)))
  expect_equal(widths[[2]] / widths[[1]], 3)
  expect_equal(heights[[2]] / heights[[1]], 3)
})

test_that("geom_glycan rejects non-positive size multipliers", {
  data <- data.frame(
    x = 1,
    y = 1,
    structure = "Gal(b1-3)GalNAc(a1-"
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      structure = .data$structure
    )
  ) +
    geom_glycan(size = 0)

  expect_error(
    ggplot2::layer_grob(plot),
    "must be larger than 0"
  )
})

test_that("geom_glycan maps hjust and vjust to whole-cartoon alignment", {
  rplots <- "Rplots.pdf"
  unlink(rplots)
  on.exit(unlink(rplots), add = TRUE)
  data <- data.frame(
    x = c(1, 3),
    y = c(1, 1),
    hjust = c(0, 1),
    vjust = c(1, 0),
    structure = rep("Gal(b1-3)GalNAc(a1-", 2)
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      structure = .data$structure,
      hjust = .data$hjust,
      vjust = .data$vjust
    )
  ) +
    geom_glycan()

  layer <- ggplot2::layer_grob(plot)[[1]]
  hjust <- purrr::map_dbl(layer$children, "glydraw_hjust")
  vjust <- purrr::map_dbl(layer$children, "glydraw_vjust")
  content <- purrr::map(layer$children, grid::makeContent)
  child_viewports <- purrr::map(content, ~ .x$children[[1]]$vp)

  expect_equal(unname(hjust), data$hjust)
  expect_equal(unname(vjust), data$vjust)
  purrr::walk(child_viewports, expect_s3_class, "viewport")
})

test_that("geom_glycan omits standalone cartoon borders and backgrounds", {
  data <- data.frame(
    x = 1,
    y = 1,
    structure = "Gal(b1-3)GalNAc(a1-"
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      structure = .data$structure
    )
  ) +
    geom_glycan()

  grob <- ggplot2::layer_grob(plot)[[1]]$children[[1]]
  geom_cartoon <- .glycan_grob_to_plot(grob)
  geom_size <- attr(geom_cartoon, "glydraw_size_px")
  geom_panel_size <- attr(geom_cartoon, "glydraw_panel_size_px")
  standalone_cartoon <- draw_cartoon(data$structure)
  standalone_size <- attr(standalone_cartoon, "glydraw_size_px")
  standalone_panel_size <- attr(
    standalone_cartoon,
    "glydraw_panel_size_px"
  )

  expect_equal(grob$glydraw_border_px, 0)
  expect_false(grob$glydraw_background)
  expect_equal(geom_size, geom_panel_size)
  expect_s3_class(
    ggplot2::calc_element("plot.background", geom_cartoon$theme),
    "element_blank"
  )
  expect_equal(
    (standalone_size - standalone_panel_size) / 2,
    c(width = 50, height = 50)
  )
})
