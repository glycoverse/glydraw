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

test_that("geom_glycan preserves mapped glycan structure vectors", {
  structures <- glyrepr::as_glycan_structure(c(
    "GalNAc(a1-",
    "Gal(b1-3)GalNAc(a1-"
  ))
  data <- tibble::tibble(
    x = c(1, 2),
    y = c(1, 2),
    structure = structures
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

  expect_s3_class(built$data[[1]]$structure, "glyrepr_structure")
  expect_equal(
    as.character(built$data[[1]]$structure),
    as.character(structures)
  )
  expect_no_error(ggplot2::layer_grob(plot))
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
  colors <- glydraw_colors()
  colors["glyYellow"] <- "#123456"
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
      style = style_glydraw(
        edge_linewidth = 1.1,
        node_linewidth = 0.3,
        colors = colors
      )
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

test_that("geom_glycan prepares repeated structures once per panel", {
  structures <- c(
    "Gal(b1-3)GalNAc(a1-",
    "Gal(b1-3)GalNAc(a1-",
    "Gal(b1-4)GlcNAc(b1-",
    "Gal(b1-3)GalNAc(a1-"
  )
  data <- data.frame(
    x = seq_along(structures),
    y = 1,
    structure = structures
  )
  call_count <- 0L
  original_glycan_grob <- glycanGrob
  testthat::local_mocked_bindings(
    glycanGrob = function(...) {
      call_count <<- call_count + 1L
      original_glycan_grob(...)
    }
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
  layer <- ggplot2::layer_grob(plot)

  expect_equal(call_count, 2L)
  expect_length(layer[[1]]$children, length(structures))
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
  cartoons <- purrr::map(content, ~ .x$children[[1]])
  widths <- purrr::map_dbl(cartoons, ~ as.numeric(.x$width))
  heights <- purrr::map_dbl(cartoons, ~ as.numeric(.x$height))
  expect_equal(unname(scales), data$size)
  purrr::walk(cartoons, expect_s3_class, "glycan_grid_grob")
  expect_equal(widths[[2]] / widths[[1]], 3)
  expect_equal(heights[[2]] / heights[[1]], 3)
})

test_that("geom_glycan exports scaled cartoons as vector graphics", {
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
    geom_glycan(size = 0.6)

  pdf_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf_file, compress = FALSE)
  print(plot)
  grDevices::dev.off()
  pdf_raw <- readBin(
    pdf_file,
    "raw",
    n = file.info(pdf_file)$size
  )

  expect_length(
    grepRaw("/Subtype /Image", pdf_raw, fixed = TRUE, all = TRUE),
    0
  )

  svg_file <- tempfile(fileext = ".svg")
  current_device <- grDevices::dev.cur()
  svg_available <- suppressWarnings(tryCatch(
    {
      grDevices::svg(svg_file)
      grDevices::dev.cur() != current_device
    },
    error = \(...) FALSE
  ))
  skip_if_not(svg_available, "The SVG graphics device is unavailable.")
  on.exit(
    if (grDevices::dev.cur() != current_device) {
      grDevices::dev.off()
    },
    add = TRUE
  )
  print(plot)
  grDevices::dev.off()
  svg <- readLines(svg_file, warn = FALSE)

  expect_equal(any(grepl("<image", svg, fixed = TRUE)), FALSE)
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

test_that("geom_glycan centers all orientations by default", {
  data <- data.frame(
    x = 1,
    y = 1,
    structure = "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$x, y = .data$y, structure = .data$structure)
  )
  grobs <- purrr::map(
    c("left", "right", "up", "down"),
    function(orient) {
      ggplot2::layer_grob(
        plot + geom_glycan(orient = orient)
      )[[1]]$children[[1]]
    }
  )

  expect_equal(
    unname(purrr::map_dbl(grobs, "glydraw_hjust")),
    rep(0.5, 4)
  )
  expect_equal(
    unname(purrr::map_dbl(grobs, "glydraw_vjust")),
    rep(0.5, 4)
  )
})

test_that("geom_glycan anchors vertical glycans at their reducing ends", {
  structures <- c(
    paste0(
      "Man(??-?)[Man(??-?)]Man(??-?)[Man(??-?)]Man(??-?)",
      "GlcNAc(??-?)GlcNAc(??-"
    ),
    "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
  )
  data <- data.frame(
    x = 0,
    y = seq_along(structures),
    structure = structures
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$x, y = .data$y, structure = .data$structure)
  ) +
    geom_glycan(orient = "up", hjust = hjust_red_end())

  grobs <- ggplot2::layer_grob(plot)[[1]]$children
  displacement <- purrr::map_dbl(grobs, .reducing_end_displacement, "x")

  expect_equal(unname(displacement), rep(0, length(structures)))
})

test_that("geom_glycan anchors horizontal glycans at their reducing ends", {
  structures <- c(
    "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-",
    "Gal(b1-3)[Fuc(a1-4)]GlcNAc(b1-"
  )
  data <- data.frame(
    x = seq_along(structures),
    y = 0,
    structure = structures
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$x, y = .data$y, structure = .data$structure)
  ) +
    geom_glycan(orient = "left", vjust = vjust_red_end())

  grobs <- ggplot2::layer_grob(plot)[[1]]$children
  displacement <- purrr::map_dbl(grobs, .reducing_end_displacement, "y")

  expect_equal(unname(displacement), rep(0, length(structures)))
})

test_that("reducing-end justification helpers require matching orientations", {
  expect_snapshot(
    error = TRUE,
    geom_glycan(orient = "left", hjust = hjust_red_end())
  )
  expect_snapshot(
    error = TRUE,
    geom_glycan(orient = "up", vjust = vjust_red_end())
  )
})

test_that("geom_glycan rotates cartoons independently of their orientation", {
  data <- data.frame(
    x = c(1, 2),
    y = c(1, 1),
    angle = c(30, -45),
    structure = rep("Gal(b1-3)GalNAc(a1-", 2)
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      structure = .data$structure,
      angle = .data$angle
    )
  ) +
    geom_glycan(orient = "up")
  static_plot <- ggplot2::ggplot(
    data[1, c("x", "y", "structure")],
    ggplot2::aes(x = .data$x, y = .data$y, structure = .data$structure)
  ) +
    geom_glycan(orient = "up", angle = 90)

  layer <- ggplot2::layer_grob(plot)[[1]]
  static_grob <- ggplot2::layer_grob(static_plot)[[1]]$children[[1]]

  expect_equal(
    unname(purrr::map_dbl(layer$children, "glydraw_angle")),
    data$angle
  )
  expect_equal(
    unname(purrr::map_dbl(layer$children, ~ .x$vp$angle)),
    data$angle
  )
  expect_true(all(purrr::map_lgl(
    layer$children,
    ~ identical(
      .x$polygon_coor$shape,
      glycanGrob(data$structure[[1]], orient = "up")$polygon_coor$shape
    )
  )))
  expect_equal(static_grob$glydraw_angle, 90)
  expect_equal(static_grob$vp$angle, 90)
})

test_that("geom_glycan justifies content while preserving drawing allowance", {
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
    geom_glycan(orient = "up", vjust = 0)

  grob <- ggplot2::layer_grob(plot)[[1]]$children[[1]]
  geom_cartoon <- .glycan_grob_to_plot(grob)
  geom_built <- ggplot2::ggplot_build(geom_cartoon)
  geom_x_data_range <- geom_built$layout$panel_scales_x[[1]]$range$range
  geom_y_data_range <- geom_built$layout$panel_scales_y[[1]]$range$range
  geom_x_panel_range <- geom_built$layout$panel_params[[1]]$x.range
  geom_y_panel_range <- geom_built$layout$panel_params[[1]]$y.range
  geom_size <- attr(geom_cartoon, "glydraw_size_px")
  geom_panel_size <- attr(geom_cartoon, "glydraw_panel_size_px")
  standalone_cartoon <- draw_cartoon(data$structure, orient = "up")
  standalone_data_range <- ggplot2::get_panel_scales(
    standalone_cartoon
  )$y$range$range
  standalone_panel_range <- ggplot2::ggplot_build(
    standalone_cartoon
  )$layout$panel_params[[1]]$y.range
  standalone_size <- attr(standalone_cartoon, "glydraw_size_px")
  standalone_panel_size <- attr(
    standalone_cartoon,
    "glydraw_panel_size_px"
  )
  justification_offset <- .glycan_justification_offset(
    geom_cartoon,
    geom_size,
    scale = 1,
    hjust = 0.5,
    vjust = 0
  )
  endpoint <- min(grob$connect_df$end_y)
  endpoint_in <- (endpoint - geom_y_panel_range[[1]]) /
    diff(geom_y_panel_range) *
    geom_size[["height"]] /
    .default_cartoon_dpi
  endpoint_from_anchor <- justification_offset[["y"]] -
    geom_size[["height"]] / .default_cartoon_dpi / 2 +
    endpoint_in

  expect_equal(grob$glydraw_border_px, 0)
  expect_false(grob$glydraw_background)
  expect_equal(geom_size, geom_panel_size)
  expect_lt(geom_x_panel_range[[1]], geom_x_data_range[[1]])
  expect_gt(geom_x_panel_range[[2]], geom_x_data_range[[2]])
  expect_lt(geom_y_panel_range[[1]], geom_y_data_range[[1]])
  expect_gt(geom_y_panel_range[[2]], geom_y_data_range[[2]])
  expect_equal(endpoint_from_anchor, 0)
  expect_s3_class(
    ggplot2::calc_element("plot.background", geom_cartoon$theme),
    "element_blank"
  )
  expect_equal(
    (standalone_size - standalone_panel_size) / 2,
    c(width = 50, height = 50)
  )
  expect_lt(standalone_panel_range[[1]], standalone_data_range[[1]])
})
