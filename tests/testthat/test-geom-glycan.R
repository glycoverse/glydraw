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
