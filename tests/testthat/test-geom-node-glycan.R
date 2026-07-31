test_that("geom_node_glycan uses ggraph node positions", {
  skip_if_not_installed("ggraph")
  graph <- igraph::make_ring(2) |>
    igraph::set_vertex_attr(
      "structure",
      value = c(
        "GalNAc(a1-",
        "Gal(b1-3)GalNAc(a1-"
      )
    ) |>
    igraph::set_vertex_attr(
      "alpha",
      value = c(0.25, 0.75)
    )

  plot <- ggraph::ggraph(graph, layout = "linear") +
    geom_node_glycan(
      ggplot2::aes(
        structure = .data$structure,
        alpha = .data$alpha
      )
    ) +
    ggplot2::scale_alpha_identity()
  built <- ggplot2::ggplot_build(plot)

  expect_equal(built$data[[1]]$x, plot$data$x)
  expect_equal(built$data[[1]]$y, plot$data$y)
  expect_equal(built$data[[1]]$structure, plot$data$structure)
  expect_equal(built$data[[1]]$alpha, plot$data$alpha)
  expect_identical(plot$layers[[1]]$geom, GeomGlycan)
  expect_identical(plot$layers[[1]]$stat, ggraph::StatFilter)
  expect_identical(plot$layers[[1]]$inherit.aes, FALSE)
})

test_that("geom_node_glycan supports node filtering", {
  skip_if_not_installed("ggraph")
  data <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    keep = c(TRUE, FALSE),
    structure = c(
      "GalNAc(a1-",
      "Gal(b1-3)GalNAc(a1-"
    )
  )

  plot <- ggplot2::ggplot(data) +
    geom_node_glycan(
      ggplot2::aes(
        structure = .data$structure,
        filter = .data$keep
      )
    )
  built <- ggplot2::ggplot_build(plot)

  expect_equal(built$data[[1]]$structure, data$structure[[1]])
  expect_equal(nrow(built$data[[1]]), 1)
})

test_that("geom_node_glycan preserves explicit positions and rendering options", {
  skip_if_not_installed("ggraph")
  rplots <- "Rplots.pdf"
  unlink(rplots)
  on.exit(unlink(rplots), add = TRUE)
  data <- data.frame(
    x = 1,
    y = 2,
    shifted_x = 3,
    shifted_y = 4,
    structure = "Gal(b1-3)GalNAc(a1-"
  )

  plot <- ggplot2::ggplot(data) +
    geom_node_glycan(
      ggplot2::aes(
        x = .data$shifted_x,
        y = .data$shifted_y,
        structure = .data$structure
      ),
      orient = "up",
      show_linkage = FALSE
    )
  built <- ggplot2::ggplot_build(plot)
  grob <- ggplot2::layer_grob(plot)[[1]]$children[[1]]

  expect_equal(built$data[[1]]$x, data$shifted_x)
  expect_equal(built$data[[1]]$y, data$shifted_y)
  expect_identical(plot$layers[[1]]$geom_params$orient, "up")
  expect_identical(grob$show_linkage, FALSE)
})
