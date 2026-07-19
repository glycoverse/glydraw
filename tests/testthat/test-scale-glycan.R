.axis_glycan_labels <- function(plot, axis_name) {
  plot_grob <- ggplot2::ggplotGrob(plot)
  axis_grob <- plot_grob$grobs[[match(axis_name, plot_grob$layout$name)]]
  labels <- purrr::keep(
    axis_grob$children$axis$grobs,
    inherits,
    what = "glycan_axis_labels"
  )

  labels[[1]]
}

.axis_glycan_title_gap <- function(labels, side) {
  label <- labels$children[[1]]
  child <- label$children[[1]]
  grid::pushViewport(grid::viewport(
    width = grid::grobWidth(labels),
    height = grid::grobHeight(labels)
  ))
  on.exit(grid::popViewport())

  width <- grid::convertWidth(grid::grobWidth(labels), "mm", valueOnly = TRUE)
  height <- grid::convertHeight(
    grid::grobHeight(labels),
    "mm",
    valueOnly = TRUE
  )
  child_width <- grid::convertWidth(
    grid::grobWidth(child),
    "mm",
    valueOnly = TRUE
  )
  child_height <- grid::convertHeight(
    grid::grobHeight(child),
    "mm",
    valueOnly = TRUE
  )
  x <- grid::convertX(label$vp$x, "mm", valueOnly = TRUE)
  y <- grid::convertY(label$vp$y, "mm", valueOnly = TRUE)

  switch(
    side,
    bottom = y - label$glydraw_vjust * child_height,
    top = height - y - (1 - label$glydraw_vjust) * child_height,
    left = x - label$glydraw_hjust * child_width,
    right = width - x - (1 - label$glydraw_hjust) * child_width
  )
}

test_that("scale_x_glycan draws vertical cartoon labels", {
  data <- data.frame(
    structure = c(
      "Gal(b1-3)GalNAc(a1-",
      "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
    ),
    value = c(1, 2)
  )

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$structure, y = .data$value)
  ) +
    ggplot2::geom_col() +
    scale_x_glycan()
  labels <- .axis_glycan_labels(plot, "axis-b")

  expect_s3_class(labels, "glycan_axis_labels")
  expect_length(labels$children, nrow(data))
  purrr::walk(labels$children, expect_s3_class, "glycanGrob")
  expect_true(all(purrr::map_lgl(
    labels$children,
    "glydraw_axis_vertical"
  )))
  expect_equal(
    unname(purrr::map_chr(labels$children, "glydraw_hjust")),
    rep(hjust_red_end(), nrow(data))
  )
  expect_equal(
    unname(purrr::map_dbl(labels$children, "glydraw_vjust")),
    c(0, 0)
  )
  expect_no_error(ggplot2::ggplotGrob(plot))
})

test_that("scale_y_glycan draws horizontal cartoon labels", {
  data <- data.frame(
    structure = c(
      "Gal(b1-3)GalNAc(a1-",
      "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
    ),
    value = c(1, 2)
  )

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$value, y = .data$structure)
  ) +
    ggplot2::geom_col() +
    scale_y_glycan()
  labels <- .axis_glycan_labels(plot, "axis-l")

  expect_s3_class(labels, "glycan_axis_labels")
  expect_length(labels$children, nrow(data))
  purrr::walk(labels$children, expect_s3_class, "glycanGrob")
  expect_false(any(purrr::map_lgl(
    labels$children,
    "glydraw_axis_vertical"
  )))
  expect_equal(
    unname(purrr::map_dbl(labels$children, "glydraw_hjust")),
    c(1, 1)
  )
  expect_equal(
    unname(purrr::map_chr(labels$children, "glydraw_vjust")),
    rep(vjust_red_end(), nrow(data))
  )
  expect_no_error(ggplot2::ggplotGrob(plot))
})

test_that("glycan axis scales accept glycan structure vectors", {
  structures <- glyrepr::as_glycan_structure(c(
    "GalNAc(a1-",
    "Gal(b1-3)GalNAc(a1-"
  ))
  data <- tibble::tibble(structure = structures, value = c(1, 2))
  x_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$structure, y = .data$value)
  ) +
    ggplot2::geom_col() +
    scale_x_glycan()
  y_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$value, y = .data$structure)
  ) +
    ggplot2::geom_col() +
    scale_y_glycan()

  x_labels <- .axis_glycan_labels(x_plot, "axis-b")
  y_labels <- .axis_glycan_labels(y_plot, "axis-l")

  expect_length(x_labels$children, length(structures))
  expect_length(y_labels$children, length(structures))
  purrr::walk(x_labels$children, expect_s3_class, "glycanGrob")
  purrr::walk(y_labels$children, expect_s3_class, "glycanGrob")
})

test_that("glycan axis scales adapt their configuration to coord_flip", {
  data <- data.frame(
    structure = "Gal(b1-3)GalNAc(a1-",
    value = 1
  )
  x_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$structure, y = .data$value)
  ) +
    ggplot2::geom_col() +
    scale_x_glycan() +
    ggplot2::coord_flip()
  y_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$value, y = .data$structure)
  ) +
    ggplot2::geom_col() +
    scale_y_glycan() +
    ggplot2::coord_flip()
  x_label <- .axis_glycan_labels(x_plot, "axis-l")$children[[1]]
  y_label <- .axis_glycan_labels(y_plot, "axis-b")$children[[1]]

  expect_false(x_label$glydraw_axis_vertical)
  expect_equal(x_label$glydraw_hjust, 1)
  expect_equal(x_label$glydraw_vjust, vjust_red_end())
  expect_true(y_label$glydraw_axis_vertical)
  expect_equal(y_label$glydraw_hjust, hjust_red_end())
  expect_equal(y_label$glydraw_vjust, 0)
})

test_that("glycan axis alignment can be adjusted", {
  data <- data.frame(
    structure = "Gal(b1-3)GalNAc(a1-",
    value = 1
  )
  x_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$structure, y = .data$value)
  ) +
    ggplot2::geom_col() +
    scale_x_glycan(vjust = 1)
  y_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$value, y = .data$structure)
  ) +
    ggplot2::geom_col() +
    scale_y_glycan(hjust = 0)
  x_label <- .axis_glycan_labels(x_plot, "axis-b")$children[[1]]
  y_label <- .axis_glycan_labels(y_plot, "axis-l")$children[[1]]

  expect_equal(x_label$glydraw_vjust, 1)
  expect_equal(y_label$glydraw_hjust, 0)
})

test_that("glycan axis scales anchor labels at their reducing ends by default", {
  structures <- c(
    paste0(
      "Man(??-?)[Man(??-?)]Man(??-?)[Man(??-?)]Man(??-?)",
      "GlcNAc(??-?)GlcNAc(??-"
    ),
    "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
  )
  data <- data.frame(structure = structures, value = c(1, 2))
  x_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$structure, y = .data$value)
  ) +
    ggplot2::geom_col() +
    scale_x_glycan()
  y_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$value, y = .data$structure)
  ) +
    ggplot2::geom_col() +
    scale_y_glycan()

  x_labels <- .axis_glycan_labels(x_plot, "axis-b")$children
  y_labels <- .axis_glycan_labels(y_plot, "axis-l")$children

  expect_equal(
    unname(purrr::map_dbl(x_labels, .reducing_end_displacement, "x")),
    rep(0, length(structures))
  )
  expect_equal(
    unname(purrr::map_dbl(y_labels, .reducing_end_displacement, "y")),
    rep(0, length(structures))
  )
})

test_that("glycan axis reducing-end helpers require matching orientations", {
  expect_snapshot(
    error = TRUE,
    scale_x_glycan(vjust = vjust_red_end())
  )
  expect_snapshot(
    error = TRUE,
    scale_y_glycan(hjust = hjust_red_end())
  )
})

test_that("default glycan axis reducing-end alignment adapts to coord_flip", {
  data <- data.frame(
    structure = "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-",
    value = 1
  )
  x_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$structure, y = .data$value)
  ) +
    ggplot2::geom_col() +
    scale_x_glycan() +
    ggplot2::coord_flip()
  y_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$value, y = .data$structure)
  ) +
    ggplot2::geom_col() +
    scale_y_glycan() +
    ggplot2::coord_flip()

  x_label <- .axis_glycan_labels(x_plot, "axis-l")$children[[1]]
  y_label <- .axis_glycan_labels(y_plot, "axis-b")$children[[1]]

  expect_equal(x_label$glydraw_hjust, 1)
  expect_equal(x_label$glydraw_vjust, vjust_red_end())
  expect_equal(.reducing_end_displacement(x_label, "y"), 0)
  expect_equal(y_label$glydraw_hjust, hjust_red_end())
  expect_equal(y_label$glydraw_vjust, 0)
  expect_equal(.reducing_end_displacement(y_label, "x"), 0)
})

test_that("glycan axis scales rotate labels independently of orientation", {
  data <- data.frame(
    structure = "Gal(b1-3)GalNAc(a1-",
    value = 1
  )
  x_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$structure, y = .data$value)
  ) +
    ggplot2::geom_col() +
    scale_x_glycan(angle = 90)
  x_unrotated_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$structure, y = .data$value)
  ) +
    ggplot2::geom_col() +
    scale_x_glycan()
  y_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$value, y = .data$structure)
  ) +
    ggplot2::geom_col() +
    scale_y_glycan(angle = -45)
  x_labels <- .axis_glycan_labels(x_plot, "axis-b")
  x_unrotated_labels <- .axis_glycan_labels(x_unrotated_plot, "axis-b")
  x_label <- x_labels$children[[1]]
  y_label <- .axis_glycan_labels(y_plot, "axis-l")$children[[1]]

  expect_true(x_label$glydraw_axis_vertical)
  expect_false(y_label$glydraw_axis_vertical)
  expect_equal(x_label$glydraw_angle, 90)
  expect_equal(y_label$glydraw_angle, -45)
  expect_equal(x_label$vp$angle, 90)
  expect_equal(y_label$vp$angle, -45)
  expect_equal(
    grid::convertWidth(grid::grobWidth(x_labels), "mm", valueOnly = TRUE),
    grid::convertHeight(
      grid::grobHeight(x_unrotated_labels),
      "mm",
      valueOnly = TRUE
    )
  )
  expect_equal(
    grid::convertHeight(grid::grobHeight(x_labels), "mm", valueOnly = TRUE),
    grid::convertWidth(
      grid::grobWidth(x_unrotated_labels),
      "mm",
      valueOnly = TRUE
    )
  )
})

test_that("glycan axis labels can be nudged", {
  data <- data.frame(
    structure = "Gal(b1-3)GalNAc(a1-",
    value = 1
  )
  x_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$structure, y = .data$value)
  ) +
    ggplot2::geom_col() +
    scale_x_glycan(nudge_x = 1, nudge_y = -2)
  y_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$value, y = .data$structure)
  ) +
    ggplot2::geom_col() +
    scale_y_glycan(nudge_x = -3, nudge_y = 4)
  x_label <- .axis_glycan_labels(x_plot, "axis-b")$children[[1]]
  y_label <- .axis_glycan_labels(y_plot, "axis-l")$children[[1]]

  expect_equal(x_label$glydraw_nudge_x, 1)
  expect_equal(x_label$glydraw_nudge_y, -2)
  expect_equal(y_label$glydraw_nudge_x, -3)
  expect_equal(y_label$glydraw_nudge_y, 4)
})

test_that("perpendicular nudges preserve the axis-title gap", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  data <- data.frame(
    structure = "Gal(b1-3)GalNAc(a1-",
    value = 1
  )
  x_plot <- function(position, nudge_y) {
    ggplot2::ggplot(
      data,
      ggplot2::aes(x = .data$structure, y = .data$value)
    ) +
      ggplot2::geom_col() +
      scale_x_glycan(position = position, nudge_y = nudge_y)
  }
  y_plot <- function(position, nudge_x) {
    ggplot2::ggplot(
      data,
      ggplot2::aes(x = .data$value, y = .data$structure)
    ) +
      ggplot2::geom_col() +
      scale_y_glycan(position = position, nudge_x = nudge_x)
  }

  bottom <- .axis_glycan_labels(x_plot("bottom", 0), "axis-b")
  bottom_nudged <- .axis_glycan_labels(x_plot("bottom", -4), "axis-b")
  bottom_inward <- .axis_glycan_labels(x_plot("bottom", 4), "axis-b")
  top <- .axis_glycan_labels(x_plot("top", 0), "axis-t")
  top_nudged <- .axis_glycan_labels(x_plot("top", 4), "axis-t")
  left <- .axis_glycan_labels(y_plot("left", 0), "axis-l")
  left_nudged <- .axis_glycan_labels(y_plot("left", -4), "axis-l")
  left_inward <- .axis_glycan_labels(y_plot("left", 4), "axis-l")
  right <- .axis_glycan_labels(y_plot("right", 0), "axis-r")
  right_nudged <- .axis_glycan_labels(y_plot("right", 4), "axis-r")

  expect_equal(
    .axis_glycan_title_gap(bottom_nudged, "bottom"),
    .axis_glycan_title_gap(bottom, "bottom")
  )
  expect_equal(
    .axis_glycan_title_gap(bottom_inward, "bottom"),
    .axis_glycan_title_gap(bottom, "bottom")
  )
  expect_equal(
    .axis_glycan_title_gap(top_nudged, "top"),
    .axis_glycan_title_gap(top, "top")
  )
  expect_equal(
    .axis_glycan_title_gap(left_nudged, "left"),
    .axis_glycan_title_gap(left, "left")
  )
  expect_equal(
    .axis_glycan_title_gap(left_inward, "left"),
    .axis_glycan_title_gap(left, "left")
  )
  expect_equal(
    .axis_glycan_title_gap(right_nudged, "right"),
    .axis_glycan_title_gap(right, "right")
  )
  expect_equal(
    grid::convertHeight(
      grid::grobHeight(bottom_nudged) - grid::grobHeight(bottom),
      "mm",
      valueOnly = TRUE
    ),
    4
  )
  expect_equal(
    grid::convertWidth(
      grid::grobWidth(left_nudged) - grid::grobWidth(left),
      "mm",
      valueOnly = TRUE
    ),
    4
  )
})

test_that("glycan axis labels support reducing-end annotations", {
  data <- data.frame(
    structure = "Gal(b1-3)GalNAc(a1-",
    value = 1
  )
  x_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$structure, y = .data$value)
  ) +
    ggplot2::geom_col() +
    scale_x_glycan(red_end = "~")
  y_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$value, y = .data$structure)
  ) +
    ggplot2::geom_col() +
    scale_y_glycan(red_end = "Reducing end")
  x_label <- .axis_glycan_labels(x_plot, "axis-b")$children[[1]]
  y_label <- .axis_glycan_labels(y_plot, "axis-l")$children[[1]]

  expect_gt(nrow(x_label$annotation_data$reducing_info$wave), 0)
  expect_true(any(
    y_label$annotation_data$reducing_info$annotation$is_red_end_text
  ))
  expect_match(
    y_label$annotation_data$reducing_info$annotation$annot[[2]],
    "Reducing end"
  )
})

test_that("glycan axis scales use plain cartoon parameters", {
  data <- data.frame(
    structure = "Gal(b1-3)GalNAc(a1-",
    value = 1
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$structure, y = .data$value)
  ) +
    ggplot2::geom_col() +
    scale_x_glycan(
      size = 0.6,
      style = glydraw_style(show_linkage = TRUE, edge_linewidth = 1.1)
    )
  label <- .axis_glycan_labels(plot, "axis-b")$children[[1]]

  expect_false("guide" %in% names(formals(scale_x_glycan)))
  expect_false("guide" %in% names(formals(scale_y_glycan)))
  expect_equal(label$glydraw_scale, 0.6)
  expect_true(label$show_linkage)
  expect_equal(label$edge_linewidth, 1.1)
})
