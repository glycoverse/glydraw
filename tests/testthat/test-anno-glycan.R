.annotation_glycan_structures <- function() {
  c(
    "GlcNAc(??-",
    "Gal(??-?)GlcNAc(??-",
    "Neu5Ac(??-?)Gal(??-?)GlcNAc(??-"
  )
}

.annotation_axis_glycan_labels <- function(plot, axis_name) {
  plot_grob <- ggplot2::ggplotGrob(plot)
  axis_grob <- plot_grob$grobs[[match(axis_name, plot_grob$layout$name)]]
  labels <- purrr::keep(
    axis_grob$children$axis$grobs,
    inherits,
    what = "glycan_axis_labels"
  )
  labels[[1]]
}

test_that("anno_glycan creates row and column annotation functions", {
  skip_if_not_installed("ComplexHeatmap")
  structures <- .annotation_glycan_structures()

  column <- anno_glycan(structures, which = "column")
  row <- anno_glycan(structures, which = "row")

  expect_s4_class(column, "AnnotationFunction")
  expect_s4_class(row, "AnnotationFunction")
  expect_equal(column@fun_name, "anno_glycan")
  expect_equal(row@fun_name, "anno_glycan")
  expect_equal(column@which, "column")
  expect_equal(row@which, "row")
  expect_equal(column@n, length(structures))
  expect_equal(row@n, length(structures))
  expect_identical(column@show_name, FALSE)
  expect_identical(row@show_name, FALSE)

  column_grobs <- column@var_env$grobs
  row_grobs <- row@var_env$grobs
  purrr::walk(column_grobs, expect_s3_class, "glycanGrob")
  purrr::walk(row_grobs, expect_s3_class, "glycanGrob")
  expect_equal(
    unname(purrr::map_lgl(column_grobs, "glydraw_axis_vertical")),
    rep(TRUE, length(structures))
  )
  expect_equal(
    unname(purrr::map_lgl(row_grobs, "glydraw_axis_vertical")),
    rep(FALSE, length(structures))
  )
  expect_equal(
    unname(purrr::map_chr(column_grobs, "glydraw_hjust")),
    rep(hjust_red_end(), length(structures))
  )
  expect_equal(
    unname(purrr::map_chr(row_grobs, "glydraw_vjust")),
    rep(vjust_red_end(), length(structures))
  )
  expect_equal(
    unname(purrr::map_chr(column_grobs, "glydraw_axis_position")),
    rep("bottom", length(structures))
  )
  expect_equal(
    unname(purrr::map_chr(row_grobs, "glydraw_axis_position")),
    rep("left", length(structures))
  )
})

test_that("anno_glycan accepts structure vectors and preserves duplicates", {
  skip_if_not_installed("ComplexHeatmap")
  structures <- glyrepr::as_glycan_structure(c(
    "GlcNAc(??-",
    "Gal(??-?)GlcNAc(??-",
    "GlcNAc(??-"
  ))
  annotation <- anno_glycan(structures, which = "row")

  expect_length(annotation@var_env$grobs, length(structures))
  expect_equal(
    unname(purrr::map_int(
      annotation@var_env$grobs,
      "glydraw_annotation_index"
    )),
    seq_along(structures)
  )
  expect_equal(
    annotation@var_env$grobs[[1]]$polygon_coor,
    annotation@var_env$grobs[[3]]$polygon_coor
  )
  expect_length(
    unique(purrr::map_chr(annotation@var_env$grobs, "name")),
    length(structures)
  )
})

test_that("anno_glycan matches glycan scale sizing and anchoring", {
  skip_if_not_installed("ComplexHeatmap")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  structures <- .annotation_glycan_structures()
  data <- data.frame(structure = structures, value = seq_along(structures))
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

  x_labels <- .annotation_axis_glycan_labels(x_plot, "axis-b")
  y_labels <- .annotation_axis_glycan_labels(y_plot, "axis-l")
  column <- anno_glycan(structures, which = "column")
  row <- anno_glycan(structures, which = "row")

  expect_equal(
    grid::convertHeight(column@height, "mm", valueOnly = TRUE),
    grid::convertHeight(
      grid::grobHeight(x_labels),
      "mm",
      valueOnly = TRUE
    )
  )
  expect_equal(
    grid::convertWidth(row@width, "mm", valueOnly = TRUE),
    grid::convertWidth(
      grid::grobWidth(y_labels),
      "mm",
      valueOnly = TRUE
    )
  )
  expect_equal(
    unname(purrr::map_dbl(
      column@var_env$grobs,
      .reducing_end_displacement,
      "x"
    )),
    rep(0, length(structures))
  )
  expect_equal(
    unname(purrr::map_dbl(
      row@var_env$grobs,
      .reducing_end_displacement,
      "y"
    )),
    rep(0, length(structures))
  )
})

test_that("anno_glycan supports scale label adjustments and styles", {
  skip_if_not_installed("ComplexHeatmap")
  colors <- glydraw_colors()
  colors["glyYellow"] <- "#123456"
  annotation <- anno_glycan(
    "Gal(b1-3)GalNAc(a1-",
    which = "row",
    side = "right",
    size = 0.6,
    angle = -45,
    hjust = 0.25,
    vjust = 0.75,
    nudge_x = 2,
    nudge_y = -3,
    show_linkage = FALSE,
    style = style_glydraw(
      red_end = "~",
      colors = colors,
      edge_linewidth = 1.1,
      node_linewidth = 0.3
    )
  )
  grob <- annotation@var_env$grobs[[1]]
  positioned <- .glycan_annotation_grob(
    annotation@var_env$grobs,
    1,
    "row",
    "right"
  )$children[[1]]

  expect_equal(grob$glydraw_scale, 0.6)
  expect_equal(grob$glydraw_angle, -45)
  expect_equal(grob$glydraw_hjust, 0.25)
  expect_equal(grob$glydraw_vjust, 0.75)
  expect_equal(grob$glydraw_nudge_x, 2)
  expect_equal(grob$glydraw_nudge_y, -3)
  expect_equal(grob$glydraw_axis_position, "right")
  expect_identical(grob$show_linkage, FALSE)
  expect_equal(grob$edge_linewidth, 1.1)
  expect_equal(grob$node_linewidth, 0.3)
  expect_contains(grob$filled_color, "#123456")
  expect_gt(nrow(grob$annotation_data$reducing_info$wave), 0)
  expect_equal(positioned$vp$angle, -45)
})

test_that("anno_glycan sizes rotated and nudged labels like glycan scales", {
  skip_if_not_installed("ComplexHeatmap")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  structures <- .annotation_glycan_structures()
  data <- data.frame(structure = structures, value = seq_along(structures))
  x_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$structure, y = .data$value)
  ) +
    ggplot2::geom_col() +
    scale_x_glycan(position = "top", angle = 90, nudge_y = 4)
  y_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$value, y = .data$structure)
  ) +
    ggplot2::geom_col() +
    scale_y_glycan(position = "right", angle = -45, nudge_x = 4)
  x_labels <- .annotation_axis_glycan_labels(x_plot, "axis-t")
  y_labels <- .annotation_axis_glycan_labels(y_plot, "axis-r")
  column <- anno_glycan(
    structures,
    which = "column",
    side = "top",
    angle = 90,
    nudge_y = 4
  )
  row <- anno_glycan(
    structures,
    which = "row",
    side = "right",
    angle = -45,
    nudge_x = 4
  )

  expect_equal(
    grid::convertHeight(column@height, "mm", valueOnly = TRUE),
    grid::convertHeight(
      grid::grobHeight(x_labels),
      "mm",
      valueOnly = TRUE
    )
  )
  expect_equal(
    grid::convertWidth(row@width, "mm", valueOnly = TRUE),
    grid::convertWidth(
      grid::grobWidth(y_labels),
      "mm",
      valueOnly = TRUE
    )
  )
})

test_that("anno_glycan follows reordered and subset indices", {
  skip_if_not_installed("ComplexHeatmap")
  structures <- .annotation_glycan_structures()
  annotation <- anno_glycan(structures, which = "column")
  grob <- .glycan_annotation_grob(
    annotation@var_env$grobs,
    c(3, 1),
    "column",
    "bottom"
  )
  subset <- annotation[c(3, 1)]

  expect_equal(
    unname(purrr::map_int(grob$children, "glydraw_annotation_index")),
    c(3L, 1L)
  )
  expect_equal(
    unname(purrr::map_int(
      subset@var_env$grobs,
      "glydraw_annotation_index"
    )),
    c(3L, 1L)
  )
  expect_equal(subset@n, 2)
})

test_that("anno_glycan renders vector ComplexHeatmap labels", {
  skip_if_not_installed("ComplexHeatmap")
  structures <- .annotation_glycan_structures()
  matrix <- matrix(seq_len(9), nrow = 3)
  output <- tempfile(fileext = ".svg")
  current_device <- grDevices::dev.cur()
  svg_available <- suppressWarnings(tryCatch(
    {
      grDevices::svg(output, width = 5, height = 5)
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

  heatmap <- ComplexHeatmap::Heatmap(
    matrix,
    show_row_names = FALSE,
    show_column_names = FALSE,
    row_split = c("a", "a", "b"),
    column_split = c("a", "b", "b"),
    left_annotation = ComplexHeatmap::rowAnnotation(
      glycan = anno_glycan(structures, which = "row")
    ),
    bottom_annotation = ComplexHeatmap::HeatmapAnnotation(
      glycan = anno_glycan(structures, which = "column")
    )
  )
  expect_no_error(ComplexHeatmap::draw(heatmap))
  grDevices::dev.off()
  on.exit(NULL, add = FALSE)

  svg <- readLines(output, warn = FALSE)
  expect_length(grep("<image", svg, fixed = TRUE), 0)
  expect_gt(length(grep("<path", svg, fixed = TRUE)), 0)
})

test_that("anno_glycan validates structures, sides, and anchoring", {
  skip_if_not_installed("ComplexHeatmap")
  structure <- "Gal(b1-3)GalNAc(a1-"

  expect_snapshot(
    error = TRUE,
    anno_glycan(character(), which = "column")
  )
  expect_snapshot(
    error = TRUE,
    anno_glycan(structure, which = "column", side = "left")
  )
  expect_snapshot(
    error = TRUE,
    anno_glycan(
      structure,
      which = "row",
      hjust = hjust_red_end()
    )
  )
})
