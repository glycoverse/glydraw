skip_if_not_installed("ggsketch", minimum_version = "2.0.0")

test_that("draw_cartoon_sketch shows linkage annotations by default", {
  default <- draw_cartoon_sketch("Gal(b1-3)GalNAc(a1-", seed = 1)
  hidden <- draw_cartoon_sketch(
    "Gal(b1-3)GalNAc(a1-",
    show_linkage = FALSE,
    seed = 1
  )

  expect_identical(formals(draw_cartoon_sketch)$show_linkage, TRUE)
  default_text <- Filter(
    \(layer) inherits(layer$geom, "GeomText"),
    default$layers
  )
  hidden_text <- Filter(
    \(layer) inherits(layer$geom, "GeomText"),
    hidden$layers
  )
  expect_length(default_text, 1)
  expect_length(hidden_text, 0)
})

test_that("draw_cartoon_sketch uses one handwriting font for text labels", {
  plot <- draw_cartoon_sketch(
    "Gal(b1-3)GalNAc(a1-",
    show_linkage = TRUE,
    seed = 1
  )
  text_index <- which(vapply(
    plot$layers,
    \(layer) inherits(layer$geom, "GeomText"),
    logical(1)
  ))
  text_layer <- plot$layers[[text_index]]
  text <- ggplot2::ggplot_build(plot)$data[[text_index]]

  expect_setequal(text$label, c("\u03b2", "3", "\u03b1"))
  expect_identical(text_layer$geom_params$parse, FALSE)
  expect_identical(unique(text$family), attr(plot, "glydraw_font_family"))
  if (
    requireNamespace("systemfonts", quietly = TRUE) &&
      nzchar(unique(text$family))
  ) {
    expect_identical(
      .sketch_font_supports_labels(unique(text$family)),
      TRUE
    )
  }
})

test_that("sketch text preserves unknown and substituent labels", {
  annotation <- data.frame(
    annot = c("?", "??", '?1', '~"?"', "3S,6S", "Ser/Thr")
  )

  expect_identical(
    .sketch_annotation_labels(annotation),
    c("?", "?", "?", "?", "3S,6S", "Ser/Thr")
  )
})

test_that("sketch text unquotes custom reducing-end labels", {
  custom_label <- 'Ser/"Thr"\\site'
  plot <- draw_cartoon_sketch(
    "Gal(b1-3)GalNAc(a1-",
    style = style_glydraw(red_end = custom_label),
    seed = 1
  )
  text_index <- which(vapply(
    plot$layers,
    \(layer) inherits(layer$geom, "GeomText"),
    logical(1)
  ))
  text <- ggplot2::ggplot_build(plot)$data[[text_index]]

  expect_identical(sum(text$label == custom_label), 1L)
  expect_identical(
    .unquote_plotmath_text(.quote_plotmath_text(custom_label)),
    custom_label
  )
})

test_that("draw_cartoon_sketch red_end overrides its style", {
  plot <- draw_cartoon_sketch(
    "Gal(b1-3)GalNAc(a1-",
    style = style_glydraw(red_end = "~"),
    red_end = "Reducing end",
    seed = 1
  )
  text_index <- which(vapply(
    plot$layers,
    \(layer) inherits(layer$geom, "GeomText"),
    logical(1)
  ))
  text <- ggplot2::ggplot_build(plot)$data[[text_index]]

  expect_contains(text$label, "Reducing end")
})

test_that("draw_cartoon_sketch gives each node a reproducible random seed", {
  structure <- paste0(
    "Gal(b1-3)Gal(b1-3)",
    "GlcNAc(b1-3)GlcNAc(b1-"
  )
  first <- draw_cartoon_sketch(structure, seed = 11)
  second <- draw_cartoon_sketch(structure, seed = 11)
  changed <- draw_cartoon_sketch(structure, seed = 12)
  node_seeds <- function(plot) {
    layers <- Filter(
      \(layer) {
        inherits(layer$geom, "GeomSketchCircle") ||
          inherits(layer$geom, "GeomSketchPolygon")
      },
      plot$layers
    )
    vapply(layers, \(layer) layer$geom_params$seed, numeric(1))
  }

  expect_length(node_seeds(first), 4)
  expect_length(unique(node_seeds(first)), 4)
  expect_identical(node_seeds(first), node_seeds(second))
  expect_identical(identical(node_seeds(first), node_seeds(changed)), FALSE)
})

test_that("draw_cartoon_sketch pencil shades every node by default", {
  plot <- draw_cartoon_sketch(
    "Gal(b1-3)GlcNAc(b1-",
    roughness = 1,
    seed = 1
  )
  custom_gap <- draw_cartoon_sketch(
    "Gal(b1-3)GlcNAc(b1-",
    hachure_gap = 0.12,
    seed = 1
  )
  circle_layers <- Filter(
    \(layer) inherits(layer$geom, "GeomSketchCircle"),
    plot$layers
  )
  custom_circle_layers <- Filter(
    \(layer) inherits(layer$geom, "GeomSketchCircle"),
    custom_gap$layers
  )
  custom_polygon_layers <- Filter(
    \(layer) inherits(layer$geom, "GeomSketchPolygon"),
    custom_gap$layers
  )
  polygon_layers <- Filter(
    \(layer) inherits(layer$geom, "GeomSketchPolygon"),
    plot$layers
  )
  node_diameter <- 2 *
    .cartoon_circle_radius_inches(
      circle_layers[[1]]$data$radius[[1]]
    )

  expect_length(circle_layers, 1)
  expect_length(polygon_layers, 1)
  expect_equal(circle_layers[[1]]$aes_params$roughness, 0.1)
  expect_equal(polygon_layers[[1]]$geom_params$roughness, 1)
  expect_identical(formals(draw_cartoon_sketch)$fill_style, "pencil_shade")
  expect_identical(
    circle_layers[[1]]$geom_params$fill_style,
    "pencil_shade"
  )
  expect_identical(
    polygon_layers[[1]]$geom_params$fill_style,
    "pencil_shade"
  )
  expect_equal(circle_layers[[1]]$geom_params$fill_roughness, 0.5)
  expect_equal(polygon_layers[[1]]$geom_params$fill_roughness, 0.5)
  expect_equal(circle_layers[[1]]$geom_params$hachure_gap, 0.03)
  expect_equal(
    polygon_layers[[1]]$geom_params$hachure_gap,
    0.03 * node_diameter
  )
  expect_equal(custom_circle_layers[[1]]$geom_params$hachure_gap, 0.12)
  expect_equal(
    custom_polygon_layers[[1]]$geom_params$hachure_gap,
    0.12 * node_diameter
  )
})

test_that("draw_cartoon_sketch builds a fixed-size sketch cartoon", {
  plot <- draw_cartoon_sketch(
    "Gal(b1-3)GalNAc(a1-",
    seed = 42,
    style = style_glydraw(font_family = "sans")
  )

  expect_s3_class(plot, "glydraw_cartoon")
  expect_s3_class(plot, "ggplot")
  expect_s3_class(plot$layers[[1]]$geom, "GeomSketchSegment")
  expect_s3_class(plot$layers[[2]]$geom, "GeomPolygon")
  expect_s3_class(plot$layers[[3]]$geom, "GeomSketchCircle")
  expect_s3_class(plot$layers[[4]]$geom, "GeomSketchPolygon")
  expect_named(attr(plot, "glydraw_size_px"), c("width", "height"))
  expect_equal(attr(plot, "glydraw_font_family"), "sans")
})

test_that("draw_cartoon_sketch preserves cartoon controls", {
  structure <- glyrepr::as_glycan_structure("Gal(b1-3)GalNAc(a1-")
  plot <- draw_cartoon_sketch(
    structure,
    show_linkage = FALSE,
    orient = "up",
    highlight = 1,
    roughness = 0.5,
    bowing = 0,
    n_passes = 1,
    seed = 7,
    fill_style = "cross_hatch",
    hachure_angle = 30,
    hachure_gap = 0.1,
    fill_weight = 0.3,
    medium = "pencil"
  )
  layers <- ggplot2::ggplot_build(plot)$data

  expect_s3_class(plot, "glydraw_cartoon")
  expect_equal(unique(layers[[1]]$roughness), 0.5)
  expect_setequal(unique(layers[[1]]$alpha), c(0.3, 1))
})

test_that("draw_cartoon_sketch sketches reducing-end waves", {
  plot <- draw_cartoon_sketch(
    "Gal(b1-3)GalNAc(a1-",
    show_linkage = TRUE,
    style = style_glydraw(red_end = "~"),
    seed = 1
  )
  wave_layers <- Filter(
    \(layer) inherits(layer$geom, "GeomSketchPath"),
    plot$layers
  )

  expect_length(wave_layers, 1)
})

test_that("draw_cartoon_sketch saves with its resolved handwriting font", {
  plot <- draw_cartoon_sketch(
    "Gal(b1-3)GalNAc(a1-",
    show_linkage = TRUE,
    seed = 1
  )
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)

  save_cartoon(plot, file)
  text_layer <- Filter(
    \(layer) inherits(layer$geom, "GeomText"),
    plot$layers
  )[[1]]

  expect_identical(
    attr(plot, "glydraw_font_family"),
    text_layer$aes_params$family
  )
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
})
