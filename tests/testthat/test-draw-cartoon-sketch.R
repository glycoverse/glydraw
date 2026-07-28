skip_if_not_installed("ggsketch", minimum_version = "2.0.0")

test_that("draw_cartoon_sketch shows linkage annotations by default", {
  default <- draw_cartoon_sketch("Gal(b1-3)GalNAc(a1-", seed = 1)
  hidden <- draw_cartoon_sketch(
    "Gal(b1-3)GalNAc(a1-",
    show_linkage = FALSE,
    seed = 1
  )

  expect_identical(formals(draw_cartoon_sketch)$show_linkage, TRUE)
  expect_s3_class(default$layers[[4]]$geom, "GeomText")
  expect_length(hidden$layers, 3)
})

test_that("draw_cartoon_sketch uses one handwriting font for text labels", {
  plot <- draw_cartoon_sketch(
    "Gal(b1-3)GalNAc(a1-",
    show_linkage = TRUE,
    seed = 1
  )
  text <- ggplot2::ggplot_build(plot)$data[[4]]

  expect_setequal(text$label, c("\u03b2", "3", "\u03b1"))
  expect_identical(plot$layers[[4]]$geom_params$parse, FALSE)
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
  expect_s3_class(plot$layers[[3]]$geom, "GeomSketchPolygon")
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

  expect_s3_class(plot$layers[[5]]$geom, "GeomSketchPath")
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

  expect_identical(
    attr(plot, "glydraw_font_family"),
    plot$layers[[4]]$aes_params$family
  )
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
})
