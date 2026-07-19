test_that("draw_cartoon works with valid branched glycan structure", {
  structure <- "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"

  expect_s3_class(
    draw_cartoon(structure),
    "glydraw_cartoon"
  )
  expect_s3_class(
    draw_cartoon(structure),
    "ggplot"
  )
})

test_that("draw_cartoon uses ggplot2 fixed panel sizing", {
  structure <- "Gal(b1-3)GalNAc(a1-"

  plot <- draw_cartoon(structure)

  expect_s3_class(plot, "glydraw_cartoon")
  expect_false(inherits(plot, "ggview"))
  expect_s3_class(plot$theme$panel.widths, "unit")
  expect_s3_class(plot$theme$panel.heights, "unit")
  expect_named(attr(plot, "glydraw_size_px"), c("width", "height"))
})

test_that("draw_cartoon controls edge and node linewidths", {
  structure <- "Gal(b1-3)GalNAc(a1-"

  default_plot <- draw_cartoon(structure)
  default_layers <- ggplot2::ggplot_build(default_plot)$data

  expect_equal(unique(default_layers[[1]]$linewidth), 0.8)
  expect_equal(unique(default_layers[[2]]$linewidth), 0.8)
  expect_equal(unique(default_layers[[3]]$linewidth), 0.8)

  custom_plot <- draw_cartoon(
    structure,
    red_end = "~",
    edge_linewidth = 1.2,
    node_linewidth = 0.4
  )
  custom_layers <- ggplot2::ggplot_build(custom_plot)$data

  expect_equal(unique(custom_layers[[1]]$linewidth), 1.2)
  expect_equal(unique(custom_layers[[2]]$linewidth), 0.4)
  expect_equal(unique(custom_layers[[3]]$linewidth), 0.4)
  expect_equal(unique(custom_layers[[5]]$linewidth), 1.2)
})

test_that("draw_cartoon applies custom monosaccharide colors over defaults", {
  structure <- "Gal(b1-4)GlcNAc(b1-"

  plot <- draw_cartoon(structure, colors = c(Gal = "#123456"))
  node_fill <- unique(ggplot2::ggplot_build(plot)$data[[3]]$fill)

  expect_contains(node_fill, "#123456")
  expect_contains(node_fill, "#0072BC")
})

test_that("draw_cartoon accepts reusable glydraw styles", {
  structure <- "Gal(b1-4)GlcNAc(b1-"
  style <- glydraw_style(
    show_linkage = FALSE,
    orient = "V",
    edge_linewidth = 1.2,
    colors = c(Gal = "#123456")
  )

  styled_plot <- draw_cartoon(structure, style = style)
  styled_layers <- ggplot2::ggplot_build(styled_plot)$data
  override_plot <- draw_cartoon(
    structure,
    style = style,
    edge_linewidth = 0.4
  )
  override_layers <- ggplot2::ggplot_build(override_plot)$data

  expect_s3_class(style, "glydraw_style")
  expect_equal(unique(styled_layers[[1]]$linewidth), 1.2)
  expect_contains(unique(styled_layers[[3]]$fill), "#123456")
  expect_equal(unique(override_layers[[1]]$linewidth), 0.4)
})

test_that("draw_cartoon validates NULL style overrides", {
  structure <- "Gal(b1-4)GlcNAc(b1-"
  style <- glydraw_style(edge_linewidth = 1.2)

  expect_error(
    draw_cartoon(structure, style = style, edge_linewidth = NULL),
    "edge_linewidth"
  )
})

test_that("draw_cartoon rejects unsupported custom color names", {
  structure <- "Gal(b1-4)GlcNAc(b1-"

  expect_error(
    draw_cartoon(structure, colors = c(NotAMono = "#123456")),
    "supported monosaccharides"
  )
})

test_that("draw_cartoon warns and hides linkage annotations for oversized nodes", {
  structure <- "Gal(b1-3)GalNAc(a1-"

  expect_warning(
    draw_cartoon(structure, node_size = 1.25, red_end = "Ser/Thr"),
    "Linkage annotations are hidden"
  )
})

test_that("draw_cartoon rejects node_size values that make residues overlap", {
  structure <- "Gal(b1-3)GalNAc(a1-"

  expect_error(
    draw_cartoon(structure, node_size = 2.1),
    "`node_size` must be no larger than 2"
  )
  expect_warning(
    expect_s3_class(draw_cartoon(structure, node_size = 2), "glydraw_cartoon"),
    "Linkage annotations are hidden"
  )
})

test_that("print.glydraw_cartoon rasterizes fixed-size cartoon for display", {
  structure <- paste0(
    "Gal(b1-4)GlcNAc(b1-2)[Gal(b1-4)GlcNAc(b1-4)]Man(a1-3)",
    "[Gal(b1-4)GlcNAc(b1-2)[Gal(b1-4)GlcNAc(b1-4)]",
    "[Gal(b1-4)GlcNAc(b1-6)]Man(a1-6)]Man(b1-4)GlcNAc(b1-4)",
    "[Fuc(a1-6)]GlcNAc(b1-"
  )
  plot <- draw_cartoon(structure)
  original_width <- as.numeric(plot$theme$panel.widths)
  size <- attr(plot, "glydraw_size_px")
  raster <- .render_cartoon_raster(plot)
  file <- tempfile(fileext = ".png")

  expect_s3_class(raster, "nativeRaster")
  expect_equal(ncol(raster), size[["width"]], tolerance = 1)
  expect_equal(nrow(raster), size[["height"]], tolerance = 1)

  grDevices::png(file, width = 4, height = 3, units = "in", res = 300)
  on.exit(grDevices::dev.off())
  printed_plot <- print(plot)

  expect_identical(printed_plot, plot)
  expect_equal(as.numeric(plot$theme$panel.widths), original_width)
})

test_that("draw_cartoon works with vertical orientation", {
  structure <- "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"

  v_plot <- draw_cartoon(structure, orient = "V")
  expect_s3_class(v_plot, "glydraw_cartoon")
  expect_s3_class(v_plot, "ggplot")
})

test_that("left and right Fuc-like triangles align with rectangle borders", {
  rectangle <- glycan_shape[["HexNAc"]]
  shape_names <- c("dHexRight", "dHexLeft", "dHexNAcRight", "dHexNAcLeft")

  purrr::walk(shape_names, function(shape_name) {
    shape <- glycan_shape[[shape_name]]

    expect_equal(
      range(shape$x),
      range(rectangle$x),
      info = shape_name
    )
  })
})

test_that("draw_cartoon left-aligns vertical substituent labels", {
  structure <- "Neu5Ac9Ac(a2-3)Gal6S(b1-"

  plot <- draw_cartoon(structure, orient = "V")
  annotation <- ggplot2::ggplot_build(plot)$data[[4]]
  substituent <- dplyr::filter(annotation, .data$label == '"9Ac"')
  x_range <- ggplot2::get_panel_scales(plot)$x$range$range

  expect_equal(substituent$hjust, 0)
  expect_gt(x_range[[2]], substituent$x + 0.5)
})

test_that("draw_cartoon bottom-aligns horizontal substituent labels", {
  structure <- "Neu5Ac9Ac(a2-3)Gal6S(b1-"

  plot <- draw_cartoon(structure, orient = "H")
  annotation <- ggplot2::ggplot_build(plot)$data[[4]]
  substituent <- dplyr::filter(annotation, .data$label == '"9Ac"')
  y_range <- ggplot2::get_panel_scales(plot)$y$range$range

  expect_equal(substituent$vjust, 0)
  expect_gt(y_range[[2]], substituent$y + 0.3)
})

test_that("draw_cartoon works with linkage hidden", {
  structure <- "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"

  plot_no_linkage <- draw_cartoon(structure, show_linkage = FALSE)
  expect_s3_class(plot_no_linkage, "glydraw_cartoon")
})

test_that("draw_cartoon works with reducing-end O-Fuc glycans", {
  glycans <- c(
    "Fuc(a1-",
    "GlcNAc(b1-3)Fuc(a1-"
  )

  cartoons <- purrr::map(glycans, draw_cartoon)

  purrr::walk(cartoons, expect_s3_class, "glydraw_cartoon")
})

test_that("draw_cartoon preserves nested Xyl-Gal-Fuc side-chain order", {
  structure <- "Glc(b1-4)[Fuc(a1-2)Gal(b1-2)Xyl(a1-6)]Glc(b1-4)Glc(b1-"

  inputs <- .prepare_cartoon_inputs(structure, NULL, "H", "")
  graph <- inputs$structure
  coor <- inputs$coor
  mono <- igraph::V(graph)$mono
  fuc <- which(mono == "Fuc")
  gal <- which(mono == "Gal")
  xyl <- which(mono == "Xyl")

  expect_equal(
    unname(coor[c(xyl, gal, fuc), "x"]),
    rep(unname(coor[xyl, "x"]), 3)
  )
  expect_gt(coor[gal, "y"], coor[xyl, "y"])
  expect_gt(coor[fuc, "y"], coor[gal, "y"])

  annotation <- .cartoon_text_annotation_data(
    graph,
    coor,
    "H",
    "",
    NULL
  )$annotation
  fuc_labels <- dplyr::filter(annotation, .data$vertice == as.character(fuc))
  gal_labels <- dplyr::filter(annotation, .data$vertice == as.character(gal))

  expect_true(all(
    fuc_labels$y > coor[gal, "y"] & fuc_labels$y < coor[fuc, "y"]
  ))
  expect_true(all(
    gal_labels$y > coor[xyl, "y"] & gal_labels$y < coor[gal, "y"]
  ))
})
