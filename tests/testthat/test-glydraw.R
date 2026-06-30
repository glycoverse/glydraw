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
  rplots <- "Rplots.pdf"
  unlink(rplots)
  on.exit(unlink(rplots), add = TRUE)
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
  expect_false(file.exists(rplots))
})

test_that("save_cartoon writes fixed-size image without ggview", {
  structure <- "Gal(b1-3)GalNAc(a1-"
  plot <- draw_cartoon(structure)
  file <- tempfile(fileext = ".png")

  save_cartoon(plot, file)

  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
})

test_that("save_cartoon rejects invalid scale values", {
  structure <- "Gal(b1-3)GalNAc(a1-"
  plot <- draw_cartoon(structure)
  file <- tempfile(fileext = ".png")

  expect_error(
    save_cartoon(plot, file, scale = 0),
    "`scale`"
  )
})

test_that("draw_cartoon works with vertical orientation", {
  structure <- "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"

  v_plot <- draw_cartoon(structure, orient = "V")
  expect_s3_class(v_plot, "glydraw_cartoon")
  expect_s3_class(v_plot, "ggplot")
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

test_that("save_cartoon saves file correctly", {
  structure <- "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  cartoon <- draw_cartoon(structure)

  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file, recursive = FALSE), add = TRUE)

  expect_no_error(save_cartoon(cartoon, temp_file))
  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)
})

test_that("export_cartoons works with character vector input", {
  glycans <- c(
    "Man(a1-3)Man(b1-4)GlcNAc(b1-",
    "Gal(b1-4)GlcNAc(b1-"
  )
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(result <- export_cartoons(glycans, temp_dir))

  expect_type(result, "list")
  expect_length(result, 2)
  files <- fs::dir_ls(temp_dir, glob = "*.png")
  expect_length(files, 2)
})

test_that("export_cartoons forwards output scale to saved files", {
  glycans <- "Gal(b1-3)GalNAc(a1-"
  default_dir <- tempfile()
  scaled_dir <- tempfile()
  on.exit(unlink(c(default_dir, scaled_dir), recursive = TRUE), add = TRUE)
  fs::dir_create(default_dir)
  fs::dir_create(scaled_dir)

  suppressMessages(export_cartoons(glycans, default_dir))
  suppressMessages(export_cartoons(glycans, scaled_dir, scale = 2))
  default_file <- fs::dir_ls(default_dir, glob = "*.png")
  scaled_file <- fs::dir_ls(scaled_dir, glob = "*.png")
  default_image <- png::readPNG(default_file)
  scaled_image <- png::readPNG(scaled_file)

  expect_equal(dim(scaled_image)[1:2], dim(default_image)[1:2] * 2)
})

test_that("export_cartoons warns that dpi is ignored", {
  glycans <- "Gal(b1-3)GalNAc(a1-"
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  expect_warning(
    suppressMessages(export_cartoons(glycans, temp_dir, dpi = 72)),
    "`dpi` is deprecated and ignored"
  )
})

test_that("export_cartoons forwards custom linewidths", {
  glycans <- "Gal(b1-3)GalNAc(a1-"
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(
    result <- export_cartoons(
      glycans,
      temp_dir,
      edge_linewidth = 1.1,
      node_linewidth = 0.3
    )
  )
  layers <- ggplot2::ggplot_build(result[[1]])$data

  expect_equal(unique(layers[[1]]$linewidth), 1.1)
  expect_equal(unique(layers[[3]]$linewidth), 0.3)
})

test_that("export_cartoons forwards custom colors", {
  glycans <- "Gal(b1-4)GlcNAc(b1-"
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(
    result <- export_cartoons(
      glycans,
      temp_dir,
      colors = c(Gal = "#123456")
    )
  )
  node_fill <- unique(ggplot2::ggplot_build(result[[1]])$data[[3]]$fill)

  expect_contains(node_fill, "#123456")
  expect_contains(node_fill, "#0072BC")
})

test_that("export_cartoons rejects node_size values that make residues overlap", {
  glycans <- "Gal(b1-3)GalNAc(a1-"
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    suppressMessages(export_cartoons(glycans, temp_dir, node_size = 2.1)),
    "`node_size` must be no larger than 2"
  )
})

test_that("export_cartoons uses character vector names as filenames", {
  glycans <- c(
    core = "Man(a1-3)Man(b1-4)GlcNAc(b1-",
    antenna = "Gal(b1-4)GlcNAc(b1-"
  )
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(result <- export_cartoons(glycans, temp_dir))

  expect_length(result, 2)
  files <- fs::dir_ls(temp_dir, glob = "*.png")
  expect_setequal(fs::path_file(files), c("core.png", "antenna.png"))
})

test_that("export_cartoons falls back to structure filenames for empty names", {
  glycans <- c(
    core = "Man(a1-3)Man(b1-4)GlcNAc(b1-",
    "Gal(b1-4)GlcNAc(b1-"
  )
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(result <- export_cartoons(glycans, temp_dir))

  expect_length(result, 2)
  files <- fs::dir_ls(temp_dir, glob = "*.png")
  expect_setequal(
    fs::path_file(files),
    c("core.png", "Gal(b1-4)GlcNAc(b1-.png")
  )
})

test_that("export_cartoons removes duplicates for character input", {
  glycans <- c(
    "Man(a1-3)Man(b1-4)GlcNAc(b1-",
    "Man(a1-3)Man(b1-4)GlcNAc(b1-",
    "Gal(b1-4)GlcNAc(b1-"
  )
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(result <- export_cartoons(glycans, temp_dir))

  expect_length(result, 2)
  files <- fs::dir_ls(temp_dir, glob = "*.png")
  expect_length(files, 2)
})

test_that("export_cartoons works with glyrepr_structure input", {
  skip_if_not_installed("glyrepr")
  structures <- glyrepr::as_glycan_structure(c(
    "Man(a1-3)Man(b1-4)GlcNAc(b1-",
    "Gal(b1-4)GlcNAc(b1-"
  ))
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(result <- export_cartoons(structures, temp_dir))

  expect_type(result, "list")
  expect_length(result, 2)
  files <- fs::dir_ls(temp_dir, glob = "*.png")
  expect_length(files, 2)
})

test_that("export_cartoons uses glyrepr_structure names as filenames", {
  skip_if_not_installed("glyrepr")
  structures <- glyrepr::as_glycan_structure(c(
    core = "Man(a1-3)Man(b1-4)GlcNAc(b1-",
    antenna = "Gal(b1-4)GlcNAc(b1-"
  ))
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(result <- export_cartoons(structures, temp_dir))

  expect_length(result, 2)
  files <- fs::dir_ls(temp_dir, glob = "*.png")
  expect_setequal(fs::path_file(files), c("core.png", "antenna.png"))
})

test_that("export_cartoons removes duplicates for glyrepr_structure input", {
  skip_if_not_installed("glyrepr")
  structures <- glyrepr::as_glycan_structure(c(
    "Man(a1-3)Man(b1-4)GlcNAc(b1-",
    "Man(a1-3)Man(b1-4)GlcNAc(b1-",
    "Gal(b1-4)GlcNAc(b1-"
  ))
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(result <- export_cartoons(structures, temp_dir))

  expect_length(result, 2)
  files <- fs::dir_ls(temp_dir, glob = "*.png")
  expect_length(files, 2)
})

test_that("export_cartoons does not support glyexp experiment input", {
  exp <- structure(list(), class = "glyexp_experiment")
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  expect_null(getS3method(
    "export_cartoons",
    "glyexp_experiment",
    optional = TRUE
  ))
  expect_error(
    export_cartoons(exp, temp_dir),
    "no applicable method"
  )
})

test_that("export_cartoons creates non-existent directory", {
  glycans <- "Man(a1-3)Man(b1-4)GlcNAc(b1-"
  non_existent_dir <- tempfile(pattern = "non_existent_dir_")
  on.exit(unlink(non_existent_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    result <- export_cartoons(glycans, non_existent_dir)
  )

  expect_length(result, 1)
  expect_true(fs::dir_exists(non_existent_dir))
  files <- fs::dir_ls(non_existent_dir, glob = "*.png")
  expect_length(files, 1)
})

test_that("export_cartoons works with jpeg extension", {
  glycans <- "Man(a1-3)Man(b1-4)GlcNAc(b1-"
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(
    result <- export_cartoons(glycans, temp_dir, file_ext = "jpg")
  )
  files <- fs::dir_ls(temp_dir, glob = "*.jpg")
  expect_length(files, 1)
})
