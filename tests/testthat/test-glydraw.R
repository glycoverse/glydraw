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

test_that("draw_cartoon works with vertical orientation", {
  structure <- "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"

  v_plot <- draw_cartoon(structure, orient = "V")
  expect_s3_class(v_plot, "glydraw_cartoon")
  expect_s3_class(v_plot, "ggplot")
})

test_that("draw_cartoon orients reducing end annotation line by orientation", {
  structure <- "Gal(b1-3)GalNAc(a1-"

  h_plot <- draw_cartoon(structure, orient = "H")
  h_segments <- ggplot2::ggplot_build(h_plot)$data[[1]]
  h_reducing <- tail(h_segments, 1)
  expect_equal(h_reducing$x, 0, tolerance = 1e-6)
  expect_equal(h_reducing$y, 0, tolerance = 1e-6)
  expect_equal(h_reducing$xend, 0.6, tolerance = 1e-6)
  expect_equal(h_reducing$yend, 0, tolerance = 1e-6)

  v_plot <- draw_cartoon(structure, orient = "V")
  v_segments <- ggplot2::ggplot_build(v_plot)$data[[1]]
  v_reducing <- tail(v_segments, 1)
  expect_equal(v_reducing$x, 0, tolerance = 1e-6)
  expect_equal(v_reducing$y, 0, tolerance = 1e-6)
  expect_equal(v_reducing$xend, 0, tolerance = 1e-6)
  expect_equal(v_reducing$yend, -0.6, tolerance = 1e-6)
})

test_that("draw_cartoon uses custom reducing-end text", {
  structure <- "Gal(b1-3)GalNAc(a1-"

  plot <- draw_cartoon(structure, red_end = "Ser/Thr")
  text <- ggplot2::ggplot_build(plot)$data[[4]]
  red_end <- text[text$label == '"Ser/Thr"', ]
  x_range <- ggplot2::get_panel_scales(plot)$x$range$range

  expect_equal(nrow(red_end), 1)
  expect_gt(red_end$x, 0.6)
  expect_equal(red_end$y, 0, tolerance = 1e-6)
  expect_equal(red_end$hjust, 0)
  expect_gt(max(x_range), red_end$x + 0.5)
})

test_that("draw_cartoon keeps custom reducing-end text with linkage hidden", {
  structure <- "Gal(b1-3)GalNAc(a1-"

  plot <- draw_cartoon(structure, show_linkage = FALSE, red_end = "Ser/Thr")
  text_layers <- purrr::keep(
    ggplot2::ggplot_build(plot)$data,
    ~ "label" %in% names(.x)
  )
  text <- dplyr::bind_rows(text_layers)

  expect_true(any(text$label == '"Ser/Thr"'))
  expect_false(any(text$label == "beta"))
  expect_false(any(text$label == "3"))
})

test_that("draw_cartoon centers vertical reducing-end text", {
  structure <- "Gal(b1-3)GalNAc(a1-"

  plot <- draw_cartoon(structure, orient = "V", red_end = "Ser/Thr")
  text <- ggplot2::ggplot_build(plot)$data[[4]]
  red_end <- text[text$label == '"Ser/Thr"', ]
  x_range <- ggplot2::get_panel_scales(plot)$x$range$range

  expect_equal(nrow(red_end), 1)
  expect_equal(red_end$hjust, 0.5)
  expect_equal(red_end$vjust, 1)
  expect_lt(min(x_range), red_end$x - 0.3)
  expect_gt(max(x_range), red_end$x + 0.3)
  expect_lt(min(ggplot2::get_panel_scales(plot)$y$range$range), red_end$y - 0.3)
})

test_that("draw_cartoon adds a reducing-end wave for tilde", {
  structure <- "Gal(b1-3)GalNAc(a1-"

  plot <- draw_cartoon(structure, red_end = "~")
  plot_build <- ggplot2::ggplot_build(plot)
  wave <- plot_build$data[[5]]
  text <- plot_build$data[[4]]

  expect_true(all(c("x", "y") %in% names(wave)))
  expect_gt(nrow(wave), 4)
  expect_gt(max(wave$x), 0.6)
  expect_lt(min(wave$x), max(wave$x))
  expect_lt(min(wave$y), 0)
  expect_gt(max(wave$y), 0)
  expect_lt(diff(range(wave$y)), 0.5)
  expect_lt(diff(range(wave$x)), 0.12)
  expect_false(any(text$label == '"~"'))
})

test_that("draw_cartoon works with linkage hidden", {
  structure <- "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"

  plot_no_linkage <- draw_cartoon(structure, show_linkage = FALSE)
  expect_s3_class(plot_no_linkage, "glydraw_cartoon")
})

test_that("draw_cartoon places substituent annotations by orientation", {
  structure <- "GalNAc6S(b1-3)GalNAc(a1-"

  h_plot <- draw_cartoon(structure, orient = "H")
  h_text <- ggplot2::ggplot_build(h_plot)$data[[4]]
  h_sub <- h_text[grepl("6S", h_text$label), ]

  expect_equal(nrow(h_sub), 1)
  if (nrow(h_sub) == 1) {
    expect_equal(h_sub$size, unique(h_text$size))
    expect_equal(h_sub$x, -1, tolerance = 1e-6)
    expect_gt(h_sub$y, 0)
  }

  v_plot <- draw_cartoon(structure, orient = "V")
  v_text <- ggplot2::ggplot_build(v_plot)$data[[4]]
  v_sub <- v_text[grepl("6S", v_text$label), ]

  expect_equal(nrow(v_sub), 1)
  if (nrow(v_sub) == 1) {
    expect_equal(v_sub$size, unique(v_text$size))
    expect_gt(v_sub$x, 0)
    expect_equal(v_sub$y, 1, tolerance = 1e-6)
  }
})

test_that("draw_cartoon ignores unknown substituent linkage in annotation", {
  structure <- "GalNAc?S(b1-3)GalNAc(a1-"

  plot <- draw_cartoon(structure)
  labels <- ggplot2::ggplot_build(plot)$data[[4]]$label

  expect_true(any(labels == "S"))
  expect_false(any(grepl("?", labels, fixed = TRUE)))
})

test_that("draw_cartoon separates crowded branch linkage annotations", {
  structure <- "Fuc(a1-2)Gal(b1-3)GlcNAc(b1-3)[Neu5Ac(a2-3)Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-6)]GalNAc(?1-"

  plot <- draw_cartoon(structure)
  annotation <- ggplot2::ggplot_build(plot)$data[[4]]
  linkage_annotation <- annotation[annotation$label != '~"?"', ]
  pair_distance <- stats::dist(linkage_annotation[, c("x", "y")])
  branch_beta <- annotation[
    annotation$label == "beta" & annotation$x > -1 & annotation$y > 0.5,
  ]
  branch_six <- annotation[annotation$label == "6", ]

  expect_gt(min(pair_distance), 0.18)
  expect_equal(nrow(branch_beta), 1)
  if (nrow(branch_beta) == 1) {
    expect_gt(branch_beta$y, 0.75)
  }
  expect_equal(nrow(branch_six), 1)
  if (nrow(branch_six) == 1) {
    expect_gt(branch_six$y, 0.3)
  }
})

test_that("draw_cartoon works with reducing-end O-Fuc glycans", {
  glycans <- c(
    "Fuc(a1-",
    "GlcNAc(b1-3)Fuc(a1-"
  )

  cartoons <- purrr::map(glycans, draw_cartoon)

  purrr::walk(cartoons, expect_s3_class, "glydraw_cartoon")
})

test_that("reducing-end Fuc keeps the regular Fuc orientation", {
  structure <- .as_single_glycan_structure("GlcNAc(b1-3)Fuc(a1-")
  graph <- glyrepr::get_structure_graphs(structure, return_list = FALSE)

  expect_equal(.residue_glycoforms(graph), c("GlcNAc", "Fuc"))
  expect_equal(unname(.calculate_residue_coordinates(graph)[2, "x"]), 0)
})

test_that("draw_cartoon places a1-6 core Fuc up and a1-3 core Fuc down", {
  structure <- "Fuc(a1-3)[Fuc(a1-6)]GlcNAc(b1-4)GlcNAc(b1-"

  plot <- draw_cartoon(structure)
  segments <- ggplot2::ggplot_build(plot)$data[[1]]
  fuc_segments <- segments[segments$x == segments$xend, ]

  expect_equal(nrow(fuc_segments), 2)
  expect_gt(max(fuc_segments$yend), 0.5)
  expect_lt(min(fuc_segments$yend), -0.5)
})

test_that(".calculate_residue_coordinates keeps same-column residues at least one unit apart", {
  structure <- .as_single_glycan_structure(
    "Fuc(a1-3)[Fuc(a1-6)]GlcNAc(b1-4)GlcNAc(b1-"
  )
  graph <- glyrepr::get_structure_graphs(structure, return_list = FALSE)
  coor <- .calculate_residue_coordinates(graph)
  same_column <- which(coor[, "x"] == -1)
  same_column_y <- sort(coor[same_column, "y"])

  expect_equal(diff(same_column_y), rep(1, length(same_column_y) - 1))
})

test_that("draw_cartoon keeps elongated Fuc branches together", {
  structure <- .as_single_glycan_structure(
    paste0(
      "WURCS=2.0/6,7,6/",
      "[a2122h-1a_1-5_2*NCC/3=O][a1221m-1a_1-5]",
      "[a2122h-1b_1-5_2*NCC/3=O][a2112h-1b_1-5]",
      "[a1122h-1b_1-5][a1122h-1a_1-5]/",
      "1-2-3-2-4-5-6/",
      "a3-b1_a4-c1_c3-d1_c4-f1_d4-e1_f3-g1"
    )
  )
  graph <- glyrepr::get_structure_graphs(structure, return_list = FALSE)
  coor <- .calculate_residue_coordinates(graph)

  expect_equal(igraph::V(graph)$mono[c(1, 2, 3)], c("Gal", "Fuc", "Man"))
  expect_equal(unname(coor[1, "x"]), unname(coor[2, "x"]))
  expect_lt(unname(coor[1, "y"]), unname(coor[2, "y"]))
  expect_false(identical(unname(coor[1, ]), unname(coor[3, ])))
})

test_that("draw_cartoon avoids widening nested branches next to leaf siblings", {
  short_structure <- .as_single_glycan_structure(
    "Man(a1-3)[Man(a1-6)]Man(a1-6)[Man(a1-3)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  )
  short_graph <- glyrepr::get_structure_graphs(
    short_structure,
    return_list = FALSE
  )
  short_coor <- .calculate_residue_coordinates(short_graph)

  terminal_distance <- abs(short_coor[1, "y"] - short_coor[2, "y"])
  branch_distance <- abs(short_coor[3, "y"] - short_coor[4, "y"])
  expect_equal(branch_distance, terminal_distance)

  elongated_structure <- .as_single_glycan_structure(
    "Man(a1-3)[Man(a1-6)]Man(a1-6)[Man(a1-2)Man(a1-3)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  )
  elongated_graph <- glyrepr::get_structure_graphs(
    elongated_structure,
    return_list = FALSE
  )
  elongated_coor <- .calculate_residue_coordinates(elongated_graph)
  outer_man <- which(
    igraph::V(elongated_graph)$mono == "Man" &
      elongated_coor[, "x"] == min(elongated_coor[, "x"])
  )
  outer_y <- sort(elongated_coor[outer_man, "y"])

  expect_equal(diff(outer_y), rep(1, 2))
})

test_that("draw_cartoon keeps elongated and leaf sibling branches evenly spaced", {
  structure <- .as_single_glycan_structure(
    "Neu5Ac(a2-3)Gal(b1-3)[Gal(b1-3)GlcNAc(b1-3)[Gal(b1-4)GlcNAc(b1-6)]Gal(b1-4)GlcNAc(b1-6)]GalNAc(a1-"
  )
  graph <- glyrepr::get_structure_graphs(structure, return_list = FALSE)
  coor <- .calculate_residue_coordinates(graph)
  same_depth <- which(coor[, "x"] == -2)
  neu5ac <- same_depth[igraph::V(graph)$mono[same_depth] == "Neu5Ac"]
  rightmost_gal <- same_depth[igraph::V(graph)$mono[same_depth] == "Gal"]

  expect_equal(length(neu5ac), 1)
  expect_equal(length(rightmost_gal), 1)
  expect_equal(
    unname(abs(coor[neu5ac, "y"] - coor[rightmost_gal, "y"])),
    1
  )
})

test_that("draw_cartoon keeps same-depth Man sibling branches evenly spaced", {
  structure <- .as_single_glycan_structure(
    "Man(a1-2)Man(a1-3)[Man(a1-3)[Man(a1-2)Man(a1-6)]Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  )
  graph <- glyrepr::get_structure_graphs(structure, return_list = FALSE)
  coor <- .calculate_residue_coordinates(graph)
  same_depth_man <- which(
    igraph::V(graph)$mono == "Man" &
      coor[, "x"] == -4
  )
  same_depth_y <- sort(coor[same_depth_man, "y"])

  expect_equal(length(same_depth_man), 3)
  expect_equal(diff(same_depth_y), rep(1, 2))
})

test_that("save_cartoon saves file correctly", {
  structure <- "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  cartoon <- draw_cartoon(structure)

  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file, recursive = FALSE), add = TRUE)

  expect_no_error(save_cartoon(cartoon, temp_file, dpi = 72))
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

  suppressMessages(result <- export_cartoons(glycans, temp_dir, dpi = 72))

  expect_type(result, "list")
  expect_length(result, 2)
  files <- fs::dir_ls(temp_dir, glob = "*.png")
  expect_length(files, 2)
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
      dpi = 72,
      edge_linewidth = 1.1,
      node_linewidth = 0.3
    )
  )
  layers <- ggplot2::ggplot_build(result[[1]])$data

  expect_equal(unique(layers[[1]]$linewidth), 1.1)
  expect_equal(unique(layers[[3]]$linewidth), 0.3)
})

test_that("export_cartoons uses character vector names as filenames", {
  glycans <- c(
    core = "Man(a1-3)Man(b1-4)GlcNAc(b1-",
    antenna = "Gal(b1-4)GlcNAc(b1-"
  )
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(result <- export_cartoons(glycans, temp_dir, dpi = 72))

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

  suppressMessages(result <- export_cartoons(glycans, temp_dir, dpi = 72))

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

  suppressMessages(result <- export_cartoons(glycans, temp_dir, dpi = 72))

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

  suppressMessages(result <- export_cartoons(structures, temp_dir, dpi = 72))

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

  suppressMessages(result <- export_cartoons(structures, temp_dir, dpi = 72))

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

  suppressMessages(result <- export_cartoons(structures, temp_dir, dpi = 72))

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
    result <- export_cartoons(glycans, non_existent_dir, dpi = 72)
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
    result <- export_cartoons(glycans, temp_dir, file_ext = "jpg", dpi = 72)
  )
  files <- fs::dir_ls(temp_dir, glob = "*.jpg")
  expect_length(files, 1)
})
