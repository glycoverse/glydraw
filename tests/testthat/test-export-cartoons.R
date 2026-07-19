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
      style = glydraw_style(edge_linewidth = 1.1, node_linewidth = 0.3)
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
      style = glydraw_style(colors = c(Gal = "#123456"))
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
