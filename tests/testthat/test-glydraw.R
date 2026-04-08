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

test_that("export_cartoons works with glyexp_experiment input", {
  skip_if_not_installed("glyexp")
  skip_if_not_installed("glyrepr")
  exp <- glyexp::real_experiment |> glyexp::slice_head_var(n = 5)
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(result <- export_cartoons(exp, temp_dir, dpi = 72))

  expect_type(result, "list")
  files <- fs::dir_ls(temp_dir, glob = "*.png")
  expect_gt(length(files), 0)
})

test_that("export_cartoons errors for non-glycoproteomics experiment", {
  skip_if_not_installed("glyexp")
  exp <- glyexp::real_experiment |> glyexp::slice_head_var(n = 5)
  exp$meta_data$exp_type <- "proteomics"
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  expect_error(
    export_cartoons(exp, temp_dir),
    "can only be an experiment with"
  )
})

test_that("export_cartoons errors when glycan_structure column is missing", {
  skip_if_not_installed("glyexp")
  exp <- glyexp::real_experiment |> glyexp::slice_head_var(n = 5)
  exp$var_info$glycan_structure <- NULL
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  expect_error(
    export_cartoons(exp, temp_dir),
    "glycan_structure"
  )
})

test_that("export_cartoons errors for non-existent directory", {
  glycans <- "Man(a1-3)Man(b1-4)GlcNAc(b1-"
  non_existent_dir <- file.path(tempdir(), "non_existent_dir_12345")

  expect_error(
    export_cartoons(glycans, non_existent_dir),
    "Assertion on 'dirname' failed"
  )
})

test_that("export_cartoons works with jpeg extension", {
  glycans <- "Man(a1-3)Man(b1-4)GlcNAc(b1-"
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(result <- export_cartoons(glycans, temp_dir, file_ext = "jpg", dpi = 72))
  files <- fs::dir_ls(temp_dir, glob = "*.jpg")
  expect_length(files, 1)
})
