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