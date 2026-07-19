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

test_that("save_cartoon saves file correctly", {
  structure <- "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  cartoon <- draw_cartoon(structure)

  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file, recursive = FALSE), add = TRUE)

  expect_no_error(save_cartoon(cartoon, temp_file))
  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)
})
