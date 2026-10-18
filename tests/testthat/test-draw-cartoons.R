test_that("grid dimensions are inferred and input order is retained", {
  x <- c("Gal(b1-3)GalNAc(a1-", "Man(a1-3)Man(b1-", "Gal(b1-3)GalNAc(a1-")
  p <- draw_cartoons(x)
  expect_s3_class(p, "glydraw_cartoon")
  expect_length(p$layers, 3)
  expect_equal(p$coordinates$limits, list(x = c(0, 2), y = c(0, 2)))
  expect_equal(
    p$layers[[1]]$geom_params$grob$polygon_coor,
    p$layers[[3]]$geom_params$grob$polygon_coor
  )
  expect_equal(
    draw_cartoons(x, nrow = 1)$coordinates$limits,
    list(x = c(0, 3), y = c(0, 1))
  )
  expect_equal(
    draw_cartoons(x, ncol = 1)$coordinates$limits,
    list(x = c(0, 1), y = c(0, 3))
  )
  expect_equal(
    draw_cartoons(x, nrow = 2, ncol = 3)$coordinates$limits,
    list(x = c(0, 3), y = c(0, 2))
  )
})

test_that("structure vectors and drawing arguments work in grids", {
  x <- glyrepr::as_glycan_structure(c(
    "Gal(b1-3)GalNAc(a1-",
    "Man(a1-3)Man(b1-"
  ))
  style <- style_glydraw(font_family = "serif")
  p <- draw_cartoons(
    x,
    orient = "up",
    show_linkage = FALSE,
    highlight = 1,
    style = style,
    red_end = "test"
  )
  expected <- glycanGrob(
    x[[1]],
    orient = "up",
    show_linkage = FALSE,
    highlight = 1,
    style = style,
    red_end = "test"
  )
  expect_equal(
    p$layers[[1]]$geom_params$grob$polygon_coor,
    expected$polygon_coor
  )
  expect_equal(
    p$layers[[1]]$geom_params$grob$annotation_data,
    expected$annotation_data
  )
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path))
  save_cartoon(p, path, scale = 0.5)
  expect_equal(
    dim(png::readPNG(path))[1:2],
    as.integer(rev(attr(p, "glydraw_size_px")) * 0.5)
  )
})

test_that("grid inputs and capacity are validated", {
  expect_snapshot(error = TRUE, draw_cartoons(character()))
  expect_snapshot(error = TRUE, draw_cartoons(rep("Gal", 37)))
  expect_snapshot(
    error = TRUE,
    draw_cartoons(c("Gal", "Man"), nrow = 1, ncol = 1)
  )
  expect_snapshot(error = TRUE, draw_cartoons("Gal", nrow = 0))
  expect_snapshot(error = TRUE, draw_cartoons("Gal", ncol = 1.5))
  expect_snapshot(error = TRUE, draw_cartoons("Gal", max_n = NA))
  expect_snapshot(error = TRUE, draw_cartoons(1))
  expect_length(draw_cartoons(rep("Gal", 37), max_n = 37)$layers, 37)
})
