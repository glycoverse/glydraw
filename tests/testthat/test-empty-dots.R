test_that("drawing entry points reject dots in their own call", {
  for (name in c(
    "draw_cartoon",
    "draw_cartoons",
    "draw_cartoon_sketch",
    "glycanGrob"
  )) {
    # Check before parsing or rendering, without evaluating the unused promise.
    call <- call(name, "invalid glycan", node_size = quote(stop("forced")))
    error <- tryCatch(eval(call), error = identity)
    expect_s3_class(error, "rlib_error_dots_nonempty")
    expect_identical(as.character(error$call[[1]]), name)
    expect_match(conditionMessage(error), "node_size")
    expect_error(
      eval(call(name, "invalid glycan", FALSE)),
      class = "rlib_error_dots_nonempty"
    )
  }
})

test_that("save and print reject unused dots before side effects", {
  file <- tempfile(fileext = ".png")
  expect_error(
    save_cartoon(NULL, file, width = 100),
    class = "rlib_error_dots_nonempty"
  )
  expect_false(file.exists(file))
  expect_error(
    print.glydraw_cartoon(NULL, typo = TRUE),
    class = "rlib_error_dots_nonempty"
  )
})

test_that("export methods reject unused dots before creating directories", {
  glycan <- "Gal(b1-3)GalNAc(a1-"
  inputs <- list(glycan, glyrepr::as_glycan_structure(glycan))
  for (x in inputs) {
    directory <- tempfile()
    expect_error(
      export_cartoons(x, directory, width = 100),
      class = "rlib_error_dots_nonempty"
    )
    expect_false(dir.exists(directory))
  }
  # Direct method calls must be protected too; the generic need not double-check.
  for (method in list(
    export_cartoons.character,
    export_cartoons.glyrepr_structure
  )) {
    expect_error(
      method(NULL, tempfile(), typo = TRUE),
      class = "rlib_error_dots_nonempty"
    )
  }
})

test_that("empty dots preserve drawing and compatibility methods", {
  glycan <- "Gal(b1-3)GalNAc(a1-"
  x <- glyrepr::as_glycan_structure(glycan)
  expect_identical(droplevels(x), x)
  expect_error(droplevels(x, typo = TRUE), class = "rlib_error_dots_nonempty")
  expect_s3_class(
    draw_cartoon(glycan, style = style_glydraw(node_size = 1.2)),
    "glydraw_cartoon"
  )
  expect_s3_class(draw_cartoons(glycan, ncol = 1), "glydraw_cartoon")
  expect_s3_class(glycanGrob(glycan, orient = "right"), "glycanGrob")
})

test_that("useful dots still reach ggplot2 layers and scales", {
  expect_identical(geom_glycan(alpha = 0.5)$aes_params$alpha, 0.5)
  expect_identical(geom_node_glycan(alpha = 0.5)$aes_params$alpha, 0.5)
  expect_identical(scale_x_glycan(name = "Glycans")$name, "Glycans")
  expect_identical(scale_y_glycan(name = "Glycans")$name, "Glycans")
})
