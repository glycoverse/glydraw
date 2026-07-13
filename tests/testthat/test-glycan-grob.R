test_that("glycanGrob constructs a drawable grid grob", {
  grob <- glycanGrob("Gal(b1-3)GalNAc(a1-")

  expect_s3_class(grob, "glycanGrob")
  expect_s3_class(grob, "gTree")
  expect_s3_class(grob, "grob")

  content <- grid::makeContent(grob)
  expect_length(content$children, 1)
  expect_s3_class(content$children[[1]], "gtable")
})

test_that("glycanGrob converts to the existing cartoon plot contract", {
  grob <- glycanGrob(
    "Gal(b1-4)GlcNAc(b1-",
    colors = c(Gal = "#123456"),
    edge_linewidth = 1.1,
    node_linewidth = 0.3
  )

  plot <- .glycan_grob_to_plot(grob)
  layers <- ggplot2::ggplot_build(plot)$data

  expect_s3_class(plot, "glydraw_cartoon")
  expect_s3_class(plot, "ggplot")
  expect_named(attr(plot, "glydraw_size_px"), c("width", "height"))
  expect_equal(unique(layers[[1]]$linewidth), 1.1)
  expect_equal(unique(layers[[3]]$linewidth), 0.3)
  expect_contains(unique(layers[[3]]$fill), "#123456")
})
