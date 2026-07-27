test_that("glycanGrob constructs a drawable grid grob", {
  grob <- glycanGrob("Gal(b1-3)GalNAc(a1-")

  expect_s3_class(grob, "glycanGrob")
  expect_s3_class(grob, "gTree")
  expect_s3_class(grob, "grob")

  content <- grid::makeContent(grob)
  expect_length(content$children, 1)
  expect_s3_class(content$children[[1]], "glycan_grid_grob")
  expect_s3_class(content$children[[1]], "gTree")
  primitives <- content$children[[1]]$children[[2]]$children
  expect_named(
    primitives,
    c(
      "glycan.edges",
      "glycan.node.mask",
      "glycan.node",
      "glycan.annotations"
    )
  )
})

test_that("glycanGrob converts to the existing cartoon plot contract", {
  grob <- glycanGrob(
    "Gal(b1-4)GlcNAc(b1-",
    style = glydraw_style(
      colors = c(Gal = "#123456"),
      edge_linewidth = 1.1,
      node_linewidth = 0.3
    )
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

test_that("glycanGrob controls the text annotation font family", {
  grob <- glycanGrob(
    "Gal(b1-3)GalNAc(a1-",
    style = glydraw_style(font_family = "serif")
  )
  content <- grid::makeContent(grob)
  annotations <-
    content$children[[1]]$children[[2]]$children[["glycan.annotations"]]
  label <- as.list(annotations$label)
  label_text <- vapply(label, as.character, character(1))
  greek_label <- label[label_text %in% c("\u03b1", "\u03b2")]

  expect_equal(unique(annotations$gp$fontfamily), "serif")
  expect_setequal(
    label_text[label_text %in% c("\u03b1", "\u03b2")],
    c("\u03b1", "\u03b2")
  )
  expect_equal(
    vapply(greek_label, typeof, character(1)),
    c("character", "character")
  )
})

test_that("native grid layout preserves the cartoon plot geometry", {
  cases <- list(
    list(
      structure = "Gal(b1-3)GalNAc(a1-",
      orient = "left",
      red_end = "",
      show_linkage = TRUE
    ),
    list(
      structure = "Gal(b1-3)[Fuc(a1-4)]GlcNAc(b1-",
      orient = "up",
      red_end = "~",
      show_linkage = TRUE
    ),
    list(
      structure = "Gal6S(b1-4)GlcNAc(b1-",
      orient = "left",
      red_end = "Ser/Thr",
      show_linkage = FALSE
    )
  )

  purrr::walk(cases, function(case) {
    grob <- glycanGrob(
      case$structure,
      orient = case$orient,
      show_linkage = case$show_linkage,
      style = glydraw_style(red_end = case$red_end)
    )
    grob$glydraw_border_px <- 0
    grob$glydraw_background <- FALSE
    layout <- .cartoon_grid_layout(grob)
    cartoon <- .glycan_grob_to_plot(grob)
    built <- ggplot2::ggplot_build(cartoon)

    expect_equal(
      layout$data_ranges$x,
      built$layout$panel_scales_x[[1]]$range$range
    )
    expect_equal(
      layout$data_ranges$y,
      built$layout$panel_scales_y[[1]]$range$range
    )
    expect_equal(
      layout$panel_ranges$x,
      built$layout$panel_params[[1]]$x.range
    )
    expect_equal(
      layout$panel_ranges$y,
      built$layout$panel_params[[1]]$y.range
    )
    expect_equal(layout$size_px, attr(cartoon, "glydraw_size_px"))
  })
})
