test_that("linear layout keeps polysaccharide backbones straight", {
  cases <- c(
    "Glc(b1-4)[Xyl(a1-6)][Xyl(a1-2)]Glc(b1-4)[Xyl(a1-6)]Glc(b1-4)[Fuc(a1-2)Gal(b1-2)Xyl(a1-6)]Glc(b1-4)Glc(b1-4)[Xyl(a1-6)]Glc(b1-",
    "Xyl(b1-4)[Ara(a1-3)]Xyl(b1-4)[Ara(a1-3)]Xyl(b1-4)Xyl(b1-4)Xyl(b1-4)Xyl(b1-",
    "Glc(b1-4)Man(b1-4)[Gal(a1-6)]Man(b1-4)Glc(b1-4)Man(b1-4)Glc(b1-"
  )
  for (glycan in cases) {
    input <- .prepare_cartoon_inputs(glycan, NULL, layout = "linear")
    graph <- input$structure
    coor <- input$coor
    backbone <- igraph::V(graph)$mono %in% c("Glc", "Man")
    if (grepl("^Xyl", glycan)) {
      backbone <- igraph::V(graph)$mono == "Xyl"
    }
    expect_equal(unname(coor[backbone, "y"]), rep(0, sum(backbone)))
    edges <- igraph::as_edgelist(graph, names = FALSE)
    delta <- coor[edges[, 1], , drop = FALSE] - coor[edges[, 2], , drop = FALSE]
    expect_equal(unname(rowSums(delta^2)), rep(1, nrow(edges)))
    expect_equal(anyDuplicated(as.data.frame(coor)), 0L)
    for (orient in c("left", "right", "up", "down")) {
      grob <- glycanGrob(
        glycan,
        orient = orient,
        style = style_glydraw(layout = "linear")
      )
      expect_s3_class(grid::makeContent(grob), "gTree")
    }
  }
})

test_that("linear layout handles nested branches and single residues", {
  for (glycan in c(
    "Xyl",
    "Gal(b1-2)[Gal(b1-3)]Xyl(a1-6)[Ara(a1-2)]Glc(b1-4)Glc"
  )) {
    input <- .prepare_cartoon_inputs(glycan, NULL, layout = "linear")
    coor <- input$coor
    expect_equal(all(is.finite(coor)), TRUE)
    expect_equal(anyDuplicated(as.data.frame(coor)), 0L)
    edges <- igraph::as_edgelist(input$structure, names = FALSE)
    delta <- coor[edges[, 1], , drop = FALSE] - coor[edges[, 2], , drop = FALSE]
    expect_equal(unname(delta[, 1] * delta[, 2]), rep(0, nrow(edges)))
  }
})

test_that("style presets preserve SNFG by default and accept linear", {
  glycan <- "Xyl(b1-4)[Ara(a1-3)]Xyl(b1-4)Xyl"
  for (constructor in list(
    style_glydraw,
    style_glygen,
    style_snfg,
    style_glycoworkbench
  )) {
    expect_identical(constructor()$layout, "SNFG")
    expect_identical(constructor(layout = "linear")$layout, "linear")
  }
  expect_equal(
    glycanGrob(glycan)$connect_df,
    glycanGrob(glycan, style = style_glydraw(layout = "SNFG"))$connect_df
  )
  expect_snapshot(error = TRUE, style_glydraw(layout = "invalid"))
})

test_that("linear style reaches ggplot layers, guides, and scales", {
  glycan <- "Xyl(b1-4)[Ara(a1-3)]Xyl(b1-4)Xyl"
  style <- style_glydraw(layout = "linear")
  layer <- geom_glycan(style = style)
  expect_identical(layer$geom_params$layout, "linear")
  expect_identical(guide_glycan(style = style)$params$glycan_layout, "linear")
  scale <- scale_x_glycan(style = style)
  expect_identical(scale$guide$params$glycan_layout, "linear")
  plot <- ggplot2::ggplot(
    data.frame(x = 1, y = 1, glycan = glycan),
    ggplot2::aes(x, y, structure = glycan)
  ) +
    layer
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
})

test_that("high-degree linear branches do not draw links through residues", {
  cases <- c(
    "Gal(b1-2)[Gal(b1-3)][Gal(b1-4)][Gal(b1-6)]Glc",
    "Gal(b1-2)[Gal(b1-3)][Gal(b1-4)][Gal(b1-6)][Gal(b1-?)]Glc",
    "Gal(b1-2)[Gal(b1-3)][Gal(b1-4)][Gal(b1-6)]Glc(b1-4)Glc",
    "Gal(b1-2)Gal(b1-2)[Gal(b1-3)Gal(b1-3)][Gal(b1-4)][Gal(b1-6)]Glc",
    "Gal(b1-4)Gal(b1-4)Gal(b1-4)[Gal(b1-2)[Gal(b1-3)][Gal(b1-4)][Gal(b1-6)]Glc(a1-6)]Glc"
  )
  for (glycan in cases) {
    for (orient in c("left", "right", "up", "down")) {
      input <- .prepare_cartoon_inputs(glycan, NULL, orient, layout = "linear")
      coor <- input$coor
      edges <- igraph::as_edgelist(input$structure, names = FALSE)
      expect_equal(all(is.finite(coor)), TRUE)
      expect_equal(anyDuplicated(as.data.frame(coor)), 0L)
      for (i in seq_len(nrow(edges))) {
        ends <- edges[i, ]
        start <- coor[ends[1], ]
        delta <- coor[ends[2], ] - start
        other <- coor[-ends, , drop = FALSE]
        relative <- sweep(other, 2, start)
        projection <- pmax(
          0,
          pmin(1, as.vector(relative %*% delta) / sum(delta^2))
        )
        distance <- sqrt(rowSums((relative - projection %o% delta)^2))
        expect_gt(min(distance), .default_node_point_size)
      }
    }
  }
})
