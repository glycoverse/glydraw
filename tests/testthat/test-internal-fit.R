test_that("panel edges fit independently of the perpendicular anchor", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::pushViewport(grid::viewport(
    width = grid::unit(2, "in"),
    height = grid::unit(2, "in")
  ))
  on.exit(grid::popViewport(), add = TRUE, after = FALSE)
  cases <- expand.grid(
    axis = c("x", "y"),
    inside = c(0.02, 0.98),
    perpendicular = c(-0.1, 0, 1, 1.1)
  )
  for (i in seq_len(nrow(cases))) {
    case <- cases[i, ]
    data <- data.frame(
      x = case$inside,
      y = case$perpendicular,
      structure = "Gal(b1-3)GalNAc(a1-"
    )
    if (case$axis == "y") {
      data[c("x", "y")] <- data[c("y", "x")]
    }
    plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, structure = structure)) +
      geom_glycan() +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)
    content <- grid::makeContent(ggplot2::layer_grob(plot)[[1]])
    bounds <- .glycan_bounds_inches(content$children[[1]])
    edges <- if (case$axis == "x") {
      bounds[c("left", "right")]
    } else {
      bounds[c("bottom", "top")]
    }
    expect_gte(edges[[1]] + case$inside * 2, 0)
    expect_lte(edges[[2]] + case$inside * 2, 2)
  }
})
