.glycan_legend_table <- function(plot) {
  plot_grob <- ggplot2::ggplotGrob(plot)
  guide_box <- plot_grob$grobs[[
    match("guide-box-right", plot_grob$layout$name)
  ]]

  guide_box$grobs[[match("guides", guide_box$layout$name)]]
}

test_that("guide_glycan replaces legend text with glycan cartoons", {
  structures <- c(
    "Gal(b1-3)GalNAc(a1-",
    "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
  )
  data <- data.frame(structure = structures, value = c(1, 2))
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$structure,
      y = .data$value,
      fill = .data$structure
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_discrete(
      guide = guide_glycan(size = 0.3, show_linkage = FALSE)
    )

  legend <- .glycan_legend_table(plot)
  labels <- legend$grobs[grepl("^label-", legend$layout$name)]
  keys <- legend$grobs[grepl("^key-", legend$layout$name)]

  expect_length(labels, length(structures))
  purrr::walk(labels, expect_s3_class, "glycan_legend_label")
  expect_equal(
    purrr::map_chr(labels, "glydraw_structure"),
    structures
  )
  purrr::walk(
    labels,
    ~ expect_s3_class(.x$children[[1]], "glycanGrob")
  )
  expect_true(all(purrr::map_lgl(
    labels,
    ~ isTRUE(all.equal(.x$children[[1]]$glydraw_scale, 0.3))
  )))
  expect_false(any(purrr::map_lgl(
    labels,
    ~ .x$children[[1]]$show_linkage
  )))
  label_viewports <- purrr::map(labels, ~ .x$children[[1]]$vp)
  label_viewports_are_set <- purrr::map_lgl(label_viewports, Negate(is.null))
  expect_true(all(label_viewports_are_set))
  if (all(label_viewports_are_set)) {
    expect_equal(
      purrr::map_dbl(label_viewports, ~ .x$justification[[1]]),
      rep(0.5, length(labels))
    )
  }
  expect_equal(
    purrr::map_dbl(labels, ~ .x$children[[1]]$glydraw_hjust),
    rep(0, length(labels))
  )
  label_gaps <- purrr::map_dbl(labels, function(label) {
    child <- label$children[[1]]$children[[1]]
    grid::convertWidth(
      grid::grobWidth(label) - grid::grobWidth(child),
      "mm",
      valueOnly = TRUE
    )
  })
  expect_true(all(label_gaps > 0))
  expect_equal(label_gaps, rep(label_gaps[[1]], length(labels)))

  # The scale's ordinary fill keys remain visible beside the cartoons.
  expect_length(keys, length(structures))
  expect_true(all(purrr::map_int(keys, ~ length(.x$children)) >= 2))
  key_viewports_are_fixed <- purrr::map_lgl(keys, ~ !is.null(.x$vp))
  expect_true(all(key_viewports_are_fixed))
  if (all(key_viewports_are_fixed)) {
    key_widths <- purrr::map_dbl(
      keys,
      ~ grid::convertWidth(.x$vp$width, "mm", valueOnly = TRUE)
    )
    key_heights <- purrr::map_dbl(
      keys,
      ~ grid::convertHeight(.x$vp$height, "mm", valueOnly = TRUE)
    )

    expect_equal(key_widths, rep(key_widths[[1]], length(keys)))
    expect_equal(key_heights, rep(key_heights[[1]], length(keys)))
    expect_equal(key_widths, key_heights)
  }
  expect_no_error(ggplot2::ggplotGrob(plot))
})

test_that("guide_glycan draws scale labels as cartoons", {
  structures <- c(
    A = "Gal(b1-3)GalNAc(a1-",
    B = "Gal(b1-4)GlcNAc(b1-"
  )
  data <- data.frame(group = names(structures), value = c(1, 2))
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$group, y = .data$value, fill = .data$group)
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_discrete(
      labels = structures,
      guide = guide_glycan(orient = "V", colors = c(Gal = "#123456"))
    )

  legend <- .glycan_legend_table(plot)
  labels <- legend$grobs[grepl("^label-", legend$layout$name)]

  expect_equal(
    purrr::map_chr(labels, "glydraw_structure"),
    unname(structures)
  )
  expect_true(all(purrr::map_lgl(
    labels,
    ~ .x$children[[1]]$glydraw_orient == "V"
  )))
  expect_true(all(purrr::map_lgl(
    labels,
    ~ "#123456" %in% .x$children[[1]]$filled_color
  )))
})

test_that("guide_glycan returns a configured legend guide", {
  guide <- guide_glycan(reverse = TRUE, nrow = 1, size = 0.25)

  expect_s3_class(guide, "GuideGlycan")
  expect_s3_class(guide, "GuideLegend")
  expect_true(guide$params$reverse)
  expect_equal(guide$params$nrow, 1)
  expect_equal(guide$params$glycan_size, 0.25)
  expect_error(guide_glycan(size = 0), "larger than")
  expect_error(guide_glycan(orient = "diagonal"), "orient")
})

test_that("guide_glycan renders cartoon legend labels", {
  data <- data.frame(
    structure = c(
      "Gal(b1-3)GalNAc(a1-",
      "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
    ),
    value = c(1, 2)
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$structure,
      y = .data$value,
      fill = .data$structure
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_discrete(
      guide = guide_glycan(size = 0.3, show_linkage = FALSE)
    )

  vdiffr::expect_doppelganger("glycan legend labels", plot)
})
