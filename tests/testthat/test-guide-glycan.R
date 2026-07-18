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
  expect_equal(
    purrr::map_chr(labels, ~ .x$children[[1]]$glydraw_vjust),
    rep(vjust_red_end(), length(labels))
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

test_that("guide_glycan accepts mapped glycan structure vectors", {
  structures <- glyrepr::as_glycan_structure(c(
    "GalNAc(a1-",
    "Gal(b1-3)GalNAc(a1-"
  ))
  data <- tibble::tibble(
    x = seq_along(structures),
    structure = structures,
    value = c(1, 2)
  )
  base_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$x,
      y = .data$value,
      fill = .data$structure
    )
  ) +
    ggplot2::geom_col()
  plots <- list(
    automatic = base_plot + ggplot2::guides(fill = guide_glycan()),
    explicit = base_plot +
      ggplot2::scale_fill_discrete(guide = guide_glycan())
  )

  purrr::walk(plots, function(plot) {
    legend <- .glycan_legend_table(plot)
    labels <- legend$grobs[grepl("^label-", legend$layout$name)]

    expect_length(labels, length(structures))
    expect_equal(
      sort(purrr::map_chr(labels, "glydraw_structure")),
      sort(as.character(structures))
    )
  })
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

test_that("guide_glycan anchors labels at their reducing ends by default", {
  structures <- c(
    paste0(
      "Man(??-?)[Man(??-?)]Man(??-?)[Man(??-?)]Man(??-?)",
      "GlcNAc(??-?)GlcNAc(??-"
    ),
    "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
  )
  data <- data.frame(structure = structures, value = c(1, 2))
  base_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$structure,
      y = .data$value,
      fill = .data$structure
    )
  ) +
    ggplot2::geom_col()
  vertical_plot <- base_plot +
    ggplot2::scale_fill_discrete(
      guide = guide_glycan(orient = "V")
    )
  horizontal_plot <- base_plot +
    ggplot2::scale_fill_discrete(
      guide = guide_glycan(orient = "H")
    )

  vertical_legend <- .glycan_legend_table(vertical_plot)
  horizontal_legend <- .glycan_legend_table(horizontal_plot)
  vertical_labels <- vertical_legend$grobs[
    grepl("^label-", vertical_legend$layout$name)
  ]
  horizontal_labels <- horizontal_legend$grobs[
    grepl("^label-", horizontal_legend$layout$name)
  ]
  vertical_grobs <- purrr::map(vertical_labels, ~ .x$children[[1]])
  horizontal_grobs <- purrr::map(horizontal_labels, ~ .x$children[[1]])

  expect_equal(
    unname(purrr::map_dbl(
      vertical_grobs,
      .reducing_end_displacement,
      "x"
    )),
    rep(0, length(structures))
  )
  expect_equal(
    unname(purrr::map_dbl(
      horizontal_grobs,
      .reducing_end_displacement,
      "y"
    )),
    rep(0, length(structures))
  )
})

test_that("guide_glycan includes reducing-end offsets in label heights", {
  structures <- c(
    paste0(
      "Man(??-?)[Man(??-?)]Man(??-?)[Man(??-?)]Man(??-?)",
      "GlcNAc(??-?)GlcNAc(??-"
    ),
    "Gal(b1-3)[Fuc(a1-4)]GlcNAc(b1-"
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
    ggplot2::scale_fill_discrete(guide = guide_glycan())

  legend <- .glycan_legend_table(plot)
  labels <- legend$grobs[grepl("^label-", legend$layout$name)]

  purrr::walk(labels, function(label) {
    glycan <- label$children[[1]]
    child <- glycan$children[[1]]
    cartoon <- .glycan_grob_to_plot(glycan)
    size <- attr(cartoon, "glydraw_size_px")
    offset <- .glycan_justification_offset(
      cartoon,
      size,
      glycan$glydraw_scale,
      glycan$glydraw_hjust,
      glycan$glydraw_vjust,
      glycan$reducing_end_coor
    )
    expected_height <- grid::grobHeight(child) +
      grid::unit(2 * abs(offset[["y"]]), "in")

    expect_gt(abs(offset[["y"]]), 0)
    expect_equal(
      grid::convertHeight(grid::grobHeight(label), "mm", valueOnly = TRUE),
      grid::convertHeight(expected_height, "mm", valueOnly = TRUE)
    )
    expect_equal(
      grid::convertHeight(glycan$vp$height, "mm", valueOnly = TRUE),
      grid::convertHeight(expected_height, "mm", valueOnly = TRUE)
    )
  })
})

test_that("guide_glycan reducing-end helpers require matching orientations", {
  expect_snapshot(
    error = TRUE,
    guide_glycan(orient = "H", hjust = hjust_red_end())
  )
  expect_snapshot(
    error = TRUE,
    guide_glycan(orient = "V", vjust = vjust_red_end())
  )
})

test_that("guide_glycan separates vertically stacked legend keys", {
  data <- tibble::tibble(
    x = 1:6,
    y = 1:6,
    glycan = rep(
      c(
        "GalNAc(a1-",
        "Gal(b1-3)GalNAc(a1-",
        "Gal(b1-3)[GlcNAc(b1-6)]GalNAc(a1-"
      ),
      each = 2
    )
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$x, y = .data$y, color = .data$glycan)
  ) +
    ggplot2::geom_point() +
    ggplot2::guides(color = guide_glycan())

  legend <- .glycan_legend_table(plot)
  key_rows <- legend$layout$t[grepl("^key-", legend$layout$name)]
  gap_rows <- setdiff(seq(min(key_rows), max(key_rows)), key_rows)
  gap_heights <- grid::convertHeight(
    legend$heights[gap_rows],
    "mm",
    valueOnly = TRUE
  )

  expect_length(gap_heights, length(key_rows) - 1)
  expect_true(all(gap_heights > 0))
  expect_equal(gap_heights, rep(gap_heights[[1]], length(gap_heights)))

  custom_legend <- .glycan_legend_table(
    plot + ggplot2::theme(legend.key.spacing.y = grid::unit(4, "mm"))
  )
  custom_key_rows <- custom_legend$layout$t[
    grepl("^key-", custom_legend$layout$name)
  ]
  custom_gap_rows <- setdiff(
    seq(min(custom_key_rows), max(custom_key_rows)),
    custom_key_rows
  )
  custom_gap_heights <- grid::convertHeight(
    custom_legend$heights[custom_gap_rows],
    "mm",
    valueOnly = TRUE
  )

  expect_equal(custom_gap_heights, rep(4, length(custom_gap_heights)))
})

test_that("guide_glycan returns a configured legend guide", {
  guide <- guide_glycan(
    reverse = TRUE,
    nrow = 1,
    size = 0.25,
    hjust = 0.25,
    vjust = 0.75,
    override.aes = list(color = "red", pch = 16)
  )

  expect_s3_class(guide, "GuideGlycan")
  expect_s3_class(guide, "GuideLegend")
  expect_true(guide$params$reverse)
  expect_equal(guide$params$nrow, 1)
  expect_equal(guide$params$glycan_size, 0.25)
  expect_equal(guide$params$glycan_hjust, 0.25)
  expect_equal(guide$params$glycan_vjust, 0.75)
  expect_named(guide$params$override.aes, c("colour", "shape"))
  expect_error(guide_glycan(size = 0), "larger than")
  expect_error(guide_glycan(orient = "diagonal"), "orient")
})
