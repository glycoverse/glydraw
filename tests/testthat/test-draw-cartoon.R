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

test_that("draw_cartoon uses ggplot2 fixed panel sizing", {
  structure <- "Gal(b1-3)GalNAc(a1-"

  plot <- draw_cartoon(structure)

  expect_s3_class(plot, "glydraw_cartoon")
  expect_false(inherits(plot, "ggview"))
  expect_s3_class(plot$theme$panel.widths, "unit")
  expect_s3_class(plot$theme$panel.heights, "unit")
  expect_named(attr(plot, "glydraw_size_px"), c("width", "height"))
})

test_that("draw_cartoon controls edge and node linewidths", {
  structure <- "Gal(b1-3)GalNAc(a1-"

  default_plot <- draw_cartoon(structure)
  default_layers <- ggplot2::ggplot_build(default_plot)$data

  expect_equal(unique(default_layers[[1]]$linewidth), 0.8)
  expect_equal(unique(default_layers[[2]]$linewidth), 0.8)
  expect_equal(unique(default_layers[[3]]$linewidth), 0.8)

  custom_plot <- draw_cartoon(
    structure,
    style = style_glydraw(
      red_end = "~",
      edge_linewidth = 1.2,
      node_linewidth = 0.4
    )
  )
  custom_layers <- ggplot2::ggplot_build(custom_plot)$data

  expect_equal(unique(custom_layers[[1]]$linewidth), 1.2)
  expect_equal(unique(custom_layers[[2]]$linewidth), 0.4)
  expect_equal(unique(custom_layers[[3]]$linewidth), 0.4)
  expect_equal(unique(custom_layers[[5]]$linewidth), 1.2)
})

test_that("draw_cartoon controls the text annotation font family", {
  plot <- draw_cartoon(
    "Gal(b1-3)GalNAc(a1-",
    style = style_glydraw(font_family = "serif")
  )
  layers <- ggplot2::ggplot_build(plot)$data

  expect_equal(unique(layers[[4]]$family), "serif")
  expect_contains(layers[[4]]$label, '"\u03b1"')
  expect_contains(layers[[4]]$label, '"\u03b2"')
  expect_equal(attr(plot, "glydraw_font_family"), "serif")
})

test_that("Greek anomer annotations use the selected text family", {
  annotation <- data.frame(
    annot = c("alpha", "beta", "1"),
    hjust = NA_real_,
    vjust = NA_real_,
    is_red_end_text = FALSE
  )

  prepared <- .prepare_plotmath_annotations(annotation)
  family_labels <- .font_family_annotation_labels(prepared, "serif")
  parsed <- parse(text = family_labels)

  expect_equal(
    family_labels,
    c('"\u03b1"', '"\u03b2"', "1")
  )
  expect_equal(
    vapply(parsed[1:2], typeof, character(1)),
    c("character", "character")
  )
  expect_equal(
    .font_family_annotation_labels(prepared, ""),
    c("alpha", "beta", "1")
  )
})

test_that("beta annotations are nudged perpendicular to linkage lines", {
  purrr::walk(c("left", "right", "up", "down"), function(orient) {
    beta_inputs <- .prepare_cartoon_inputs(
      "Gal(b1-3)GalNAc(a1-",
      NULL,
      orient,
      ""
    )
    alpha_inputs <- .prepare_cartoon_inputs(
      "Gal(a1-3)GalNAc(a1-",
      NULL,
      orient,
      ""
    )
    beta <- .linkage_annotation_data(
      beta_inputs$structure,
      beta_inputs$coor,
      orient = orient
    )[1, ]
    alpha <- .linkage_annotation_data(
      alpha_inputs$structure,
      alpha_inputs$coor,
      orient = orient
    )[1, ]
    direction <- c(
      beta$segment_end_x - beta$segment_start_x,
      beta$segment_end_y - beta$segment_start_y
    )
    nudge <- c(beta$x - alpha$x, beta$y - alpha$y)
    clockwise_normal <- c(direction[[2]], -direction[[1]]) /
      sqrt(sum(direction^2))
    beta_offset <- c(
      beta$x - beta$segment_start_x,
      beta$y - beta$segment_start_y
    )
    alpha_offset <- c(
      alpha$x - alpha$segment_start_x,
      alpha$y - alpha$segment_start_y
    )
    expected_nudge <- .beta_perpendicular_nudge_for_linkage(
      "b1",
      beta$segment_start_x,
      beta$segment_end_x
    )

    expect_equal(sum(nudge * direction), 0, tolerance = 1e-12)
    expect_equal(sqrt(sum(nudge^2)), expected_nudge)
    if (expected_nudge > 0) {
      expect_gt(
        sum(beta_offset * clockwise_normal),
        sum(alpha_offset * clockwise_normal)
      )
    } else {
      expect_equal(beta_offset, alpha_offset)
    }
  })

  skewed <- .linkage_label_positions(0, 0, 1, 0.5)
  skewed_nudged <- .linkage_label_positions(
    0,
    0,
    1,
    0.5,
    chil_perpendicular_nudge = .beta_perpendicular_nudge_for_linkage(
      "b1",
      0,
      1
    )
  )
  skewed_direction <- c(1, 0.5)
  skewed_delta <- as.vector(skewed_nudged$chil - skewed$chil)

  expect_equal(sum(skewed_delta * skewed_direction), 0, tolerance = 1e-12)
  expect_equal(
    sqrt(sum(skewed_delta^2)),
    .beta_annotation_perpendicular_nudge
  )
  expect_equal(.beta_perpendicular_nudge_for_linkage("b1", 0, 0), 0)
  expect_equal(.beta_perpendicular_nudge_for_linkage("a1", 0, 1), 0)
})

test_that("larger nodes shorten all three linkage annotation gaps", {
  default <- .linkage_label_positions(0, 0, 1, 0)
  enlarged <- .linkage_label_positions(0, 0, 1, 0, node_size = 1.2)
  default_radius <- .default_node_point_size
  enlarged_radius <- .default_node_point_size * 1.2
  default_x <- c(default$chil[[1]], default$par[[1]])
  enlarged_x <- c(enlarged$chil[[1]], enlarged$par[[1]])
  default_gaps <- c(
    default_x[[1]] - default_radius,
    default_x[[2]] - default_x[[1]],
    1 - default_radius - default_x[[2]]
  )
  enlarged_gaps <- c(
    enlarged_x[[1]] - enlarged_radius,
    enlarged_x[[2]] - enlarged_x[[1]],
    1 - enlarged_radius - enlarged_x[[2]]
  )

  expect_lt(max(enlarged_gaps - default_gaps), 0)
  expect_equal(
    default_gaps - enlarged_gaps,
    rep(.annotation_extra_offset(1.2) * 2 / 3, 3)
  )
})

test_that("labels on one linkage do not collide with each other", {
  inputs <- .prepare_cartoon_inputs(
    "Gal(b1-3)GalNAc(a1-",
    NULL,
    "left",
    "~"
  )
  annotation <- .linkage_annotation_data(
    inputs$structure,
    inputs$coor,
    node_size = 1.3,
    orient = "left"
  )

  separated <- .separate_overlapping_annotations(annotation)

  expect_equal(separated[, c("x", "y")], annotation[, c("x", "y")])
})

test_that("overlapping labels on different linkages reflect as a group", {
  glycan <- paste0(
    "Neu5Ac(a2-3)Gal(b1-3)[Neu5Ac(a2-3)Gal(b1-4)",
    "[Fuc(a1-3)]GlcNAc(b1-6)]GalNAc(a1-"
  )
  inputs <- .prepare_cartoon_inputs(glycan, NULL, "left", "~")
  annotation <- .linkage_annotation_data(
    inputs$structure,
    inputs$coor,
    orient = "left"
  )
  reflected_rows <- which(
    annotation$vertice == "4" &
      annotation$annot %in% c("beta", "6")
  )
  segment <- as.matrix(annotation[, c(
    "segment_start_x",
    "segment_start_y",
    "segment_end_x",
    "segment_end_y"
  )])
  reflected <- .reflected_annotation_coordinates(
    as.matrix(annotation[, c("x", "y")]),
    segment
  )

  separated <- .separate_overlapping_annotations(annotation)

  expect_equal(
    unname(as.matrix(separated[reflected_rows, c("x", "y")])),
    unname(reflected[reflected_rows, ])
  )
  expect_equal(
    separated[-reflected_rows, c("x", "y")],
    annotation[-reflected_rows, c("x", "y")]
  )
})

test_that("reducing-end beta annotations follow the physical edge direction", {
  purrr::walk(c("left", "right", "up", "down"), function(orient) {
    beta_inputs <- .prepare_cartoon_inputs(
      "Gal(b1-3)GalNAc(b1-",
      NULL,
      orient,
      ""
    )
    alpha_inputs <- .prepare_cartoon_inputs(
      "Gal(b1-3)GalNAc(a1-",
      NULL,
      orient,
      ""
    )
    beta <- .reducing_end_annotation_data(
      beta_inputs$structure,
      beta_inputs$coor,
      orient
    )
    alpha <- .reducing_end_annotation_data(
      alpha_inputs$structure,
      alpha_inputs$coor,
      orient
    )
    direction <- c(
      beta$segment$end_x - beta$segment$start_x,
      beta$segment$end_y - beta$segment$start_y
    )
    nudge <- c(
      beta$annotation$x[[1]] - alpha$annotation$x[[1]],
      beta$annotation$y[[1]] - alpha$annotation$y[[1]]
    )
    expected_nudge <- .beta_perpendicular_nudge_for_linkage(
      "beta",
      beta$segment$start_x,
      beta$segment$end_x
    )

    expect_equal(sum(nudge * direction), 0, tolerance = 1e-12)
    expect_equal(sqrt(sum(nudge^2)), expected_nudge)
  })
})

test_that("style constructors reject a NULL red_end", {
  expect_snapshot(
    error = TRUE,
    style_glydraw(red_end = NULL)
  )
  constructors <- list(style_glygen, style_snfg, style_glycoworkbench)
  purrr::walk(constructors, function(constructor) {
    condition <- rlang::catch_cnd(
      constructor(red_end = NULL),
      classes = "error"
    )

    expect_s3_class(condition, "error")
    expect_match(conditionMessage(condition), "red_end_length")
  })
})

test_that("tagged amino-acid sequences require exactly one site character", {
  valid_sites <- c("D", "@", "*", '"', "\\")
  purrr::walk(valid_sites, function(site) {
    red_end <- paste0("ABC<site>", site, "</site>EFG")
    style <- style_glydraw(red_end = red_end)
    sequence <- .parse_reducing_end_aa_sequence(style$red_end)

    expect_identical(sequence$site, site)
    expect_silent(parse(text = .format_reducing_end_aa_sequence(sequence)))
  })

  expect_snapshot(
    error = TRUE,
    style_glydraw(red_end = "ABC<site></site>EFG")
  )
  expect_snapshot(
    error = TRUE,
    style_glydraw(red_end = "ABC<site>DE</site>FG")
  )
  expect_snapshot(
    error = TRUE,
    style_glydraw(red_end = "ABC<site>D</site>EFG<site>H</site>I")
  )
  expect_snapshot(
    error = TRUE,
    style_glydraw(red_end = "ABC<site>D")
  )
})

test_that("amino-acid reducing ends anchor and fit in every orientation", {
  structure <- paste0(
    "Man(a1-3)[Man(a1-6)]Man(b1-4)",
    "GlcNAc(b1-4)GlcNAc(b1-"
  )
  red_end <- "ABC<site>D</site>EFGHIJK"
  parsed_sequence <- .parse_reducing_end_aa_sequence(red_end)
  metrics <- .reducing_end_aa_sequence_metrics(parsed_sequence)
  orientations <- c("left", "right", "up", "down")
  expected_angles <- c(left = 90, right = -90, up = 0, down = 0)
  expected_vjust <- c(left = 1, right = 1, up = 1, down = 0)

  purrr::walk(orientations, function(orient) {
    grob <- glycanGrob(structure, orient = orient, red_end = red_end)
    info <- grob$annotation_data$reducing_info
    sequence <- dplyr::filter(info$annotation, .data$is_aa_sequence)
    segment <- info$segment
    expected_coor <- c(x = segment$end_x, y = segment$end_y)

    expect_equal(sequence$angle, expected_angles[[orient]])
    expect_equal(sequence$vjust, expected_vjust[[orient]])
    expect_equal(
      sequence$hjust * metrics$width,
      metrics$site_center
    )
    expect_equal(
      unname(unlist(sequence[c("x", "y")])),
      unname(expected_coor)
    )
    expect_match(sequence$annot, 'bold\\("D"\\)')
    expect_false(grepl("<site>", sequence$annot, fixed = TRUE))
    expect_equal(nrow(info$bounds), 4)
  })
})

test_that("large amino-acid reducing ends reserve their full text box", {
  grob <- glycanGrob(
    "GalNAc(a1-",
    red_end = "ABC<site>D</site>EFJHI",
    style = style_glydraw(red_end_size = 10)
  )
  info <- grob$annotation_data$reducing_info
  sequence <- dplyr::filter(info$annotation, .data$is_aa_sequence)
  line_end <- c(x = info$segment$end_x, y = info$segment$end_y)
  parsed_sequence <- .parse_reducing_end_aa_sequence(
    "ABC<site>D</site>EFJHI"
  )
  metrics <- .reducing_end_aa_sequence_metrics(
    parsed_sequence,
    red_end_size = 10
  )

  expect_equal(unname(unlist(sequence[c("x", "y")])), unname(line_end))
  expect_equal(
    sequence$hjust * metrics$width,
    metrics$site_center
  )
  expect_equal(
    diff(range(info$bounds$x)),
    metrics$height
  )
  expect_equal(
    diff(range(info$bounds$y)),
    metrics$width
  )
  expect_no_error(
    glycanGrob(
      "GalNAc(a1-",
      style = style_glygen(
        red_end = "ABC<site>D</site>EFJHI",
        red_end_size = 10
      )
    )
  )
})

test_that("style constructors control reducing-end line length and text size", {
  constructors <- list(
    style_glydraw,
    style_glygen,
    style_snfg,
    style_glycoworkbench
  )
  styles <- purrr::map(constructors, ~ .x(red_end_length = 1.25))

  expect_equal(purrr::map_dbl(styles, "red_end_length"), rep(1.25, 4))
  expect_equal(
    purrr::map_dbl(constructors, ~ .x()$red_end_length),
    c(0.6, 1, 1, 1)
  )
  expect_equal(
    purrr::map_chr(constructors, ~ names(formals(.x))[[3]]),
    rep("red_end_length", 4)
  )
  sizes <- purrr::map(constructors, ~ .x(red_end_size = 9))
  expect_equal(purrr::map_dbl(sizes, "red_end_size"), rep(9, 4))
  expect_equal(
    purrr::map_dbl(constructors, ~ .x()$red_end_size),
    rep(6, 4)
  )
  expect_equal(
    purrr::map_chr(constructors, ~ names(formals(.x))[[4]]),
    rep("red_end_size", 4)
  )

  purrr::walk(c("left", "right", "up", "down"), function(orient) {
    default <- glycanGrob("Gal(b1-3)GalNAc(a1-", orient = orient)
    custom <- glycanGrob(
      "Gal(b1-3)GalNAc(a1-",
      orient = orient,
      style = style_glydraw(red_end_length = 1.25)
    )
    segment <- custom$annotation_data$reducing_info$segment
    length <- sqrt(
      (segment$end_x - segment$start_x)^2 +
        (segment$end_y - segment$start_y)^2
    )

    expect_equal(length, 1.25)
    expect_equal(
      custom$annotation_data$reducing_info$annotation[1, c("x", "y")],
      default$annotation_data$reducing_info$annotation[1, c("x", "y")]
    )
  })
})

test_that("red_end_size controls only custom reducing-end text", {
  structure <- "Gal(b1-3)GalNAc(a1-"
  custom <- glycanGrob(
    structure,
    style = style_glydraw(red_end = "Ser/Thr", red_end_size = 9)
  )
  annotation <- custom$annotation_data$annotation

  expect_equal(
    annotation$text_size[annotation$is_red_end_text],
    9
  )
  expect_equal(
    unique(annotation$text_size[!annotation$is_red_end_text]),
    6
  )

  default_wave <- glycanGrob(
    structure,
    style = style_glydraw(red_end = "~")
  )
  resized_wave <- glycanGrob(
    structure,
    style = style_glydraw(red_end = "~", red_end_size = 12)
  )
  expect_equal(
    resized_wave$annotation_data$reducing_info$wave,
    default_wave$annotation_data$reducing_info$wave
  )
})

test_that("zero-length reducing ends retain axis-aligned anomer labels", {
  structures <- c(
    "Gal(b1-3)GalNAc(a1-",
    "Gal(b1-3)GalNAc(b1-"
  )
  red_ends <- c("", "~", "Reducing end")

  purrr::walk(c("left", "right", "up", "down"), function(orient) {
    purrr::walk(structures, function(structure) {
      purrr::walk(red_ends, function(red_end) {
        grob <- glycanGrob(
          structure,
          orient = orient,
          style = style_glydraw(
            red_end = red_end,
            red_end_length = 0
          )
        )
        reducing_info <- grob$annotation_data$reducing_info
        label <- reducing_info$annotation[1, c("x", "y")]
        expected_label <- grob$reducing_end_coor +
          .reducing_end_line_vector(orient, 0.42)

        expect_equal(
          vapply(reducing_info, nrow, integer(1)),
          c(annotation = 1L, segment = 0L, wave = 0L, bounds = 0L)
        )
        expect_equal(
          unname(unlist(label)),
          unname(expected_label)
        )
        expect_identical(reducing_info$annotation$is_red_end_text, FALSE)
      })
    })
  })
})

test_that("draw_cartoon applies a complete custom SNFG palette", {
  structure <- "Gal(b1-4)GlcNAc(b1-"
  colors <- glydraw_colors()
  colors["glyYellow"] <- "#123456"

  plot <- draw_cartoon(
    structure,
    style = style_glydraw(colors = colors)
  )
  node_fill <- unique(ggplot2::ggplot_build(plot)$data[[3]]$fill)

  expect_contains(node_fill, "#123456")
  expect_contains(node_fill, "#0072BC")
})

test_that("draw_cartoon accepts reusable glydraw styles", {
  structure <- "Gal(b1-4)GlcNAc(b1-"
  colors <- glydraw_colors()
  colors["glyYellow"] <- "#123456"
  style <- style_glydraw(
    edge_linewidth = 1.2,
    font_family = "serif",
    colors = colors
  )

  styled_plot <- draw_cartoon(
    structure,
    show_linkage = FALSE,
    orient = "up",
    style = style
  )
  styled_layers <- ggplot2::ggplot_build(styled_plot)$data

  expect_s3_class(style, "glydraw_style")
  expect_false(any(c("show_linkage", "orient") %in% names(style)))
  expect_equal(style$font_family, "serif")
  expect_equal(unique(styled_layers[[1]]$linewidth), 1.2)
  expect_contains(unique(styled_layers[[3]]$fill), "#123456")
})

test_that("main APIs expose only the red_end style override", {
  expect_setequal(
    intersect(
      getNamespaceExports("glydraw"),
      c("glydraw_style", "style_glydraw")
    ),
    "style_glydraw"
  )
  styling_arguments <- names(formals(style_glydraw))
  interfaces <- list(
    draw_cartoon = draw_cartoon,
    draw_cartoon_sketch = draw_cartoon_sketch,
    glycanGrob = glycanGrob,
    geom_glycan = geom_glycan,
    guide_glycan = guide_glycan,
    scale_x_glycan = scale_x_glycan,
    scale_y_glycan = scale_y_glycan,
    anno_glycan = anno_glycan,
    export_cartoons = export_cartoons,
    export_cartoons.character = export_cartoons.character,
    export_cartoons.glyrepr_structure = export_cartoons.glyrepr_structure
  )
  interface_arguments <- purrr::map(interfaces, ~ names(formals(.x)))

  purrr::walk(
    interface_arguments,
    ~ expect_setequal(intersect(styling_arguments, .x), "red_end")
  )
  expect_false(any(c("show_linkage", "orient") %in% styling_arguments))
  expect_true(all(
    c("show_linkage", "orient") %in% interface_arguments$draw_cartoon
  ))
  expect_true(all(
    c("show_linkage", "orient") %in% interface_arguments$glycanGrob
  ))
  expect_true(all(
    c("show_linkage", "orient") %in% interface_arguments$geom_glycan
  ))
  expect_true(all(
    c("show_linkage", "orient") %in% interface_arguments$guide_glycan
  ))
  expect_true(all(
    c("show_linkage", "orient") %in% interface_arguments$export_cartoons
  ))
  expect_true("show_linkage" %in% interface_arguments$scale_x_glycan)
  expect_true("show_linkage" %in% interface_arguments$scale_y_glycan)
  expect_true("show_linkage" %in% interface_arguments$anno_glycan)
  purrr::walk(
    interfaces,
    ~ expect_identical(formals(.x)$style, quote(style_glydraw()))
  )
  purrr::walk(
    interfaces,
    ~ expect_null(formals(.x)$red_end)
  )
  purrr::walk(
    interface_arguments,
    ~ expect_identical(tail(.x, 1), "red_end")
  )
})

test_that("explicit red_end overrides style and NULL inherits it", {
  structure <- "Gal(b1-3)GalNAc(a1-"
  style <- style_glydraw(red_end = "~")

  inherited <- glycanGrob(structure, style = style)
  overridden <- glycanGrob(
    structure,
    style = style,
    red_end = "Reducing end"
  )
  drawn <- draw_cartoon(
    structure,
    style = style,
    red_end = "Reducing end"
  )
  drawn_annotation <- drawn$layers[["geom_text"]]$data$annot

  expect_gt(nrow(inherited$annotation_data$reducing_info$wave), 0)
  expect_true(any(
    overridden$annotation_data$reducing_info$annotation$is_red_end_text
  ))
  expect_match(
    overridden$annotation_data$reducing_info$annotation$annot[[2]],
    "Reducing end"
  )
  expect_contains(drawn_annotation, '"Reducing end"')
  expect_identical(style$red_end, "~")
})

test_that("draw_cartoon requires the complete named SNFG palette", {
  structure <- "Gal(b1-4)GlcNAc(b1-"
  colors <- glydraw_colors()

  expect_identical(style_glydraw()$colors, glydraw_colors())
  expect_error(
    draw_cartoon(structure, style = style_glydraw(colors = colors[-1])),
    "exactly the names"
  )
  expect_error(
    style_glydraw(colors = c(colors, extra = "#123456")),
    "exactly the names"
  )
  names(colors)[2] <- names(colors)[1]
  expect_error(
    style_glydraw(colors = colors),
    "exactly the names"
  )
  colors <- glydraw_colors()
  colors["glyYellow"] <- "not-a-color"
  expect_error(
    style_glydraw(colors = colors),
    "valid R colors"
  )
})

test_that("draw_cartoon warns and hides linkage annotations for oversized nodes", {
  structure <- "Gal(b1-3)GalNAc(a1-"

  expect_warning(
    draw_cartoon(
      structure,
      style = style_glydraw(node_size = 1.5, red_end = "Ser/Thr")
    ),
    "Linkage annotations are hidden"
  )
})

test_that("draw_cartoon rejects node_size values that make residues overlap", {
  structure <- "Gal(b1-3)GalNAc(a1-"

  expect_error(
    draw_cartoon(structure, style = style_glydraw(node_size = 2.1)),
    "`node_size` must be no larger than 2"
  )
  expect_warning(
    expect_s3_class(
      draw_cartoon(structure, style = style_glydraw(node_size = 2)),
      "glydraw_cartoon"
    ),
    "Linkage annotations are hidden"
  )
})

test_that("print.glydraw_cartoon rasterizes fixed-size cartoon for display", {
  structure <- paste0(
    "Gal(b1-4)GlcNAc(b1-2)[Gal(b1-4)GlcNAc(b1-4)]Man(a1-3)",
    "[Gal(b1-4)GlcNAc(b1-2)[Gal(b1-4)GlcNAc(b1-4)]",
    "[Gal(b1-4)GlcNAc(b1-6)]Man(a1-6)]Man(b1-4)GlcNAc(b1-4)",
    "[Fuc(a1-6)]GlcNAc(b1-"
  )
  plot <- draw_cartoon(structure)
  original_width <- as.numeric(plot$theme$panel.widths)
  size <- attr(plot, "glydraw_size_px")
  raster <- .render_cartoon_raster(plot)
  file <- tempfile(fileext = ".png")

  expect_s3_class(raster, "nativeRaster")
  expect_equal(ncol(raster), size[["width"]], tolerance = 1)
  expect_equal(nrow(raster), size[["height"]], tolerance = 1)

  grDevices::png(file, width = 4, height = 3, units = "in", res = 300)
  on.exit(grDevices::dev.off())
  printed_plot <- print(plot)

  expect_identical(printed_plot, plot)
  expect_equal(as.numeric(plot$theme$panel.widths), original_width)
})

test_that("draw_cartoon supports four directional orientations", {
  structure <- "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  orientations <- c("left", "right", "up", "down")
  inputs <- purrr::map(
    orientations,
    ~ .prepare_cartoon_inputs(structure, NULL, .x, "")
  ) |>
    stats::setNames(orientations)
  left <- inputs$left$coor

  expect_equal(
    inputs$right$coor,
    cbind(x = -left[, "x"], y = -left[, "y"])
  )
  expect_equal(
    inputs$up$coor,
    cbind(x = left[, "y"], y = -left[, "x"])
  )
  expect_equal(
    inputs$down$coor,
    cbind(x = -left[, "y"], y = left[, "x"])
  )
  purrr::walk(orientations, function(orient) {
    plot <- draw_cartoon(
      structure,
      orient = orient,
      style = style_glydraw(red_end = "Asn")
    )
    expect_s3_class(plot, "glydraw_cartoon")
    expect_s3_class(plot, "ggplot")
  })
})

test_that("reducing ends point away from each directional orientation", {
  orientations <- c("left", "right", "up", "down")
  vectors <- purrr::map(
    orientations,
    .reducing_end_line_vector,
    length = 1
  ) |>
    stats::setNames(orientations)

  expect_equal(vectors$left, c(x = 1, y = 0))
  expect_equal(vectors$right, c(x = -1, y = 0))
  expect_equal(vectors$up, c(x = 0, y = -1))
  expect_equal(vectors$down, c(x = 0, y = 1))
})

test_that("draw_cartoon rejects the previous orientation abbreviations", {
  expect_snapshot(
    error = TRUE,
    draw_cartoon("Gal(b1-3)GalNAc(a1-", orient = "H")
  )
})

test_that("left and right Fuc-like triangles align with rectangle borders", {
  rectangle <- glycan_shape[["HexNAc"]]
  shape_names <- c("dHexRight", "dHexLeft", "dHexNAcRight", "dHexNAcLeft")

  purrr::walk(shape_names, function(shape_name) {
    shape <- glycan_shape[[shape_name]]

    expect_equal(
      range(shape$x),
      range(rectangle$x),
      info = shape_name
    )
  })
})

test_that("dHex uses Fuc-like layout and orientation", {
  structure <- "HexNAc(??-?)[dHex(??-?)]HexNAc(??-"
  inputs <- .prepare_cartoon_inputs(structure, NULL, "left", "")
  dhex <- which(igraph::V(inputs$structure)$mono == "dHex")

  expect_equal(inputs$coor[dhex, ], c(x = 0, y = 1))
  expect_equal(
    .residue_glycoforms(inputs$structure, inputs$coor, "flex")[dhex],
    "dHexUp"
  )
  expect_equal(
    .residue_glycoforms(inputs$structure, inputs$coor, "up")[dhex],
    "dHex"
  )
  expect_s3_class(draw_cartoon(structure), "glydraw_cartoon")
})

test_that("double core Fuc without linkages uses opposite branch sides", {
  structure <- "GlcNAc(??-?)[Fuc(??-?)][Fuc(??-?)]GlcNAc(??-"
  inputs <- .prepare_cartoon_inputs(structure, NULL, "left", "")
  graph <- inputs$structure
  core <- length(graph)
  fuc <- as.integer(igraph::neighbors(graph, core, mode = "out"))
  fuc <- fuc[igraph::V(graph)[fuc]$mono == "Fuc"]

  expect_equal(
    sort(unname(inputs$coor[fuc, "y"] - inputs$coor[core, "y"])),
    c(-1, 1)
  )
})

test_that("bisecting GlcNAc is centered without linkage information", {
  structure <- paste0(
    "Neu5Ac(??-?)Gal(??-?)GlcNAc(??-?)Man(??-?)",
    "[Gal(??-?)GlcNAc(??-?)Man(??-?)]",
    "[GlcNAc(??-?)]Man(??-?)GlcNAc(??-?)GlcNAc(??-"
  )
  inputs <- .prepare_cartoon_inputs(structure, NULL, "left", "")
  graph <- inputs$structure
  child_num <- purrr::map_int(
    seq_along(igraph::V(graph)),
    \(vertex) length(igraph::neighbors(graph, vertex, mode = "out"))
  )
  core <- which(igraph::V(graph)$mono == "Man" & child_num == 3)
  children <- as.integer(igraph::neighbors(graph, core, mode = "out"))
  bisecting <- children[igraph::V(graph)[children]$mono == "GlcNAc"]
  arms <- children[igraph::V(graph)[children]$mono == "Man"]

  expect_equal(unname(inputs$coor[bisecting, "y"]), 0)
  expect_equal(sort(unname(inputs$coor[arms, "y"])), c(-1, 1))
  expect_s3_class(
    draw_cartoon(structure, show_linkage = FALSE),
    "glydraw_cartoon"
  )
})

test_that("draw_cartoon left-aligns vertical substituent labels", {
  structure <- "Neu5Ac9Ac(a2-3)Gal6S(b1-"

  plot <- draw_cartoon(structure, orient = "up")
  annotation <- ggplot2::ggplot_build(plot)$data[[4]]
  substituent <- dplyr::filter(annotation, .data$label == '"9Ac"')
  x_range <- ggplot2::get_panel_scales(plot)$x$range$range

  expect_equal(substituent$hjust, 0)
  expect_gt(x_range[[2]], substituent$x + 0.5)
})

test_that("draw_cartoon bottom-aligns horizontal substituent labels", {
  structure <- "Neu5Ac9Ac(a2-3)Gal6S(b1-"

  plot <- draw_cartoon(structure, orient = "left")
  annotation <- ggplot2::ggplot_build(plot)$data[[4]]
  substituent <- dplyr::filter(annotation, .data$label == '"9Ac"')
  y_range <- ggplot2::get_panel_scales(plot)$y$range$range

  expect_equal(substituent$vjust, 0)
  expect_gt(y_range[[2]], substituent$y + 0.3)
})

test_that("draw_cartoon aligns substituent labels in new directions", {
  structure <- "Neu5Ac9Ac(a2-3)Gal6S(b1-"
  right <- draw_cartoon(structure, orient = "right")
  right_annotation <- ggplot2::ggplot_build(right)$data[[4]]
  right_substituent <- dplyr::filter(
    right_annotation,
    .data$label == '"9Ac"'
  )
  right_y_range <- ggplot2::get_panel_scales(right)$y$range$range
  down <- draw_cartoon(structure, orient = "down")
  down_annotation <- ggplot2::ggplot_build(down)$data[[4]]
  down_substituent <- dplyr::filter(
    down_annotation,
    .data$label == '"9Ac"'
  )
  down_x_range <- ggplot2::get_panel_scales(down)$x$range$range

  expect_equal(right_substituent$vjust, 1)
  expect_lt(right_y_range[[1]], right_substituent$y - 0.3)
  expect_equal(down_substituent$hjust, 1)
  expect_lt(down_x_range[[1]], down_substituent$x - 0.5)
})

test_that("draw_cartoon works with linkage hidden", {
  structure <- "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"

  plot_no_linkage <- draw_cartoon(structure, show_linkage = FALSE)
  expect_s3_class(plot_no_linkage, "glydraw_cartoon")
})

test_that("linkage-hidden cartoons skip unused annotation construction", {
  inputs <- .prepare_cartoon_inputs(
    "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-",
    NULL,
    "left",
    ""
  )
  testthat::local_mocked_bindings(
    .linkage_annotation_data = function(...) {
      stop("linkage annotations were constructed")
    }
  )

  annotation <- .cartoon_text_annotation_data(
    inputs$structure,
    inputs$coor,
    show_linkage = FALSE
  )

  expect_equal(nrow(annotation$show_without_linkage), 0)
})

test_that("draw_cartoon works with reducing-end O-Fuc glycans", {
  glycans <- c(
    "Fuc(a1-",
    "GlcNAc(b1-3)Fuc(a1-"
  )

  cartoons <- purrr::map(glycans, draw_cartoon)

  purrr::walk(cartoons, expect_s3_class, "glydraw_cartoon")
})

test_that("draw_cartoon preserves nested Xyl-Gal-Fuc side-chain order", {
  structure <- "Glc(b1-4)[Fuc(a1-2)Gal(b1-2)Xyl(a1-6)]Glc(b1-4)Glc(b1-"

  inputs <- .prepare_cartoon_inputs(structure, NULL, "left", "")
  graph <- inputs$structure
  coor <- inputs$coor
  mono <- igraph::V(graph)$mono
  fuc <- which(mono == "Fuc")
  gal <- which(mono == "Gal")
  xyl <- which(mono == "Xyl")

  expect_equal(
    unname(coor[c(xyl, gal, fuc), "x"]),
    rep(unname(coor[xyl, "x"]), 3)
  )
  expect_gt(coor[gal, "y"], coor[xyl, "y"])
  expect_gt(coor[fuc, "y"], coor[gal, "y"])

  annotation <- .cartoon_text_annotation_data(
    graph,
    coor,
    "left",
    "",
    NULL
  )$annotation
  fuc_labels <- dplyr::filter(annotation, .data$vertice == as.character(fuc))
  gal_labels <- dplyr::filter(annotation, .data$vertice == as.character(gal))

  expect_true(all(
    fuc_labels$y > coor[gal, "y"] & fuc_labels$y < coor[fuc, "y"]
  ))
  expect_true(all(
    gal_labels$y > coor[xyl, "y"] & gal_labels$y < coor[gal, "y"]
  ))
})

test_that("linkage annotations preserve row-wise topology calculations", {
  structures <- c(
    "Gal(b1-3)[GlcNAc(b1-6)]GalNAc(a1-",
    paste0(
      "Neu5Ac(a2-3)Gal(b1-3)[Fuc(a1-2)Gal(b1-3)[Fuc(a1-4)]",
      "GlcNAc(b1-3)[Gal(b1-4)GlcNAc(b1-6)]Gal(b1-4)",
      "GlcNAc(b1-6)]GalNAc(a1-"
    )
  )

  purrr::walk(structures, function(structure) {
    inputs <- .prepare_cartoon_inputs(structure, NULL, "left", "")
    expected <- purrr::map_dfr(
      seq_len(length(inputs$structure) - 1),
      \(.vertex) {
        .linkage_annotation_rows(
          inputs$structure,
          inputs$coor,
          .vertex,
          orient = "left"
        )
      }
    )
    expected$annot <- .normalize_linkage_labels(expected$annot)

    expect_identical(
      .linkage_annotation_data(inputs$structure, inputs$coor, orient = "left"),
      expected
    )
  })
})
