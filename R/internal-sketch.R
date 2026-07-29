# Internal helpers for assembling glycan cartoons from ggsketch layers.

#' Convert a glycan grob to a sketch-style cartoon plot
#'
#' @param grob A `glycanGrob` object returned by [glycanGrob()].
#' @param sketch A named list of ggsketch rendering parameters.
#'
#' @returns A `glydraw_cartoon` ggplot object with fixed-size metadata.
#' @noRd
.glycan_grob_to_sketch_plot <- function(grob, sketch) {
  checkmate::assert_class(grob, "glycanGrob")
  border_px <- grob$glydraw_border_px
  if (is.null(border_px)) {
    border_px <- .default_cartoon_border_px
  }
  background <- grob$glydraw_background
  if (is.null(background)) {
    background <- TRUE
  }

  plot <- .sketch_cartoon_base_layers(grob, sketch)
  plot <- .add_sketch_cartoon_text_layers(plot, grob)
  font_family <- attr(plot, "glydraw_sketch_font_family", exact = TRUE)
  if (is.null(font_family)) {
    font_family <- .resolve_sketch_text_family()
  }
  plot <- .add_cartoon_text_bounds(plot, grob$annotation_data$bounds)
  plot <- .add_sketch_reducing_end_layers(plot, grob, sketch)
  if (!background) {
    plot <- .remove_cartoon_background(plot)
  }
  plot <- .finalize_cartoon_size(plot, border_px = border_px)
  attr(plot, "glydraw_font_family") <- font_family
  plot
}

#' Build sketch segment and residue polygon layers
#'
#' @param grob A prepared `glycanGrob`.
#' @param sketch A named list of ggsketch rendering parameters.
#'
#' @returns A ggplot object containing sketch-style glycan layers.
#' @noRd
.sketch_cartoon_base_layers <- function(grob, sketch) {
  node_layers <- .sketch_cartoon_node_layers(grob, sketch)

  ggplot2::ggplot() +
    ggsketch::geom_sketch_segment(
      data = grob$connect_df,
      ggplot2::aes(
        x = .data$start_x,
        y = .data$start_y,
        xend = .data$end_x,
        yend = .data$end_y
      ),
      alpha = grob$connect_df$transparency,
      linewidth = grob$edge_linewidth,
      roughness = sketch$roughness,
      bowing = sketch$bowing,
      n_passes = sketch$n_passes,
      seed = sketch$seed,
      medium = sketch$medium
    ) +
    ggplot2::geom_polygon(
      data = grob$polygon_coor,
      ggplot2::aes(
        x = .data$point_x,
        y = .data$point_y,
        group = .data$group
      ),
      fill = "white",
      color = "white",
      linewidth = grob$node_linewidth
    ) +
    node_layers +
    ggplot2::coord_fixed(ratio = 1, clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}

#' Build one independently seeded sketch layer per residue
#'
#' @param grob A prepared `glycanGrob`.
#' @param sketch A named list of ggsketch rendering parameters.
#'
#' @returns A list of ggplot2 sketch polygon layers.
#' @noRd
.sketch_cartoon_node_layers <- function(grob, sketch) {
  group_levels <- unique(grob$polygon_coor$group)
  rows_by_group <- split(
    seq_len(nrow(grob$polygon_coor)),
    factor(grob$polygon_coor$group, levels = group_levels)
  )
  seeds <- .sketch_node_seeds(sketch$seed, length(rows_by_group))
  fill_gap <- sketch$hachure_gap

  purrr::map2(rows_by_group, seeds, function(rows, seed) {
    node <- grob$polygon_coor[rows, , drop = FALSE]
    node_diameter <- 2 * .cartoon_circle_radius_inches(node$radius[[1L]])
    fill_gap_inches <- fill_gap * node_diameter
    if (node$primitive[[1L]] == "circle") {
      return(
        ggsketch::geom_sketch_circle(
          data = node[1L, , drop = FALSE],
          ggplot2::aes(
            x = .data$center_x,
            y = .data$center_y,
            r = .data$radius
          ),
          alpha = node$alpha[[1L]],
          fill = grob$filled_color[[rows[[1L]]]],
          color = scales::alpha("black", node$alpha[[1L]]),
          linewidth = grob$node_linewidth,
          # Ellipse jitter reads more strongly than polygon jitter at node size.
          roughness = sketch$roughness * 0.1,
          bowing = sketch$bowing,
          n_passes = sketch$n_passes,
          seed = seed,
          fill_style = sketch$fill_style,
          hachure_angle = sketch$hachure_angle,
          hachure_gap = fill_gap,
          fill_weight = sketch$fill_weight,
          fill_roughness = sketch$roughness * 0.5
        )
      )
    }

    ggsketch::geom_sketch_polygon(
      data = node,
      ggplot2::aes(
        x = .data$point_x,
        y = .data$point_y,
        group = .data$group
      ),
      alpha = node$alpha,
      fill = grob$filled_color[rows],
      color = scales::alpha("black", node$alpha),
      linewidth = grob$node_linewidth,
      roughness = sketch$roughness,
      bowing = sketch$bowing,
      n_passes = sketch$n_passes,
      seed = seed,
      fill_style = sketch$fill_style,
      hachure_angle = sketch$hachure_angle,
      hachure_gap = fill_gap_inches,
      fill_weight = sketch$fill_weight,
      fill_roughness = sketch$roughness * 0.5
    )
  })
}

#' Derive stable independent seeds for sketch nodes
#'
#' @param seed Optional user-supplied sketch seed.
#' @param n Number of node seeds to derive.
#'
#' @returns An integer vector of length `n`.
#' @noRd
.sketch_node_seeds <- function(seed, n) {
  if (is.null(seed) || (length(seed) == 1L && is.na(seed))) {
    seed <- getOption("ggsketch.seed", 1L)
  }
  base_seed <- as.integer(seed[[1L]])
  offsets <- seq_len(n) * 104729
  as.integer((as.double(base_seed) + offsets) %% 2147483647)
}

#' Add sketch cartoon text layers
#'
#' @param plot A ggplot object.
#' @param grob A prepared `glycanGrob`.
#'
#' @returns A ggplot object with zero or one added text layer.
#' @noRd
.add_sketch_cartoon_text_layers <- function(plot, grob) {
  annotation <- if (grob$show_linkage) {
    grob$annotation_data$annotation
  } else {
    grob$annotation_data$show_without_linkage
  }
  if (nrow(annotation) == 0) {
    return(plot)
  }
  family <- .resolve_sketch_text_family()
  annotation$annot_label <- .sketch_annotation_labels(annotation)
  aa_sequence <- annotation$is_aa_sequence
  for (parse in c(FALSE, TRUE)) {
    rows <- aa_sequence == parse
    if (!any(rows)) {
      next
    }
    text_layer <- ggsketch::geom_sketch_text(
      data = annotation[rows, , drop = FALSE],
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$annot_label,
        hjust = .data$hjust,
        vjust = .data$vjust,
        angle = .data$angle
      ),
      alpha = annotation$transparency[rows],
      parse = parse,
      size = annotation$text_size[rows],
      family = family
    )
    plot <- plot + text_layer
  }
  attr(plot, "glydraw_sketch_font_family") <- family
  plot
}

#' Prepare plain-text labels for a sketch cartoon
#'
#' @param annotation An annotation data frame with character column `annot`.
#'
#' @returns A character vector containing Unicode Greek letters, normalized
#'   unknown linkages, and otherwise unchanged annotation text.
#' @noRd
.sketch_annotation_labels <- function(annotation) {
  labels <- annotation$annot
  if ("is_red_end_text" %in% names(annotation)) {
    red_end_text <- !is.na(annotation$is_red_end_text) &
      annotation$is_red_end_text
    labels[red_end_text] <- .unquote_plotmath_text(labels[red_end_text])
  }
  if ("is_aa_sequence" %in% names(annotation)) {
    labels[annotation$is_aa_sequence] <- annotation$annot_label[
      annotation$is_aa_sequence
    ]
  }
  labels[labels == "alpha"] <- "\u03b1"
  labels[labels == "beta"] <- "\u03b2"
  unknown <- labels %in% c("?", "??", '~"?"') | grepl("^\\?\\d+", labels)
  labels[unknown] <- "?"
  labels
}

#' Resolve a handwriting font that covers all sketch annotation glyphs
#'
#' @returns A character font family. Selection prefers an available handwriting
#'   font that contains Greek alpha, Greek beta, and decimal digits.
#' @noRd
.resolve_sketch_text_family <- function() {
  resolved <- ggsketch::geom_sketch_text()$aes_params$family
  if (
    !requireNamespace("systemfonts", quietly = TRUE) ||
      .sketch_font_supports_labels(resolved)
  ) {
    return(resolved)
  }

  available <- suppressMessages(ggsketch::ggsketch_check_fonts())
  candidates <- names(available)[available]
  supported <- vapply(
    candidates,
    .sketch_font_supports_labels,
    logical(1)
  )
  if (any(supported)) {
    return(candidates[[which(supported)[[1]]]])
  }
  resolved
}

#' Check whether a font covers sketch linkage labels
#'
#' @param font_family A character font family.
#'
#' @returns A logical scalar.
#' @noRd
.sketch_font_supports_labels <- function(font_family) {
  if (!nzchar(font_family)) {
    return(FALSE)
  }
  glyphs <- systemfonts::glyph_info(
    c("\u03b1", "\u03b2", as.character(0:9)),
    family = font_family
  )
  all(glyphs$index > 0)
}

#' Add a sketch reducing-end wave and its invisible bounds
#'
#' @param plot A ggplot object.
#' @param grob A prepared `glycanGrob`.
#' @param sketch A named list of ggsketch rendering parameters.
#'
#' @returns A ggplot object with optional wave and bounds layers.
#' @noRd
.add_sketch_reducing_end_layers <- function(plot, grob, sketch) {
  reducing_info <- grob$annotation_data$reducing_info
  if (nrow(reducing_info$wave) > 0) {
    plot <- plot +
      ggsketch::geom_sketch_path(
        data = reducing_info$wave,
        ggplot2::aes(x = .data$x, y = .data$y),
        linewidth = grob$edge_linewidth,
        roughness = sketch$roughness,
        bowing = sketch$bowing,
        n_passes = sketch$n_passes,
        seed = sketch$seed,
        medium = sketch$medium
      )
  }
  if (nrow(reducing_info$bounds) > 0) {
    plot <- plot +
      ggplot2::geom_blank(
        data = reducing_info$bounds,
        ggplot2::aes(x = .data$x, y = .data$y)
      )
  }
  plot
}
