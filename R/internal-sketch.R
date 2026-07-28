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
    font_family <- grob$font_family
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
    ggsketch::geom_sketch_polygon(
      data = grob$polygon_coor,
      ggplot2::aes(
        x = .data$point_x,
        y = .data$point_y,
        group = .data$group
      ),
      alpha = grob$polygon_coor$alpha,
      fill = grob$filled_color,
      color = scales::alpha("black", grob$polygon_coor$alpha),
      linewidth = grob$node_linewidth,
      roughness = sketch$roughness,
      bowing = sketch$bowing,
      n_passes = sketch$n_passes,
      seed = sketch$seed,
      fill_style = sketch$fill_style,
      hachure_angle = sketch$hachure_angle,
      hachure_gap = sketch$hachure_gap,
      fill_weight = sketch$fill_weight
    ) +
    ggplot2::coord_fixed(ratio = 1, clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
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
  family <- if (identical(grob$font_family, "")) {
    NULL
  } else {
    grob$font_family
  }
  label_family <- if (is.null(family)) "ggsketch" else family
  annotation$annot_label <- .font_family_annotation_labels(
    annotation,
    label_family
  )

  text_layer <- ggsketch::geom_sketch_text(
    data = annotation,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      label = .data$annot_label,
      hjust = .data$hjust,
      vjust = .data$vjust
    ),
    alpha = annotation$transparency,
    parse = TRUE,
    size = 6,
    family = family
  )
  plot <- plot + text_layer
  attr(plot, "glydraw_sketch_font_family") <- text_layer$aes_params$family
  plot
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
