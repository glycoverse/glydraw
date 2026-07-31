# Internal helpers for drawing prepared glycan geometry directly with grid.

#' Calculate native grid layout metadata
#'
#' @param grob A prepared `glycanGrob`.
#'
#' @returns A list containing data ranges, expanded panel ranges, panel size,
#'   total size, border size, and background state.
#' @noRd
.cartoon_grid_layout <- function(grob) {
  annotation <- if (grob$show_linkage) {
    grob$annotation_data$annotation
  } else {
    grob$annotation_data$show_without_linkage
  }
  reducing_info <- grob$annotation_data$reducing_info

  x_range <- .cartoon_grid_range(c(
    grob$connect_df$start_x,
    grob$connect_df$end_x,
    grob$polygon_coor$point_x,
    annotation$x,
    grob$annotation_data$bounds$x,
    reducing_info$wave$x,
    reducing_info$bounds$x
  ))
  y_range <- .cartoon_grid_range(c(
    grob$connect_df$start_y,
    grob$connect_df$end_y,
    grob$polygon_coor$point_y,
    annotation$y,
    grob$annotation_data$bounds$y,
    reducing_info$wave$y,
    reducing_info$bounds$y
  ))
  data_ranges <- list(x = x_range, y = y_range)
  panel_ranges <- lapply(
    data_ranges,
    scales::expand_range,
    mul = .cartoon_panel_expansion
  )
  panel_size_px <- c(
    width = .cartoon_units_per_coordinate * diff(x_range),
    height = .cartoon_units_per_coordinate * diff(y_range)
  )
  border_px <- grob$glydraw_border_px
  if (is.null(border_px)) {
    border_px <- .default_cartoon_border_px
  }
  background <- grob$glydraw_background
  if (is.null(background)) {
    background <- TRUE
  }

  list(
    data_ranges = data_ranges,
    panel_ranges = panel_ranges,
    panel_size_px = panel_size_px,
    size_px = panel_size_px + 2 * border_px,
    border_px = border_px,
    background = background
  )
}

#' Find the finite range of grid coordinates
#'
#' @param x Numeric coordinate values.
#'
#' @returns A finite numeric range of length two.
#' @noRd
.cartoon_grid_range <- function(x) {
  range(x[is.finite(x)])
}

#' Construct a fixed-size native grid glycan
#'
#' @param grob A prepared `glycanGrob`.
#' @param layout Grid layout metadata from `.cartoon_grid_layout()`.
#' @param scale Positive whole-cartoon size multiplier.
#' @param name Grob name prefix.
#'
#' @returns A `glycan_grid_grob` containing native grid primitives.
#' @noRd
.cartoon_grid_grob <- function(grob, layout, scale, name) {
  panel_width <- grid::unit(
    layout$panel_size_px[["width"]] /
      .default_cartoon_dpi *
      scale,
    "in"
  )
  panel_height <- grid::unit(
    layout$panel_size_px[["height"]] /
      .default_cartoon_dpi *
      scale,
    "in"
  )
  width <- grid::unit(
    layout$size_px[["width"]] / .default_cartoon_dpi * scale,
    "in"
  )
  height <- grid::unit(
    layout$size_px[["height"]] / .default_cartoon_dpi * scale,
    "in"
  )
  primitives <- .cartoon_grid_primitives(grob, layout, scale)
  panel <- grid::gTree(
    children = rlang::exec(grid::gList, !!!primitives),
    vp = grid::viewport(
      width = panel_width,
      height = panel_height,
      xscale = c(0, 1),
      yscale = c(0, 1),
      clip = "off"
    ),
    name = paste0(name, ".panel")
  )
  children <- list()
  if (layout$background) {
    children <- append(
      children,
      list(grid::rectGrob(
        width = width,
        height = height,
        gp = grid::gpar(col = NA, fill = "white"),
        name = paste0(name, ".background")
      ))
    )
  }
  children <- append(children, list(panel))

  grid::gTree(
    children = rlang::exec(grid::gList, !!!children),
    width = width,
    height = height,
    name = paste0(name, ".grid"),
    cl = "glycan_grid_grob"
  )
}

#' Wrap native primitives in the legacy fixed-size gtable shell
#'
#' The shell preserves the established grid viewport structure used by visual
#' snapshots without rebuilding the complete glycan ggplot.
#'
#' @param grob A prepared `glycanGrob`.
#' @param layout Grid layout metadata from `.cartoon_grid_layout()`.
#' @param name Grob name prefix.
#'
#' @returns A `glycan_grid_grob` inheriting from `gtable`.
#' @noRd
.cartoon_grid_gtable <- function(grob, layout, name) {
  result <- .cartoon_grid_gtable_template()
  panel_index <- match("panel", result$layout$name)
  panel <- result$grobs[[panel_index]]
  base_children <- panel$children
  primitives <- .cartoon_grid_primitives(grob, layout, scale = 1)
  panel$children <- rlang::exec(
    grid::gList,
    !!!base_children[seq_len(2)],
    !!!primitives,
    !!!base_children[3:5]
  )
  panel$childrenOrder <- names(panel$children)
  result$grobs[[panel_index]] <- panel
  result$widths[[7]] <- grid::unit(
    layout$panel_size_px[["width"]] / .default_cartoon_dpi,
    "in"
  )
  result$heights[[9]] <- grid::unit(
    layout$panel_size_px[["height"]] / .default_cartoon_dpi,
    "in"
  )
  result$width <- grid::unit(
    layout$size_px[["width"]] / .default_cartoon_dpi,
    "in"
  )
  result$height <- grid::unit(
    layout$size_px[["height"]] / .default_cartoon_dpi,
    "in"
  )
  result$name <- paste0(name, ".grid")
  class(result) <- c("glycan_grid_grob", class(result))
  result
}

#' Build the reusable gtable shell for native glycan primitives
#'
#' @returns A fixed-size blank ggplot gtable.
#' @noRd
.cartoon_grid_gtable_template <- local({
  template <- NULL

  function() {
    if (is.null(template)) {
      plot <- ggplot2::ggplot() +
        ggplot2::coord_fixed(ratio = 1, clip = "off") +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "none")
      plot <- .remove_cartoon_background(plot)
      plot <- .set_fixed_panel_size(
        plot,
        c(width = .default_cartoon_dpi, height = .default_cartoon_dpi),
        .default_cartoon_dpi
      )
      template <<- ggplot2::ggplotGrob(plot)
    }
    template
  }
})

#' Build native grid drawing primitives
#'
#' @param grob A prepared `glycanGrob`.
#' @param layout Grid layout metadata from `.cartoon_grid_layout()`.
#' @param scale Positive whole-cartoon size multiplier.
#'
#' @returns A list of native grid grobs in drawing order.
#' @noRd
.cartoon_grid_primitives <- function(grob, layout, scale) {
  alpha <- .cartoon_grid_alpha(grob)
  primitives <- list(
    .cartoon_grid_segments(grob, layout, scale),
    .cartoon_grid_polygons(
      grob,
      layout,
      scale,
      fill = rep("#FFFFFF", nrow(grob$polygon_coor)),
      colour = rep("white", nrow(grob$polygon_coor)),
      alpha = NULL,
      name = "glycan.node.mask"
    ),
    .cartoon_grid_polygons(
      grob,
      layout,
      scale,
      fill = grob$filled_color,
      colour = rep("black", nrow(grob$polygon_coor)),
      alpha = grob$polygon_coor$alpha,
      name = "glycan.node"
    ),
    .cartoon_grid_text(grob, layout, scale),
    .cartoon_grid_wave(grob, layout, scale)
  )

  primitives <- Filter(\(primitive) !inherits(primitive, "null"), primitives)
  names(primitives) <- vapply(primitives, \(primitive) primitive$name, "")
  if (alpha != 1) {
    primitives <- .composite_cartoon_alpha(primitives, alpha)
  }
  primitives
}

#' Resolve and validate whole-cartoon transparency
#'
#' @param grob A prepared `glycanGrob`.
#'
#' @returns A numeric alpha value. Missing values and `NULL` resolve to `1`.
#' @noRd
.cartoon_grid_alpha <- function(grob) {
  alpha <- grob$glydraw_alpha
  if (is.null(alpha) || is.na(alpha)) {
    alpha <- 1
  }
  .validate_cartoon_alpha_device(alpha)
  alpha
}

#' Validate graphics-device support for whole-cartoon transparency
#'
#' @param alpha Numeric whole-cartoon transparency.
#' @param capabilities Graphics-device capabilities returned by
#'   [grDevices::dev.capabilities()].
#'
#' @returns `alpha`, invisibly. Throws an error when non-opaque alpha is
#'   requested on a device without alpha-mask and transformation support.
#' @noRd
.validate_cartoon_alpha_device <- function(
  alpha,
  capabilities = grDevices::dev.capabilities()
) {
  supported <- is.character(capabilities$masks) &&
    "alpha" %in% capabilities$masks &&
    isTRUE(capabilities$transformations)

  if (alpha != 1 && !supported) {
    cli::cli_abort(c(
      "The active graphics device does not support the {.field alpha} aesthetic.",
      "i" = "Use a device that supports alpha masks and transformations, such as {.fn grDevices::pdf} or {.fn grDevices::svg}."
    ))
  }

  invisible(alpha)
}

#' Apply transparency once to an isolated cartoon
#'
#' @param primitives Named list of native grid grobs in drawing order.
#' @param alpha Numeric whole-cartoon transparency.
#'
#' @returns A named list containing one isolated, alpha-masked group.
#' @noRd
.composite_cartoon_alpha <- function(primitives, alpha) {
  source <- grid::gTree(
    children = rlang::exec(grid::gList, !!!primitives),
    name = "glycan.alpha.source"
  )
  alpha_mask <- grid::as.mask(
    grid::rectGrob(
      gp = grid::gpar(
        col = NA,
        fill = scales::alpha("black", alpha)
      ),
      name = "glycan.alpha.mask"
    ),
    type = "alpha"
  )
  alpha_group <- grid::groupGrob(
    source,
    name = "glycan.alpha.group"
  )

  rlang::set_names(
    list(grid::grobTree(
      alpha_group,
      vp = grid::viewport(mask = alpha_mask),
      name = "glycan.alpha"
    )),
    "glycan.alpha"
  )
}

#' Normalize grid coordinates into the drawing viewport
#'
#' @param x Numeric coordinates.
#' @param range Expanded panel range.
#'
#' @returns Numeric coordinates between zero and one for values inside the
#'   panel.
#' @noRd
.normalize_cartoon_grid_coordinates <- function(x, range) {
  (x - range[[1]]) / diff(range)
}

#' Draw glycan connection segments
#'
#' @param grob A prepared `glycanGrob`.
#' @param layout Grid layout metadata from `.cartoon_grid_layout()`.
#' @param scale Positive whole-cartoon size multiplier.
#'
#' @returns A `segments` grob, or a null grob for empty input.
#' @noRd
.cartoon_grid_segments <- function(grob, layout, scale) {
  segments <- grob$connect_df
  if (nrow(segments) == 0) {
    return(grid::nullGrob())
  }

  grid::segmentsGrob(
    x0 = grid::unit(
      .normalize_cartoon_grid_coordinates(
        segments$start_x,
        layout$panel_ranges$x
      ),
      "native"
    ),
    y0 = grid::unit(
      .normalize_cartoon_grid_coordinates(
        segments$start_y,
        layout$panel_ranges$y
      ),
      "native"
    ),
    x1 = grid::unit(
      .normalize_cartoon_grid_coordinates(
        segments$end_x,
        layout$panel_ranges$x
      ),
      "native"
    ),
    y1 = grid::unit(
      .normalize_cartoon_grid_coordinates(
        segments$end_y,
        layout$panel_ranges$y
      ),
      "native"
    ),
    gp = grid::gpar(
      col = scales::alpha("black", segments$transparency),
      fill = scales::alpha("black", segments$transparency),
      lwd = rep(
        grob$edge_linewidth * ggplot2::.pt * scale,
        nrow(segments)
      ),
      lty = rep(1, nrow(segments)),
      lineend = "butt",
      linejoin = "round"
    ),
    name = "glycan.edges"
  )
}

#' Draw glycan residue polygons and native circles
#'
#' @param grob A prepared `glycanGrob`.
#' @param layout Grid layout metadata from `.cartoon_grid_layout()`.
#' @param scale Positive whole-cartoon size multiplier.
#' @param fill,colour,alpha Per-point graphical properties.
#' @param name Grob name.
#'
#' @returns A grob tree containing polygon and circle grobs as needed.
#' @noRd
.cartoon_grid_polygons <- function(
  grob,
  layout,
  scale,
  fill,
  colour,
  alpha,
  name
) {
  polygons <- grob$polygon_coor
  group_levels <- sort(unique(polygons$group))
  group <- match(polygons$group, group_levels)
  polygon_order <- order(group)
  polygons <- polygons[polygon_order, , drop = FALSE]
  group <- group[polygon_order]
  fill <- fill[polygon_order]
  colour <- colour[polygon_order]
  apply_alpha <- !is.null(alpha)
  if (apply_alpha) {
    alpha <- alpha[polygon_order]
  }
  first <- !duplicated(group)
  group_count <- sum(first)
  group_colour <- colour[first]
  group_fill <- fill[first]
  if (apply_alpha) {
    group_colour <- scales::alpha(group_colour, alpha[first])
    group_fill <- scales::alpha(group_fill, alpha[first])
  }

  polygon_rows <- polygons$primitive == "polygon"
  polygon_grob <- if (!any(polygon_rows)) {
    grid::nullGrob()
  } else {
    polygon_group <- group[polygon_rows]
    polygon_ids <- sort(unique(polygon_group))
    polygon_group <- match(polygon_group, polygon_ids)
    polygon_first <- !duplicated(group[polygon_rows])

    grid::polygonGrob(
      x = grid::unit(
        .normalize_cartoon_grid_coordinates(
          polygons$point_x[polygon_rows],
          layout$panel_ranges$x
        ),
        "native"
      ),
      y = grid::unit(
        .normalize_cartoon_grid_coordinates(
          polygons$point_y[polygon_rows],
          layout$panel_ranges$y
        ),
        "native"
      ),
      id = polygon_group,
      gp = grid::gpar(
        col = group_colour[polygon_ids],
        fill = group_fill[polygon_ids],
        lwd = rep(
          grob$node_linewidth * ggplot2::.pt * scale,
          sum(polygon_first)
        ),
        lty = rep(1, sum(polygon_first)),
        lineend = "butt",
        linejoin = "round",
        linemitre = 10
      ),
      name = paste0(name, ".polygon")
    )
  }

  circle_groups <- which(first & polygons$primitive == "circle")
  circle_grob <- if (length(circle_groups) == 0) {
    grid::nullGrob()
  } else {
    circle_ids <- group[circle_groups]
    grid::circleGrob(
      x = grid::unit(
        .normalize_cartoon_grid_coordinates(
          polygons$center_x[circle_groups],
          layout$panel_ranges$x
        ),
        "native"
      ),
      y = grid::unit(
        .normalize_cartoon_grid_coordinates(
          polygons$center_y[circle_groups],
          layout$panel_ranges$y
        ),
        "native"
      ),
      r = grid::unit(
        .cartoon_circle_radius_inches(
          polygons$radius[circle_groups],
          scale
        ),
        "in"
      ),
      gp = grid::gpar(
        col = group_colour[circle_ids],
        fill = group_fill[circle_ids],
        lwd = rep(
          grob$node_linewidth * ggplot2::.pt * scale,
          length(circle_groups)
        ),
        lty = rep(1, length(circle_groups)),
        lineend = "butt",
        linejoin = "round"
      ),
      name = paste0(name, ".circle")
    )
  }

  grid::grobTree(polygon_grob, circle_grob, name = name)
}

#' Draw glycan text annotations
#'
#' @param grob A prepared `glycanGrob`.
#' @param layout Grid layout metadata from `.cartoon_grid_layout()`.
#' @param scale Positive whole-cartoon size multiplier.
#'
#' @returns A `text` grob, or a null grob for empty input.
#' @noRd
.cartoon_grid_text <- function(grob, layout, scale) {
  annotation <- if (grob$show_linkage) {
    grob$annotation_data$annotation
  } else {
    grob$annotation_data$show_without_linkage
  }
  if (nrow(annotation) == 0) {
    return(grid::nullGrob())
  }
  labels <- .font_family_annotation_labels(annotation, grob$font_family)

  grid::textGrob(
    label = parse(text = labels),
    x = grid::unit(
      .normalize_cartoon_grid_coordinates(
        annotation$x,
        layout$panel_ranges$x
      ),
      "native"
    ),
    y = grid::unit(
      .normalize_cartoon_grid_coordinates(
        annotation$y,
        layout$panel_ranges$y
      ),
      "native"
    ),
    just = "centre",
    hjust = annotation$hjust,
    vjust = annotation$vjust,
    rot = annotation$angle,
    gp = grid::gpar(
      col = scales::alpha("black", annotation$transparency),
      fontsize = annotation$text_size * ggplot2::.pt * scale,
      fontfamily = rep(grob$font_family, nrow(annotation)),
      lineheight = rep(1.2, nrow(annotation)),
      font = rep(1, nrow(annotation))
    ),
    name = "glycan.annotations"
  )
}

#' Draw a wavy reducing-end annotation
#'
#' @param grob A prepared `glycanGrob`.
#' @param layout Grid layout metadata from `.cartoon_grid_layout()`.
#' @param scale Positive whole-cartoon size multiplier.
#'
#' @returns A `polyline` grob, or a null grob when no wave is requested.
#' @noRd
.cartoon_grid_wave <- function(grob, layout, scale) {
  wave <- grob$annotation_data$reducing_info$wave
  if (nrow(wave) == 0) {
    return(grid::nullGrob())
  }

  grid::polylineGrob(
    x = grid::unit(
      .normalize_cartoon_grid_coordinates(
        wave$x,
        layout$panel_ranges$x
      ),
      "native"
    ),
    y = grid::unit(
      .normalize_cartoon_grid_coordinates(
        wave$y,
        layout$panel_ranges$y
      ),
      "native"
    ),
    id = rep(1, nrow(wave)),
    gp = grid::gpar(
      col = "#000000",
      fill = "#000000",
      lwd = grob$edge_linewidth * ggplot2::.pt * scale,
      lty = 1,
      lineend = "butt",
      linejoin = "round",
      linemitre = 10
    ),
    name = "glycan.reducing.wave"
  )
}

#' Measure native glycan grid width
#'
#' @param x A `glycan_grid_grob`.
#'
#' @returns The fixed native glycan width.
#' @noRd
#' @exportS3Method grid::widthDetails
widthDetails.glycan_grid_grob <- function(x) {
  x$width
}

#' Measure native glycan grid height
#'
#' @param x A `glycan_grid_grob`.
#'
#' @returns The fixed native glycan height.
#' @noRd
#' @exportS3Method grid::heightDetails
heightDetails.glycan_grid_grob <- function(x) {
  x$height
}
