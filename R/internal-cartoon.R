# Internal helpers for turning prepared glycan geometry into ggplot layers,
# fixed-size cartoon metadata, residue polygons, segments, and text layers.

#' Convert residue centers to polygon vertices
#'
#' @param gly_list A data frame with columns `center_x`, `center_y`,
#'   `glycoform`, and `transparency`, usually from `.cartoon_residue_data()`.
#' @param point_size Numeric scale factor for SNFG shape templates.
#'
#' @returns A data frame with columns `point_x`, `point_y`, `group`, `color`,
#'   and `alpha`. Multi-part residue shapes contribute multiple groups.
#' @noRd
.residue_polygon_data <- function(gly_list, point_size) {
  # Progressively read and process lines in gly_list
  polygon_coor <- gly_list |>
    purrr::pmap_dfr(function(center_x, center_y, glycoform, transparency) {
      composition <- glycan_dict[[glycoform]][1] # Mapping the Composition of Glycoform, e.g.'Fuc'->'dHex'
      df1 <- data.frame(
        point_x = c(point_size * glycan_shape[[composition]]$x + center_x),
        point_y = c(point_size * glycan_shape[[composition]]$y + center_y),
        # For Distinguishing the Coordinates of each point
        group = paste0(glycoform, center_x, "_", center_y),
        color = glycan_dict[[glycoform]][2],
        alpha = transparency
      )
      if (length(glycan_dict[[glycoform]]) > 2) {
        df2 <- data.frame(
          point_x = c(point_size * glycan_shape[[composition]]$xx + center_x),
          point_y = c(point_size * glycan_shape[[composition]]$yy + center_y),
          # For Distinguishing the Coordinates of each point
          group = paste0(glycoform, center_x, "_", center_y, 'remain'),
          color = glycan_dict[[glycoform]][3],
          alpha = transparency
        )
        df1 <- dplyr::bind_rows(df1, df2)
      }
      return(df1)
    })
  return(polygon_coor)
}

#' Prepare graph, coordinates, and options for one cartoon
#'
#' @param structure A scalar `glyrepr::glycan_structure()` or a scalar
#'   character structure string accepted by `glyparse::auto_parse()`.
#' @param highlight `NULL` or a numeric vector of 1-based vertex indices to
#'   highlight. Values are only honored when `structure` is already a
#'   glycan-structure object.
#' @param orient Drawing orientation, either `"H"` or `"V"`.
#' @param red_end A non-missing string reducing-end annotation.
#'
#' @returns A list with `structure`, an igraph glycan graph; `coor`, a numeric
#'   coordinate matrix with columns `x` and `y`; `highlight`, the validated or
#'   cleared highlight vector; and `orient`, the matched orientation string.
#' @noRd
.prepare_cartoon_inputs <- function(
  structure,
  highlight,
  orient = c("H", "V"),
  red_end = ""
) {
  checkmate::assert_string(red_end, na.ok = FALSE)
  if (!is.null(highlight) && !glyrepr::is_glycan_structure(structure)) {
    cli::cli_warn(
      "{.arg highlight} can only be set when {.arg structure} is a {.fn glyrepr::glycan_structure}."
    )
    highlight <- NULL
  }

  structure <- .as_single_glycan_structure(structure)
  structure <- glyrepr::get_structure_graphs(structure, return_list = FALSE)
  highlight <- .validate_highlight_indices(highlight, length(structure))
  orient <- rlang::arg_match(orient)

  list(
    structure = structure,
    coor = .oriented_cartoon_coordinates(structure, orient),
    highlight = highlight,
    orient = orient
  )
}

#' Rotate residue coordinates for the requested orientation
#'
#' @param structure An igraph glycan graph.
#' @param orient Drawing orientation, either `"H"` or `"V"`.
#'
#' @returns A numeric matrix with columns `x` and `y`. Horizontal orientation
#'   returns `.calculate_residue_coordinates()` unchanged; vertical orientation
#'   swaps axes and negates the old `x` coordinate.
#' @noRd
.oriented_cartoon_coordinates <- function(structure, orient = c("H", "V")) {
  orient <- rlang::arg_match(orient)
  coor <- .calculate_residue_coordinates(structure)
  if (orient == "H") {
    return(coor)
  }

  rotated <- coor
  rotated[, 1] <- coor[, 2]
  rotated[, 2] <- -coor[, 1]
  rotated
}

#' Build residue center data for polygon drawing
#'
#' @param structure An igraph glycan graph.
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param highlight `NULL` or a numeric vector of 1-based vertex indices to
#'   highlight.
#'
#' @returns A data frame with columns `center_x`, `center_y`, `glycoform`, and
#'   `transparency`, one row per residue vertex.
#' @noRd
.cartoon_residue_data <- function(structure, coor, highlight = NULL) {
  gly_list <- data.frame(coor, glycoform = .residue_glycoforms(structure))
  if (!is.null(highlight)) {
    gly_list$transparency <- replace(
      rep(0.3, length(structure)),
      highlight,
      1.0
    )
  } else {
    gly_list$transparency <- 1.0
  }
  colnames(gly_list) <- c("center_x", "center_y", "glycoform", "transparency")
  gly_list
}

#' Build all text annotation data for a cartoon
#'
#' @param structure An igraph glycan graph.
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param orient Drawing orientation, either `"H"` or `"V"`.
#' @param red_end A string reducing-end annotation.
#' @param highlight `NULL` or a numeric vector of 1-based vertex indices to
#'   highlight.
#'
#' @returns A list with `annotation`, the complete text annotation data frame;
#'   `red_end_text`, only custom reducing-end text rows; and `reducing_info`,
#'   the list returned by `.reducing_end_annotation_data()`.
#' @noRd
.cartoon_text_annotation_data <- function(
  structure,
  coor,
  orient = c("H", "V"),
  red_end = "",
  highlight = NULL
) {
  orient <- rlang::arg_match(orient)
  struc_annotation <- dplyr::bind_rows(
    .linkage_annotation_data(structure, coor),
    .substituent_annotation_data(structure, coor, orient)
  )
  reducing_info <- .reducing_end_annotation_data(
    structure,
    coor,
    orient,
    red_end
  )
  struc_annotation <- dplyr::bind_rows(
    struc_annotation,
    reducing_info$annotation
  )
  struc_annotation <- .separate_overlapping_annotations(struc_annotation)
  struc_annotation <- .apply_highlight_to_annotations(
    struc_annotation,
    highlight
  )
  struc_annotation <- .prepare_plotmath_annotations(struc_annotation)

  list(
    annotation = struc_annotation,
    red_end_text = dplyr::filter(struc_annotation, .data$is_red_end_text),
    reducing_info = reducing_info
  )
}

#' Apply highlight transparency to text annotations
#'
#' @param annotation A data frame with a `vertice` column and one row per text
#'   annotation.
#' @param highlight `NULL` or a numeric vector of 1-based vertex indices to
#'   highlight.
#'
#' @returns The same data frame columns as `annotation`, plus or updated with
#'   numeric column `transparency`.
#' @noRd
.apply_highlight_to_annotations <- function(annotation, highlight = NULL) {
  if (is.null(highlight)) {
    annotation$transparency <- 1
  } else {
    annotation$transparency <- (annotation$vertice %in% highlight) * 0.7 + 0.3
  }
  annotation
}

#' Prepare annotation labels for `geom_text(parse = TRUE)`
#'
#' @param annotation A data frame with columns `annot`, `hjust`, `vjust`, and
#'   `is_red_end_text`; missing justification columns are allowed.
#'
#' @returns The same data frame columns as `annotation`, with normalized
#'   `is_red_end_text`, `hjust`, and `vjust`, plus character column
#'   `annot_label` containing plotmath-safe labels.
#' @noRd
.prepare_plotmath_annotations <- function(annotation) {
  annotation |>
    dplyr::mutate(
      is_red_end_text = dplyr::if_else(
        is.na(.data$is_red_end_text),
        FALSE,
        .data$is_red_end_text
      ),
      hjust = dplyr::if_else(is.na(.data$hjust), 0.5, .data$hjust),
      vjust = dplyr::if_else(is.na(.data$vjust), 0.5, .data$vjust),
      annot_label = dplyr::case_when(
        .data$annot == "?" ~ '~"?"',
        .data$annot == "??" ~ '~"?"',
        grepl("^\\?\\d+", .data$annot) ~ '~"?"',
        !.can_parse_plotmath(.data$annot) ~ .quote_plotmath_text(.data$annot),
        TRUE ~ .data$annot
      )
    )
}

#' Build all line segments for a cartoon
#'
#' @param structure An igraph glycan graph.
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param reducing_segment A data frame with columns `start_x`, `start_y`,
#'   `end_x`, and `end_y` for the reducing-end segment.
#' @param gly_list A data frame returned by `.cartoon_residue_data()`.
#'
#' @returns A data frame with columns `start_x`, `start_y`, `end_x`, `end_y`,
#'   and `transparency`, including glycosidic segments and the reducing-end
#'   segment.
#' @noRd
.cartoon_segment_data <- function(
  structure,
  coor,
  reducing_segment,
  gly_list
) {
  gly_connect <- .connection_segment_data(structure, coor)
  connect_df <- data.frame(
    start_x = gly_connect$start_x,
    start_y = gly_connect$start_y,
    end_x = gly_connect$end_x,
    end_y = gly_connect$end_y
  )
  connect_df <- dplyr::bind_rows(connect_df, reducing_segment)
  connect_df$transparency <- gly_list$transparency
  connect_df
}

#' Assemble the complete cartoon plot
#'
#' @param connect_df A data frame returned by `.cartoon_segment_data()`.
#' @param polygon_coor A data frame returned by `.residue_polygon_data()`.
#' @param filled_color A character vector of polygon fill colors, one value per
#'   row in `polygon_coor`.
#' @param annotation_data A list returned by `.cartoon_text_annotation_data()`.
#' @param show_linkage A logical scalar indicating whether linkage annotations
#'   should be drawn.
#'
#' @returns A `glydraw_cartoon` ggplot object with fixed-size metadata
#'   attributes.
#' @noRd
.assemble_cartoon_plot <- function(
  connect_df,
  polygon_coor,
  filled_color,
  annotation_data,
  show_linkage
) {
  gly_graph <- .cartoon_base_layers(connect_df, polygon_coor, filled_color)
  gly_graph <- .add_cartoon_text_layers(
    gly_graph,
    annotation_data,
    show_linkage
  )
  gly_graph <- .add_reducing_end_layers(
    gly_graph,
    annotation_data$reducing_info
  )
  .finalize_cartoon_size(gly_graph)
}

#' Build segment and residue polygon layers
#'
#' @param connect_df A data frame with segment endpoints and `transparency`.
#' @param polygon_coor A data frame with polygon vertices, groups, and `alpha`.
#' @param filled_color A character vector of polygon fill colors, one value per
#'   row in `polygon_coor`.
#'
#' @returns A ggplot object containing segment, white mask polygon, colored
#'   residue polygon, fixed coordinate, and blank theme layers.
#' @noRd
.cartoon_base_layers <- function(connect_df, polygon_coor, filled_color) {
  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = connect_df,
      ggplot2::aes(
        x = .data$start_x,
        y = .data$start_y,
        xend = .data$end_x,
        yend = .data$end_y
      ),
      alpha = connect_df$transparency,
      linewidth = 0.8
    ) +
    ggplot2::geom_polygon(
      data = polygon_coor,
      ggplot2::aes(x = .data$point_x, y = .data$point_y, group = .data$group),
      fill = "white",
      color = "white",
      linewidth = 0.8
    ) +
    ggplot2::geom_polygon(
      data = polygon_coor,
      ggplot2::aes(x = .data$point_x, y = .data$point_y, group = .data$group),
      alpha = polygon_coor$alpha,
      fill = filled_color,
      color = scales::alpha("black", polygon_coor$alpha),
      linewidth = 0.8
    ) +
    ggplot2::coord_fixed(ratio = 1, clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}

#' Add the appropriate text layers to a cartoon
#'
#' @param plot A ggplot object.
#' @param annotation_data A list returned by `.cartoon_text_annotation_data()`.
#' @param show_linkage A logical scalar. `TRUE` draws all linkage and
#'   reducing-end text; `FALSE` draws only custom reducing-end text when
#'   present.
#'
#' @returns A ggplot object with zero or one added text layer.
#' @noRd
.add_cartoon_text_layers <- function(
  plot,
  annotation_data,
  show_linkage
) {
  if (show_linkage) {
    return(.add_plotmath_text_layer(plot, annotation_data$annotation))
  }
  if (nrow(annotation_data$red_end_text) > 0) {
    return(.add_plotmath_text_layer(plot, annotation_data$red_end_text))
  }
  plot
}

#' Add one plotmath text layer
#'
#' @param plot A ggplot object.
#' @param annotation A data frame with columns `x`, `y`, `annot_label`,
#'   `hjust`, `vjust`, and `transparency`.
#'
#' @returns A ggplot object with one `geom_text(parse = TRUE)` layer added.
#' @noRd
.add_plotmath_text_layer <- function(plot, annotation) {
  plot +
    ggplot2::geom_text(
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
    )
}

#' Add reducing-end wave and invisible bound layers
#'
#' @param plot A ggplot object.
#' @param reducing_info A list returned by `.reducing_end_annotation_data()`.
#'
#' @returns A ggplot object with a `geom_path()` wave layer when available and
#'   a `geom_blank()` bounds layer when bounds are available.
#' @noRd
.add_reducing_end_layers <- function(plot, reducing_info) {
  if (nrow(reducing_info$wave) > 0) {
    plot <- plot +
      ggplot2::geom_path(
        data = reducing_info$wave,
        ggplot2::aes(x = .data$x, y = .data$y),
        linewidth = 0.8
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

#' Add fixed-size metadata to a cartoon plot
#'
#' @param plot A ggplot object.
#' @param dpi Numeric dots per inch used to convert pixels to inches.
#' @param border_px Numeric plot border size in pixels.
#'
#' @returns A `glydraw_cartoon` ggplot object with attributes
#'   `glydraw_panel_size_px` and `glydraw_size_px`, each a named numeric vector
#'   with `width` and `height`.
#' @noRd
.finalize_cartoon_size <- function(plot, dpi = 300, border_px = 50) {
  plot <- .add_plot_border(plot, border_px / dpi * 72)
  panel_size <- .cartoon_size_pixels(plot, border_px = 0)
  size <- .cartoon_size_pixels(plot, border_px = border_px)
  plot <- .set_fixed_panel_size(plot, panel_size, dpi = dpi)
  attr(plot, "glydraw_panel_size_px") <- unlist(panel_size)
  attr(plot, "glydraw_size_px") <- unlist(size)
  structure(plot, class = c("glydraw_cartoon", class(plot)))
}

#' Calculate fixed cartoon size in pixels
#'
#' @param cartoon A ggplot object whose panel scales have been trained.
#' @param border_px Numeric border size in pixels to add on every side.
#'
#' @return A list with numeric `width` and `height` values in pixels.
#' @noRd
.cartoon_size_pixels <- function(cartoon, border_px = 0) {
  panel_width <- 3 *
    118 *
    diff(ggplot2::get_panel_scales(cartoon)$x$range$range)
  panel_height <- 3 *
    118 *
    diff(ggplot2::get_panel_scales(cartoon)$y$range$range)
  width <- panel_width + 2 * border_px
  height <- panel_height + 2 * border_px
  return(list(width = width, height = height))
}

#' Add a fixed plot margin border
#'
#' @param plot A ggplot object.
#' @param border_pt Numeric border size in points.
#'
#' @return A ggplot object. When `border_pt` is positive, the plot has equal
#'   top, right, bottom, and left margins; otherwise it is returned unchanged.
#' @noRd
.add_plot_border <- function(plot, border_pt) {
  if (is.null(border_pt) || border_pt <= 0) {
    return(plot)
  }
  plot +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        t = border_pt,
        r = border_pt,
        b = border_pt,
        l = border_pt,
        unit = "pt"
      )
    )
}

#' Set fixed ggplot2 panel dimensions
#'
#' @param plot A ggplot2 object.
#' @param panel_size_px A named list or vector with numeric `width` and
#'   `height` panel size in pixels.
#' @param dpi Numeric dots per inch used to convert pixels to inches.
#'
#' @return A ggplot2 object with `panel.widths` and `panel.heights` theme
#'   entries set as grid units.
#' @noRd
.set_fixed_panel_size <- function(plot, panel_size_px, dpi) {
  plot +
    ggplot2::theme(
      panel.widths = grid::unit(panel_size_px[["width"]] / dpi, "in"),
      panel.heights = grid::unit(panel_size_px[["height"]] / dpi, "in")
    )
}
