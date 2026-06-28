#' Match the coordinates of glycan shape
#'
#' @param gly_list a list
#'
#' @returns the coordinate list of glycan shape
#'
#' @examples .create_polygon_coor(gly_list)
#' @noRd
.create_polygon_coor <- function(gly_list, point_size) {
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

#' Prepare validated inputs for glycan cartoon plotting
#'
#' @param structure A glycan structure or structure string.
#' @param highlight An integer vector of highlighted vertex indices.
#' @param orient Drawing orientation.
#' @param red_end Reducing-end annotation.
#'
#' @returns A list with structure graph, coordinates, highlight, and orientation.
#' @noRd
.prepare_cartoon_input <- function(
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

  structure <- .ensure_one_structure(structure)
  structure <- glyrepr::get_structure_graphs(structure, return_list = FALSE)
  highlight <- .ensure_highlight_para(highlight, length(structure))
  orient <- rlang::arg_match(orient)

  list(
    structure = structure,
    coor = .cartoon_coordinates(structure, orient),
    highlight = highlight,
    orient = orient
  )
}

#' Calculate cartoon coordinates for a drawing orientation
#'
#' @param structure An igraph glycan structure.
#' @param orient Drawing orientation.
#'
#' @returns A coordinate matrix.
#' @noRd
.cartoon_coordinates <- function(structure, orient = c("H", "V")) {
  orient <- rlang::arg_match(orient)
  coor <- .coor_cal(structure)
  if (orient == "H") {
    return(coor)
  }

  rotated <- coor
  rotated[, 1] <- coor[, 2]
  rotated[, 2] <- -coor[, 1]
  rotated
}

#' Build residue-level drawing data for a cartoon
#'
#' @param structure An igraph glycan structure.
#' @param coor A coordinate matrix.
#' @param highlight An integer vector of highlighted vertex indices.
#'
#' @returns A data frame of residue centers, glycoforms, and transparency.
#' @noRd
.cartoon_gly_list <- function(structure, coor, highlight = NULL) {
  gly_list <- data.frame(coor, glycoform = .glycoform_info(structure))
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

#' Build annotation data for a glycan cartoon
#'
#' @param structure An igraph glycan structure.
#' @param coor A coordinate matrix.
#' @param orient Drawing orientation.
#' @param red_end Reducing-end annotation.
#' @param highlight An integer vector of highlighted vertex indices.
#'
#' @returns A list with prepared annotation tables and reducing-end data.
#' @noRd
.cartoon_annotation_data <- function(
  structure,
  coor,
  orient = c("H", "V"),
  red_end = "",
  highlight = NULL
) {
  orient <- rlang::arg_match(orient)
  struc_annotation <- dplyr::bind_rows(
    .gly_annotation(structure, coor),
    .substituent_annotation(structure, coor, orient)
  )
  reducing_info <- .reducing_end_annotation(structure, coor, orient, red_end)
  struc_annotation <- dplyr::bind_rows(
    struc_annotation,
    reducing_info$annotation
  )
  struc_annotation <- .resolve_annotation_overlap(struc_annotation)
  struc_annotation <- .apply_annotation_transparency(
    struc_annotation,
    highlight
  )
  struc_annotation <- .normalize_cartoon_annotations(struc_annotation)

  list(
    annotation = struc_annotation,
    red_end_text = dplyr::filter(struc_annotation, .data$is_red_end_text),
    reducing_info = reducing_info
  )
}

#' Apply highlight transparency to annotation rows
#'
#' @param annotation A cartoon annotation data frame.
#' @param highlight An integer vector of highlighted vertex indices.
#'
#' @returns The annotation data frame with a transparency column.
#' @noRd
.apply_annotation_transparency <- function(annotation, highlight = NULL) {
  if (is.null(highlight)) {
    annotation$transparency <- 1
  } else {
    annotation$transparency <- (annotation$vertice %in% highlight) * 0.7 + 0.3
  }
  annotation
}

#' Normalize cartoon annotations for plotmath text drawing
#'
#' @param annotation A cartoon annotation data frame.
#'
#' @returns The annotation data frame with text drawing columns.
#' @noRd
.normalize_cartoon_annotations <- function(annotation) {
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
        !.is_parseable_annotation(.data$annot) ~ .quote_annotation(.data$annot),
        TRUE ~ .data$annot
      )
    )
}

#' Build connection segment data for a glycan cartoon
#'
#' @param structure An igraph glycan structure.
#' @param coor A coordinate matrix.
#' @param reducing_segment A data frame for the reducing-end segment.
#' @param gly_list A data frame returned by `.cartoon_gly_list()`.
#'
#' @returns A data frame of segment coordinates and transparency.
#' @noRd
.cartoon_connection_data <- function(
  structure,
  coor,
  reducing_segment,
  gly_list
) {
  gly_connect <- .connect_info(structure, coor)
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

#' Build the complete ggplot object for a glycan cartoon
#'
#' @param connect_df A connection segment data frame.
#' @param polygon_coor A residue polygon coordinate data frame.
#' @param filled_color Polygon fill colors.
#' @param annotation_data A list returned by `.cartoon_annotation_data()`.
#' @param show_linkage Show linkage annotations.
#'
#' @returns A `glydraw_cartoon` ggplot object.
#' @noRd
.build_cartoon_plot <- function(
  connect_df,
  polygon_coor,
  filled_color,
  annotation_data,
  show_linkage
) {
  gly_graph <- .cartoon_base_plot(connect_df, polygon_coor, filled_color)
  gly_graph <- .add_cartoon_annotation_layers(
    gly_graph,
    annotation_data,
    show_linkage
  )
  gly_graph <- .add_reducing_end_layers(
    gly_graph,
    annotation_data$reducing_info
  )
  .finalize_cartoon_plot(gly_graph)
}

#' Build base layers for a glycan cartoon
#'
#' @param connect_df A connection segment data frame.
#' @param polygon_coor A residue polygon coordinate data frame.
#' @param filled_color Polygon fill colors.
#'
#' @returns A ggplot object.
#' @noRd
.cartoon_base_plot <- function(connect_df, polygon_coor, filled_color) {
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

#' Add text annotation layers to a glycan cartoon
#'
#' @param plot A ggplot object.
#' @param annotation_data A list returned by `.cartoon_annotation_data()`.
#' @param show_linkage Show linkage annotations.
#'
#' @returns A ggplot object.
#' @noRd
.add_cartoon_annotation_layers <- function(
  plot,
  annotation_data,
  show_linkage
) {
  if (show_linkage) {
    return(.add_cartoon_text_layer(plot, annotation_data$annotation))
  }
  if (nrow(annotation_data$red_end_text) > 0) {
    return(.add_cartoon_text_layer(plot, annotation_data$red_end_text))
  }
  plot
}

#' Add one text annotation layer to a glycan cartoon
#'
#' @param plot A ggplot object.
#' @param annotation A cartoon annotation data frame.
#'
#' @returns A ggplot object.
#' @noRd
.add_cartoon_text_layer <- function(plot, annotation) {
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

#' Add reducing-end wave and bound layers to a glycan cartoon
#'
#' @param plot A ggplot object.
#' @param reducing_info A list returned by `.reducing_end_annotation()`.
#'
#' @returns A ggplot object.
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

#' Apply fixed sizing and class metadata to a cartoon plot
#'
#' @param plot A ggplot object.
#' @param dpi Dots per inch for pixel conversion.
#' @param border_px Plot border in pixels.
#'
#' @returns A `glydraw_cartoon` ggplot object.
#' @noRd
.finalize_cartoon_plot <- function(plot, dpi = 300, border_px = 50) {
  plot <- .apply_border(plot, border_px / dpi * 72)
  panel_size <- .decide_size(plot, border_px = 0)
  size <- .decide_size(plot, border_px = border_px)
  plot <- .apply_fixed_panel_size(plot, panel_size, dpi = dpi)
  attr(plot, "glydraw_panel_size_px") <- unlist(panel_size)
  attr(plot, "glydraw_size_px") <- unlist(size)
  structure(plot, class = c("glydraw_cartoon", class(plot)))
}

#' Decide fixed cartoon image size
#'
#' @param cartoon A glycan cartoon plot.
#' @param border_px Border size in pixels.
#'
#' @return A list with `width` and `height` values in pixels.
#' @noRd
.decide_size <- function(cartoon, border_px = 0) {
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

#' Apply plot margin border
#'
#' @param plot A ggplot object.
#' @param border_pt Border size in points.
#'
#' @return A ggplot object with plot margins applied.
#' @noRd
.apply_border <- function(plot, border_pt) {
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

#' Apply fixed ggplot2 panel size
#'
#' @param plot A ggplot2 object.
#' @param panel_size_px A named list or vector with `width` and `height` panel
#'   size in pixels.
#' @param dpi Dots per inch used to convert pixels to physical panel units.
#'
#' @return A ggplot2 object with fixed panel width and height.
#' @noRd
.apply_fixed_panel_size <- function(plot, panel_size_px, dpi) {
  plot +
    ggplot2::theme(
      panel.widths = grid::unit(panel_size_px[["width"]] / dpi, "in"),
      panel.heights = grid::unit(panel_size_px[["height"]] / dpi, "in")
    )
}
