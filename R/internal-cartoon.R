# Internal helpers for turning prepared glycan geometry into ggplot layers,
# fixed-size cartoon metadata, residue polygons, segments, and text layers.

.default_node_point_size <- 0.215
.default_cartoon_dpi <- 300
.default_cartoon_border_px <- 50
.cartoon_units_per_coordinate <- 3 * 118
.cartoon_panel_expansion <- 0.05
.node_size_linkage_threshold <- 1.4
.node_size_upper_boundary <- 2

#' Validate node-size input
#'
#' @param node_size Numeric node-size multiplier.
#'
#' @returns `node_size`, invisibly.
#' @noRd
.validate_node_size <- function(node_size) {
  checkmate::assert_number(node_size, lower = 0)
  if (node_size > .node_size_upper_boundary) {
    cli::cli_abort(
      "{.arg node_size} must be no larger than {.val {(.node_size_upper_boundary)}} because larger values make residues overlap."
    )
  }

  invisible(node_size)
}

#' Resolve whether text annotations can be shown for the requested node size
#'
#' @param show_linkage Logical scalar requested by the user.
#' @param node_size Numeric node-size multiplier.
#'
#' @returns A logical scalar indicating whether the regular text annotation
#'   layer should be drawn.
#' @noRd
.resolve_linkage_visibility <- function(show_linkage, node_size) {
  checkmate::assert_flag(show_linkage)
  if (!show_linkage || node_size <= .node_size_linkage_threshold) {
    return(show_linkage)
  }

  cli::cli_warn(c(
    "Linkage annotations are hidden because {.arg node_size} is larger than {.val {(.node_size_linkage_threshold)}}.",
    "i" = "Set {.arg show_linkage = FALSE} to silence this warning, or use a smaller {.arg node_size}."
  ))
  FALSE
}

#' Validate SNFG colors
#'
#' @param colors A named character vector of SNFG color values.
#'
#' @returns A named character vector of SNFG colors.
#' @noRd
.validate_colors <- function(colors = glydraw_colors()) {
  checkmate::assert_character(colors, any.missing = FALSE)
  color_names <- names(colors)
  expected_names <- names(glydraw_colors())
  if (
    is.null(color_names) ||
      length(color_names) != length(expected_names) ||
      anyDuplicated(color_names) ||
      !setequal(color_names, expected_names)
  ) {
    cli::cli_abort(
      "{.arg colors} must have exactly the names returned by {.fn glydraw_colors}."
    )
  }

  valid_colors <- vapply(
    colors,
    function(color) {
      !inherits(try(grDevices::col2rgb(color), silent = TRUE), "try-error")
    },
    logical(1)
  )
  if (any(!valid_colors)) {
    cli::cli_abort("{.arg colors} must contain only valid R colors.")
  }

  invisible(colors)
}

#' Resolve polygon fill colors
#'
#' @param polygon_coor A data frame returned by `.residue_polygon_data()`.
#' @param colors A named character vector returned by `.validate_colors()`.
#'
#' @returns A character vector of polygon fill colors, one value per row in
#'   `polygon_coor`.
#' @noRd
.resolve_residue_fill_colors <- function(
  polygon_coor,
  colors = glydraw_colors()
) {
  unname(colors[as.character(polygon_coor$color)])
}

#' Convert residue centers to polygon vertices
#'
#' @param gly_list A data frame with columns `center_x`, `center_y`,
#'   `mono`, `glycoform`, and `transparency`, usually from
#'   `.cartoon_residue_data()`.
#' @param point_size Numeric scale factor for SNFG shape templates.
#'
#' @returns A data frame with columns `point_x`, `point_y`, `center_x`,
#'   `center_y`, `radius`, `primitive`, `group`, `mono`, `color`, and `alpha`.
#'   Multi-part residue shapes contribute multiple groups.
#' @noRd
.residue_polygon_data <- function(gly_list, point_size) {
  # Progressively read and process lines in gly_list
  polygon_coor <- gly_list |>
    purrr::pmap_dfr(function(
      center_x,
      center_y,
      mono,
      glycoform,
      transparency
    ) {
      composition <- glycan_dict[[glycoform]][1] # Mapping the Composition of Glycoform, e.g.'Fuc'->'dHex'
      df1 <- data.frame(
        point_x = c(point_size * glycan_shape[[composition]]$x + center_x),
        point_y = c(point_size * glycan_shape[[composition]]$y + center_y),
        center_x = center_x,
        center_y = center_y,
        radius = point_size,
        primitive = if (composition == "Hex") "circle" else "polygon",
        # For Distinguishing the Coordinates of each point
        group = paste0(glycoform, center_x, "_", center_y),
        mono = mono,
        color = glycan_dict[[glycoform]][2],
        alpha = transparency
      )
      if (length(glycan_dict[[glycoform]]) > 2) {
        df2 <- data.frame(
          point_x = c(point_size * glycan_shape[[composition]]$xx + center_x),
          point_y = c(point_size * glycan_shape[[composition]]$yy + center_y),
          center_x = center_x,
          center_y = center_y,
          radius = point_size,
          primitive = "polygon",
          # For Distinguishing the Coordinates of each point
          group = paste0(glycoform, center_x, "_", center_y, 'remain'),
          mono = mono,
          color = glycan_dict[[glycoform]][3],
          alpha = transparency
        )
        df1 <- dplyr::bind_rows(df1, df2)
      }
      return(df1)
    })
  return(polygon_coor)
}

#' Convert a residue radius to its physical circle-grob size
#'
#' @param radius Numeric radius in cartoon coordinates.
#' @param scale Positive whole-cartoon size multiplier.
#'
#' @returns A numeric radius in inches.
#' @noRd
.cartoon_circle_radius_inches <- function(radius, scale = 1) {
  radius *
    .cartoon_units_per_coordinate /
    .default_cartoon_dpi *
    scale /
    (1 + 2 * .cartoon_panel_expansion)
}

#' Draw residue polygons and native circles in one ggplot2 layer
#'
#' @noRd
GeomGlydrawResidue <- ggplot2::ggproto(
  "GeomGlydrawResidue",
  ggplot2::Geom,
  required_aes = c(
    "x",
    "y",
    "center_x",
    "center_y",
    "radius",
    "primitive",
    "group"
  ),
  default_aes = ggplot2::aes(
    colour = "black",
    fill = "white",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),
  draw_key = ggplot2::draw_key_polygon,
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    polygon_data <- data[data$primitive == "polygon", , drop = FALSE]
    polygon_grob <- if (nrow(polygon_data) == 0) {
      grid::nullGrob()
    } else {
      ggplot2::GeomPolygon$draw_panel(
        polygon_data,
        panel_params,
        coord
      )
    }

    circle_data <- data[data$primitive == "circle", , drop = FALSE]
    circle_data <- circle_data[!duplicated(circle_data$group), , drop = FALSE]
    circle_grob <- if (nrow(circle_data) == 0) {
      grid::nullGrob()
    } else {
      centers <- circle_data
      centers$x <- centers$center_x
      centers$y <- centers$center_y
      centers <- coord$transform(centers, panel_params)

      grid::circleGrob(
        x = grid::unit(centers$x, "native"),
        y = grid::unit(centers$y, "native"),
        r = grid::unit(
          .cartoon_circle_radius_inches(circle_data$radius),
          "in"
        ),
        gp = grid::gpar(
          col = scales::alpha(circle_data$colour, circle_data$alpha),
          fill = scales::alpha(circle_data$fill, circle_data$alpha),
          lwd = circle_data$linewidth * ggplot2::.pt,
          lty = circle_data$linetype,
          lineend = "butt",
          linejoin = "round"
        )
      )
    }

    grid::grobTree(polygon_grob, circle_grob)
  }
)

#' Add a mixed native-circle and polygon residue layer
#'
#' @noRd
.geom_glydraw_residue <- function(
  data,
  fill,
  colour,
  linewidth,
  alpha = NULL
) {
  params <- list(
    fill = fill,
    colour = colour,
    linewidth = linewidth,
    na.rm = FALSE
  )
  if (!is.null(alpha)) {
    params$alpha <- alpha
  }

  ggplot2::layer(
    geom = GeomGlydrawResidue,
    stat = "identity",
    position = "identity",
    data = data,
    mapping = ggplot2::aes(
      x = .data$point_x,
      y = .data$point_y,
      center_x = .data$center_x,
      center_y = .data$center_y,
      radius = .data$radius,
      primitive = .data$primitive,
      group = .data$group
    ),
    inherit.aes = FALSE,
    params = params
  )
}

#' Prepare graph, coordinates, and options for one cartoon
#'
#' @param structure A scalar `glyrepr::glycan_structure()` or a scalar
#'   character structure string accepted by `glyparse::auto_parse()`.
#' @param highlight `NULL` or a numeric vector of 1-based vertex indices to
#'   highlight. Values are only honored when `structure` is already a
#'   glycan-structure object.
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#' @param red_end A non-missing string reducing-end annotation or `NULL`.
#'
#' @returns A list with `structure`, an igraph glycan graph; `coor`, a numeric
#'   coordinate matrix with columns `x` and `y`; `highlight`, the validated or
#'   cleared highlight vector; and `orient`, the matched orientation string.
#' @noRd
.prepare_cartoon_inputs <- function(
  structure,
  highlight,
  orient = c("left", "right", "up", "down"),
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
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#'
#' @returns A numeric matrix with columns `x` and `y`. `"left"` returns
#'   `.calculate_residue_coordinates()` unchanged; the other orientations
#'   rotate those coordinates around the reducing end.
#' @noRd
.oriented_cartoon_coordinates <- function(
  structure,
  orient = c("left", "right", "up", "down")
) {
  orient <- rlang::arg_match(orient)
  coor <- .calculate_residue_coordinates(structure)
  .rotate_cartoon_coordinates(coor, orient)
}

#' Rotate coordinates from the canonical left orientation
#'
#' @param coor A numeric matrix with columns `x` and `y`.
#' @param orient Glycan drawing orientation.
#'
#' @returns A numeric matrix with the same dimensions and dimnames as `coor`.
#' @noRd
.rotate_cartoon_coordinates <- function(coor, orient) {
  orient <- rlang::arg_match(
    orient,
    c("left", "right", "up", "down")
  )
  rotated <- coor
  if (orient == "left") {
    return(rotated)
  }
  if (orient == "right") {
    rotated[, "x"] <- -coor[, "x"]
    rotated[, "y"] <- -coor[, "y"]
    return(rotated)
  }
  if (orient == "up") {
    rotated[, "x"] <- coor[, "y"]
    rotated[, "y"] <- -coor[, "x"]
    return(rotated)
  }

  rotated[, "x"] <- -coor[, "y"]
  rotated[, "y"] <- coor[, "x"]
  rotated
}

#' Rotate a vector from the canonical left orientation
#'
#' @param vector A named numeric vector with elements `x` and `y`.
#' @param orient Glycan drawing orientation.
#'
#' @returns A named numeric vector with elements `x` and `y`.
#' @noRd
.rotate_cartoon_vector <- function(vector, orient) {
  vector <- matrix(
    c(vector[["x"]], vector[["y"]]),
    nrow = 1,
    dimnames = list(NULL, c("x", "y"))
  )
  as.vector(.rotate_cartoon_coordinates(vector, orient)[1, ]) |>
    stats::setNames(c("x", "y"))
}

#' Express oriented coordinates in the canonical left orientation
#'
#' @param coor A numeric matrix with columns `x` and `y`.
#' @param orient Glycan drawing orientation.
#'
#' @returns A numeric matrix with the same dimensions and dimnames as `coor`.
#' @noRd
.cartoon_coordinates_as_left <- function(coor, orient) {
  inverse <- switch(
    orient,
    left = "left",
    right = "right",
    up = "down",
    down = "up"
  )
  .rotate_cartoon_coordinates(coor, inverse)
}

#' Check whether a glycan orientation is horizontal
#'
#' @param orient Glycan drawing orientation.
#'
#' @returns A logical scalar.
#' @noRd
.is_horizontal_glycan_orientation <- function(orient) {
  orient %in% c("left", "right")
}

#' Check whether a glycan orientation is vertical
#'
#' @param orient Glycan drawing orientation.
#'
#' @returns A logical scalar.
#' @noRd
.is_vertical_glycan_orientation <- function(orient) {
  orient %in% c("up", "down")
}

#' Build residue center data for polygon drawing
#'
#' @param structure An igraph glycan graph.
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param highlight `NULL` or a numeric vector of 1-based vertex indices to
#'   highlight.
#' @param fuc_orient Fuc-like triangle orientation, either `"flex"` or `"up"`.
#'
#' @returns A data frame with columns `center_x`, `center_y`, `glycoform`, and
#'   `transparency`, one row per residue vertex.
#' @noRd
.cartoon_residue_data <- function(
  structure,
  coor,
  highlight = NULL,
  fuc_orient = c("flex", "up")
) {
  fuc_orient <- rlang::arg_match(fuc_orient)
  gly_list <- data.frame(
    coor,
    mono = igraph::V(structure)$mono,
    glycoform = .residue_glycoforms(structure, coor, fuc_orient)
  )
  if (!is.null(highlight)) {
    gly_list$transparency <- replace(
      rep(0.3, length(structure)),
      highlight,
      1.0
    )
  } else {
    gly_list$transparency <- 1.0
  }
  colnames(gly_list) <- c(
    "center_x",
    "center_y",
    "mono",
    "glycoform",
    "transparency"
  )
  gly_list
}

#' Build all text annotation data for a cartoon
#'
#' @param structure An igraph glycan graph.
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#' @param red_end A string reducing-end annotation, optionally containing one
#'   tagged amino-acid site.
#' @param red_end_length Length of the reducing-end line in plot coordinate
#'   units.
#' @param red_end_size Size of custom reducing-end text.
#' @param font_family Font family used for text annotations.
#' @param highlight `NULL` or a numeric vector of 1-based vertex indices to
#'   highlight.
#' @param node_size Numeric scalar used as a multiplier for the default node
#'   size.
#' @param show_linkage Logical scalar indicating whether linkage annotations
#'   will be drawn.
#'
#' @returns A list with `annotation`, the complete text annotation data frame;
#'   `show_without_linkage`, substituent and custom reducing-end text rows that
#'   remain visible when linkage labels are hidden; `bounds`, invisible bound
#'   points for text sizing; and `reducing_info`, the list returned by
#'   `.reducing_end_annotation_data()`.
#' @noRd
.cartoon_text_annotation_data <- function(
  structure,
  coor,
  orient = c("left", "right", "up", "down"),
  red_end = "",
  highlight = NULL,
  node_size = 1,
  show_linkage = TRUE,
  red_end_length = 0.6,
  red_end_size = 6,
  font_family = ""
) {
  orient <- rlang::arg_match(orient)
  substituent_annotation <- .substituent_annotation_data(
    structure,
    coor,
    orient,
    node_size = node_size
  )
  substituent_bounds <- .substituent_annotation_bounds(
    substituent_annotation,
    orient
  )
  substituent_annotation <- substituent_annotation |>
    dplyr::mutate(show_without_linkage = TRUE)
  reducing_info <- .reducing_end_annotation_data(
    structure,
    coor,
    orient,
    red_end,
    red_end_length,
    red_end_size,
    font_family
  )
  reducing_annotation <- reducing_info$annotation |>
    dplyr::mutate(show_without_linkage = .data$is_red_end_text)
  visible_without_linkage <- nrow(substituent_annotation) > 0 ||
    any(reducing_annotation$show_without_linkage)
  if (show_linkage || visible_without_linkage) {
    linkage_annotation <- .linkage_annotation_data(
      structure,
      coor,
      node_size = node_size,
      orient = orient
    ) |>
      dplyr::mutate(show_without_linkage = FALSE)
  } else {
    linkage_annotation <- .empty_linkage_annotation_data() |>
      dplyr::mutate(show_without_linkage = logical())
  }
  struc_annotation <- dplyr::bind_rows(
    linkage_annotation,
    substituent_annotation
  )
  struc_annotation <- dplyr::bind_rows(
    struc_annotation,
    reducing_annotation
  )
  struc_annotation <- .separate_overlapping_annotations(struc_annotation)
  struc_annotation <- .apply_highlight_to_annotations(
    struc_annotation,
    highlight
  )
  struc_annotation <- .prepare_plotmath_annotations(
    struc_annotation,
    red_end_size
  )

  list(
    annotation = struc_annotation,
    show_without_linkage = dplyr::filter(
      struc_annotation,
      .data$show_without_linkage
    ),
    bounds = substituent_bounds,
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
    annotation$transparency <- rep(1, nrow(annotation))
  } else {
    annotation$transparency <- (annotation$vertice %in% highlight) * 0.7 + 0.3
  }
  annotation
}

#' Prepare annotation labels for `geom_text(parse = TRUE)`
#'
#' @param annotation A data frame with columns `annot`, `hjust`, `vjust`, and
#'   `is_red_end_text`; missing justification and angle columns are allowed.
#' @param red_end_size Size of custom reducing-end text.
#'
#' @returns The same data frame columns as `annotation`, with normalized
#'   `is_red_end_text`, `is_aa_sequence`, `hjust`, `vjust`, and `angle`, plus
#'   character column `annot_label` containing plotmath-safe labels.
#' @noRd
.prepare_plotmath_annotations <- function(annotation, red_end_size = 6) {
  if (!"is_aa_sequence" %in% names(annotation)) {
    annotation$is_aa_sequence <- FALSE
  }
  if (!"angle" %in% names(annotation)) {
    annotation$angle <- 0
  }
  annotation |>
    dplyr::mutate(
      is_red_end_text = dplyr::if_else(
        is.na(.data$is_red_end_text),
        FALSE,
        .data$is_red_end_text
      ),
      is_aa_sequence = dplyr::if_else(
        is.na(.data$is_aa_sequence),
        FALSE,
        .data$is_aa_sequence
      ),
      hjust = dplyr::if_else(is.na(.data$hjust), 0.5, .data$hjust),
      vjust = dplyr::if_else(is.na(.data$vjust), 0.5, .data$vjust),
      angle = dplyr::if_else(is.na(.data$angle), 0, .data$angle),
      text_size = dplyr::if_else(
        .data$is_red_end_text,
        red_end_size,
        6
      ),
      annot_label = dplyr::case_when(
        .data$is_aa_sequence ~ .data$annot,
        .data$annot == "?" ~ '~"?"',
        .data$annot == "??" ~ '~"?"',
        grepl("^\\?\\d+", .data$annot) ~ '~"?"',
        !.can_parse_plotmath(.data$annot) ~ .quote_plotmath_text(.data$annot),
        TRUE ~ .data$annot
      )
    )
}

#' Resolve plotmath labels for a text annotation font family
#'
#' Plotmath renders named Greek symbols with its symbol font, ignoring the
#' selected text family. When a custom family is requested, quote alpha and beta
#' as Unicode text so they use the same family as the other annotations.
#'
#' @param annotation A prepared annotation data frame with `annot` and
#'   `annot_label` columns.
#' @param font_family Font family used for text annotations.
#'
#' @returns A character vector of plotmath-safe labels.
#' @noRd
.font_family_annotation_labels <- function(annotation, font_family) {
  labels <- annotation$annot_label
  if (identical(font_family, "")) {
    return(labels)
  }
  labels[annotation$annot == "alpha"] <- .quote_plotmath_text("\u03b1")
  labels[annotation$annot == "beta"] <- .quote_plotmath_text("\u03b2")
  labels
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
  connect_df$transparency <- gly_list$transparency[seq_len(nrow(connect_df))]
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
#' @param edge_linewidth Numeric scalar used for linkage lines.
#' @param node_linewidth Numeric scalar used for node borders.
#' @param font_family Font family used for text annotations.
#' @param border_px Numeric plot border size in pixels.
#' @param background Logical scalar indicating whether the ggplot background
#'   grob should be retained.
#'
#' @returns A `glydraw_cartoon` ggplot object with fixed-size metadata
#'   attributes.
#' @noRd
.assemble_cartoon_plot <- function(
  connect_df,
  polygon_coor,
  filled_color,
  annotation_data,
  show_linkage,
  edge_linewidth,
  node_linewidth,
  font_family,
  border_px = .default_cartoon_border_px,
  background = TRUE
) {
  gly_graph <- .cartoon_base_layers(
    connect_df,
    polygon_coor,
    filled_color,
    edge_linewidth,
    node_linewidth
  )
  gly_graph <- .add_cartoon_text_layers(
    gly_graph,
    annotation_data,
    show_linkage,
    font_family
  )
  gly_graph <- .add_cartoon_text_bounds(gly_graph, annotation_data$bounds)
  gly_graph <- .add_reducing_end_layers(
    gly_graph,
    annotation_data$reducing_info,
    edge_linewidth
  )
  if (!background) {
    gly_graph <- .remove_cartoon_background(gly_graph)
  }
  gly_graph <- .finalize_cartoon_size(gly_graph, border_px = border_px)
  attr(gly_graph, "glydraw_font_family") <- font_family
  gly_graph
}

#' Build segment and residue polygon layers
#'
#' @param connect_df A data frame with segment endpoints and `transparency`.
#' @param polygon_coor A data frame with polygon vertices, groups, and `alpha`.
#' @param filled_color A character vector of polygon fill colors, one value per
#'   row in `polygon_coor`.
#' @param edge_linewidth Numeric scalar used for linkage lines.
#' @param node_linewidth Numeric scalar used for node borders.
#'
#' @returns A ggplot object containing segment, white mask polygon, colored
#'   residue polygon, fixed coordinate, and blank theme layers.
#' @noRd
.cartoon_base_layers <- function(
  connect_df,
  polygon_coor,
  filled_color,
  edge_linewidth,
  node_linewidth
) {
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
      linewidth = edge_linewidth
    ) +
    .geom_glydraw_residue(
      data = polygon_coor,
      fill = "white",
      colour = "white",
      linewidth = node_linewidth
    ) +
    .geom_glydraw_residue(
      data = polygon_coor,
      alpha = polygon_coor$alpha,
      fill = filled_color,
      colour = scales::alpha("black", polygon_coor$alpha),
      linewidth = node_linewidth
    ) +
    ggplot2::coord_fixed(ratio = 1, clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}

#' Add the appropriate text layers to a cartoon
#'
#' @param plot A ggplot object.
#' @param annotation_data A list returned by `.cartoon_text_annotation_data()`.
#' @param show_linkage A logical scalar. `TRUE` draws all text; `FALSE` draws
#'   only substituent and custom reducing-end text when present.
#' @param font_family Font family used for text annotations.
#'
#' @returns A ggplot object with zero or one added text layer.
#' @noRd
.add_cartoon_text_layers <- function(
  plot,
  annotation_data,
  show_linkage,
  font_family
) {
  if (show_linkage) {
    return(.add_plotmath_text_layer(
      plot,
      annotation_data$annotation,
      font_family
    ))
  }
  if (nrow(annotation_data$show_without_linkage) > 0) {
    return(.add_plotmath_text_layer(
      plot,
      annotation_data$show_without_linkage,
      font_family
    ))
  }
  plot
}

#' Add one plotmath text layer
#'
#' @param plot A ggplot object.
#' @param annotation A data frame with columns `x`, `y`, `annot_label`,
#'   `hjust`, `vjust`, `angle`, and `transparency`.
#' @param font_family Font family used for text annotations.
#'
#' @returns A ggplot object with one `geom_text(parse = TRUE)` layer added.
#' @noRd
.add_plotmath_text_layer <- function(plot, annotation, font_family) {
  annotation$annot_label <- .font_family_annotation_labels(
    annotation,
    font_family
  )
  plot +
    ggplot2::geom_text(
      data = annotation,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$annot_label,
        hjust = .data$hjust,
        vjust = .data$vjust,
        angle = .data$angle
      ),
      alpha = annotation$transparency,
      parse = TRUE,
      size = annotation$text_size,
      family = font_family,
    )
}

#' Add invisible text bounds to a cartoon
#'
#' @param plot A ggplot object.
#' @param bounds A data frame with numeric columns `x` and `y`.
#'
#' @returns A ggplot object with one `geom_blank()` layer when bounds are
#'   available.
#' @noRd
.add_cartoon_text_bounds <- function(plot, bounds) {
  if (nrow(bounds) == 0) {
    return(plot)
  }
  plot +
    ggplot2::geom_blank(
      data = bounds,
      ggplot2::aes(x = .data$x, y = .data$y)
    )
}

#' Add reducing-end wave and invisible bound layers
#'
#' @param plot A ggplot object.
#' @param reducing_info A list returned by `.reducing_end_annotation_data()`.
#' @param edge_linewidth Numeric scalar used for linkage lines.
#'
#' @returns A ggplot object with a `geom_path()` wave layer when available and
#'   a `geom_blank()` bounds layer when bounds are available.
#' @noRd
.add_reducing_end_layers <- function(plot, reducing_info, edge_linewidth) {
  if (nrow(reducing_info$wave) > 0) {
    plot <- plot +
      ggplot2::geom_path(
        data = reducing_info$wave,
        ggplot2::aes(x = .data$x, y = .data$y),
        linewidth = edge_linewidth
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
.finalize_cartoon_size <- function(
  plot,
  dpi = 300,
  border_px = .default_cartoon_border_px
) {
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
  panel_width <- .cartoon_units_per_coordinate *
    diff(ggplot2::get_panel_scales(cartoon)$x$range$range)
  panel_height <- .cartoon_units_per_coordinate *
    diff(ggplot2::get_panel_scales(cartoon)$y$range$range)
  width <- panel_width + 2 * border_px
  height <- panel_height + 2 * border_px
  return(list(width = width, height = height))
}

#' Remove background grobs from a cartoon plot
#'
#' @param plot A ggplot2 object.
#'
#' @returns `plot` with blank panel and plot backgrounds.
#' @noRd
.remove_cartoon_background <- function(plot) {
  plot +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank()
    )
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
