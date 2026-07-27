# ComplexHeatmap annotation helpers for drawing glycan row and column labels.

#' Use glycan cartoons as ComplexHeatmap labels
#'
#' `anno_glycan()` creates a [ComplexHeatmap::AnnotationFunction()] that draws
#' glycan cartoons in the order used by a ComplexHeatmap heatmap. The cartoons
#' follow row or column clustering, reordering, and splitting.
#'
#' Column annotations use the visual defaults of [scale_x_glycan()]: vertical
#' cartoons anchored at their reducing ends and aligned along their bottom
#' bounds. Row annotations use the defaults of [scale_y_glycan()]: horizontal
#' cartoons aligned along their right bounds and anchored at their reducing
#' ends. `side` should match the heatmap annotation side and defaults to
#' `"bottom"` for columns and `"left"` for rows.
#'
#' The required annotation width or height is calculated from the largest
#' rendered cartoon, including rotation and perpendicular nudging. Supply
#' `width` for row annotations or `height` for column annotations to override
#' the calculated size.
#'
#' @param structure A character vector of glycan structure strings supported by
#'   [glyparse::auto_parse()] or a [glyrepr::glycan_structure()] vector. Its
#'   order must match the rows or columns of the heatmap matrix.
#' @param which Whether the cartoons label heatmap `"column"` or `"row"`
#'   observations.
#' @param side Side on which the annotation is placed. Column annotations
#'   accept `"bottom"` or `"top"`; row annotations accept `"left"` or
#'   `"right"`. Defaults to the corresponding glycan scale position.
#' @param size Positive scalar that uniformly scales each cartoon. Defaults to
#'   `0.4`.
#' @param angle Rotation in degrees applied to each cartoon independently of
#'   its drawing orientation. Defaults to `0`.
#' @param hjust Horizontal justification. `NULL` uses [hjust_red_end()] for
#'   column labels and `1` for row labels.
#' @param vjust Vertical justification. `NULL` uses `0` for column labels and
#'   [vjust_red_end()] for row labels.
#' @param nudge_x Horizontal adjustment of each cartoon, in millimetres.
#'   Positive values move cartoons to the right. Defaults to `0`.
#' @param nudge_y Vertical adjustment of each cartoon, in millimetres. Positive
#'   values move cartoons upward. Defaults to `0`.
#' @param show_linkage Whether to show glycosidic linkage annotations inside
#'   the cartoons. Defaults to `TRUE`.
#' @param style A [glydraw_style()] object that controls the cartoons' visual
#'   appearance.
#' @param width Optional [grid::unit()] width for a row annotation. `NULL`
#'   calculates the width from the rendered cartoons.
#' @param height Optional [grid::unit()] height for a column annotation. `NULL`
#'   calculates the height from the rendered cartoons.
#' @param show_name Whether ComplexHeatmap should show the annotation name.
#'   Defaults to `FALSE` because the cartoons normally replace row or column
#'   names.
#'
#' @returns A ComplexHeatmap `AnnotationFunction` object.
#'
#' @examples
#' if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {
#'   mat <- matrix(
#'     seq_len(9),
#'     nrow = 3,
#'     dimnames = list(paste0("row", 1:3), paste0("column", 1:3))
#'   )
#'   structures <- c(
#'     "GlcNAc(??-",
#'     "Gal(??-?)GlcNAc(??-",
#'     "Neu5Ac(??-?)Gal(??-?)GlcNAc(??-"
#'   )
#'
#'   ComplexHeatmap::Heatmap(
#'     mat,
#'     show_row_names = FALSE,
#'     show_column_names = FALSE,
#'     left_annotation = ComplexHeatmap::rowAnnotation(
#'       glycan = anno_glycan(structures, which = "row")
#'     ),
#'     bottom_annotation = ComplexHeatmap::HeatmapAnnotation(
#'       glycan = anno_glycan(structures, which = "column")
#'     )
#'   )
#' }
#' @export
anno_glycan <- function(
  structure,
  which = c("column", "row"),
  side = NULL,
  size = 0.4,
  angle = 0,
  hjust = NULL,
  vjust = NULL,
  nudge_x = 0,
  nudge_y = 0,
  show_linkage = TRUE,
  style = glydraw_style(),
  width = NULL,
  height = NULL,
  show_name = FALSE
) {
  if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    cli::cli_abort(
      "{.pkg ComplexHeatmap} is required to use {.fn anno_glycan}."
    )
  }
  .validate_glycan_annotation_structures(structure)
  which <- rlang::arg_match(which)
  side <- .resolve_glycan_annotation_side(side, which)
  orient <- switch(which, column = "up", row = "left")
  if (is.null(hjust)) {
    hjust <- switch(which, column = hjust_red_end(), row = 1)
  }
  if (is.null(vjust)) {
    vjust <- switch(which, column = 0, row = vjust_red_end())
  }
  checkmate::assert_flag(show_name)

  options <- .validate_glycan_label_options(
    orient = orient,
    size = size,
    angle = angle,
    hjust = hjust,
    vjust = vjust,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    show_linkage = show_linkage,
    red_end = style$red_end,
    fuc_orient = style$fuc_orient,
    edge_linewidth = style$edge_linewidth,
    node_linewidth = style$node_linewidth,
    node_size = style$node_size,
    font_family = style$font_family,
    colors = style$colors
  )
  params <- .glycan_annotation_label_params(options, side)
  grobs <- .build_glycan_annotation_grobs(structure, params)

  if (identical(which, "row") && is.null(width)) {
    width <- .glycan_label_extent(grobs, "width")
  }
  if (identical(which, "column") && is.null(height)) {
    height <- .glycan_label_extent(grobs, "height")
  }

  fun <- function(index, k, n) {
    grid::grid.draw(
      .glycan_annotation_grob(grobs, index, which, side)
    )
  }
  ComplexHeatmap::AnnotationFunction(
    fun = fun,
    fun_name = "anno_glycan",
    which = which,
    var_import = list(
      grobs = grobs,
      which = which,
      side = side,
      .glycan_annotation_grob = .glycan_annotation_grob
    ),
    n = length(grobs),
    data_scale = c(0.5, length(grobs) + 0.5),
    subset_rule = list(grobs = .subset_glycan_annotation_grobs),
    show_name = show_name,
    width = width,
    height = height
  )
}

#' Validate glycan annotation structures
#'
#' @param structure Glycan structures supplied to the annotation.
#'
#' @returns `structure`, invisibly.
#' @noRd
.validate_glycan_annotation_structures <- function(structure) {
  if (
    !is.character(structure) &&
      !inherits(structure, "glyrepr_structure")
  ) {
    cli::cli_abort(
      "{.arg structure} must be a character or {.cls glyrepr_structure} vector."
    )
  }
  if (length(structure) == 0) {
    cli::cli_abort("{.arg structure} must contain at least one glycan.")
  }
  labels <- as.character(structure)
  if (anyNA(labels) || any(!nzchar(labels))) {
    cli::cli_abort(
      "{.arg structure} cannot contain missing or empty values."
    )
  }
  invisible(structure)
}

#' Resolve a ComplexHeatmap glycan annotation side
#'
#' @param side Requested heatmap side or `NULL`.
#' @param which Whether the annotation labels columns or rows.
#'
#' @returns A validated heatmap side.
#' @noRd
.resolve_glycan_annotation_side <- function(side, which) {
  valid_sides <- switch(
    which,
    column = c("bottom", "top"),
    row = c("left", "right")
  )
  if (is.null(side)) {
    return(valid_sides[[1]])
  }
  rlang::arg_match(side, valid_sides)
}

#' Translate glycan annotation options into label parameters
#'
#' @param options Validated glycan label options.
#' @param side Side on which the annotation is drawn.
#'
#' @returns A list consumed by `.new_glycan_label_grob()`.
#' @noRd
.glycan_annotation_label_params <- function(options, side) {
  list(
    glycan_orient = options$orient,
    glycan_size = options$size,
    glycan_angle = options$angle,
    glycan_hjust = options$hjust,
    glycan_vjust = options$vjust,
    glycan_nudge_x = options$nudge_x,
    glycan_nudge_y = options$nudge_y,
    glycan_show_linkage = options$show_linkage,
    glycan_red_end = options$red_end,
    glycan_fuc_orient = options$fuc_orient,
    glycan_edge_linewidth = options$edge_linewidth,
    glycan_node_linewidth = options$node_linewidth,
    glycan_node_size = options$node_size,
    glycan_font_family = options$font_family,
    glycan_colors = options$colors,
    position = side
  )
}

#' Build cached glycan annotation label grobs
#'
#' @param structure Glycan structures supplied to the annotation.
#' @param params Glycan label parameters.
#'
#' @returns A list containing one rendered grob per structure.
#' @noRd
.build_glycan_annotation_grobs <- function(structure, params) {
  labels <- as.character(structure)
  unique_labels <- unique(labels)
  unique_indices <- match(unique_labels, labels)
  unique_grobs <- purrr::map(
    unique_indices,
    \(.index) .new_glycan_label_grob(structure[.index], params)
  )
  grobs <- unique_grobs[match(labels, unique_labels)]
  purrr::map2(grobs, seq_along(grobs), function(grob, index) {
    grob$name <- paste0("anno_glycan.", index)
    grob$glydraw_annotation_index <- index
    grob
  })
}

#' Construct the grob drawn in a ComplexHeatmap annotation slice
#'
#' @param grobs Prepared glycan label grobs.
#' @param index Reordered indices supplied by ComplexHeatmap.
#' @param which Whether the annotation labels columns or rows.
#' @param side Side on which the annotation is drawn.
#'
#' @returns A positioned `gTree` containing the requested labels.
#' @noRd
.glycan_annotation_grob <- function(grobs, index, which, side) {
  labels <- grobs[index]
  n_labels <- length(labels)
  if (n_labels == 0) {
    return(grid::nullGrob())
  }
  positions <- if (identical(which, "column")) {
    seq_len(n_labels)
  } else {
    rev(seq_len(n_labels))
  }
  children <- purrr::map2(
    labels,
    positions,
    \(.grob, .position) {
      .position_glycan_label_grob(
        .grob,
        position = .position,
        vertical = identical(which, "row"),
        side = side
      )
    }
  )
  viewport <- if (identical(which, "column")) {
    grid::viewport(
      xscale = c(0.5, n_labels + 0.5),
      clip = "off"
    )
  } else {
    grid::viewport(
      yscale = c(0.5, n_labels + 0.5),
      clip = "off"
    )
  }

  grid::gTree(
    children = rlang::exec(grid::gList, !!!children),
    vp = viewport,
    name = paste0("anno_glycan.", which, ".labels")
  )
}

#' Subset cached glycan annotation grobs
#'
#' @param grobs A list of glycan label grobs.
#' @param index Indices retained by ComplexHeatmap.
#'
#' @returns The subset of `grobs`.
#' @noRd
.subset_glycan_annotation_grobs <- function(grobs, index) {
  grobs[index]
}
