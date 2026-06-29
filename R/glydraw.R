#' Draw a Symbol Nomenclature For Glycan (SNFG)
#'
#' @param structure A [glyrepr::glycan_structure()] scalar,
#'   or a string of any glycan structure text nomenclatures supported by [glyparse::auto_parse()].
#' @param ... Ignored.
#' @param show_linkage Show linkage annotation or not. Default is TRUE.
#' @param orient The orientation of glycan structure. "H" for horizontal, "V" for vertical.
#'   Default is "H"
#' @param red_end Reducing-end annotation. The default `""` keeps the current
#'   reducing-end line. Use `"~"` to add a wavy reducing end, or any other
#'   string to draw that string at the reducing end.
#' @param edge_linewidth Numeric scalar controlling the linewidth of linkage
#'   lines. Defaults to the current value, `0.8`.
#' @param node_linewidth Numeric scalar controlling the linewidth of node
#'   borders. Defaults to the current value, `0.8`.
#' @param node_size Numeric scalar used as a multiplier for the default node
#'   size. Defaults to `1`, which keeps the current size. Linkage annotations
#'   are moved farther from larger nodes, and are hidden with a warning when
#'   `node_size` is too large to leave enough annotation space. Values larger
#'   than `2` are rejected because residues overlap.
#' @param highlight An integer vector specifying the node indices to highlight.
#'   This argument is applicable only when `structure` is a [glyrepr::glycan_structure()].
#'   Note that for a [glyrepr::glycan_structure()], the node indices correspond exactly
#'   to the monosaccharides in its printed IUPAC nomenclature.
#'   For example, given `glyrepr::as_glycan_structure("Gal(b1-3)[GlcNAc(b1-6)]GalNAc(a1-")`,
#'   setting `highlight = c(1, 3)` will highlight the "Gal" and "GalNAc" nodes.
#'
#' @returns a ggplot2 object
#' @examples
#' \dontrun{
#' draw_cartoon("Gal(b1-3)GalNAc(a1-")
#' }
#' @export
draw_cartoon <- function(
  structure,
  ...,
  show_linkage = TRUE,
  orient = c("H", "V"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  highlight = NULL
) {
  checkmate::assert_number(edge_linewidth, lower = 0)
  checkmate::assert_number(node_linewidth, lower = 0)
  .validate_node_size(node_size)
  inputs <- .prepare_cartoon_inputs(structure, highlight, orient, red_end)
  structure <- inputs$structure
  coor <- inputs$coor
  highlight <- inputs$highlight
  orient <- inputs$orient
  show_linkage <- .resolve_linkage_visibility(show_linkage, node_size)

  gly_list <- .cartoon_residue_data(structure, coor, highlight)
  polygon_coor <- .residue_polygon_data(
    gly_list,
    .default_node_point_size * node_size
  )
  filled_color <- glycan_color[as.character(polygon_coor$color)]
  annotation_data <- .cartoon_text_annotation_data(
    structure,
    coor,
    orient,
    red_end,
    highlight,
    node_size = node_size
  )
  connect_df <- .cartoon_segment_data(
    structure,
    coor,
    annotation_data$reducing_info$segment,
    gly_list
  )

  .assemble_cartoon_plot(
    connect_df,
    polygon_coor,
    filled_color,
    annotation_data,
    show_linkage,
    edge_linewidth,
    node_linewidth
  )
}

#' Print glycan cartoon
#'
#' @param x A ggplot2 object returned by [draw_cartoon()].
#' @param ... Ignored.
#' @param newpage Draw the plot on a new page.
#' @param vp A grid viewport object or viewport name.
#'
#' @return The original glycan cartoon, invisibly.
#' @export
print.glydraw_cartoon <- function(
  x,
  ...,
  newpage = is.null(vp),
  vp = NULL
) {
  raster <- .render_cartoon_raster(x)
  .draw_cartoon_raster(
    raster,
    size_px = attr(x, "glydraw_size_px"),
    newpage = newpage,
    vp = vp
  )
  invisible(x)
}

#' Save fixed-size glycan cartoon image to local device.
#'
#' This function saves the glycan cartoon to a file,
#' with a suitable size.
#'
#' @param cartoon A ggplot2 object returned by [draw_cartoon()].
#' @param file File name of glycan cartoon.
#' @param ... Ignored.
#' @param dpi Deprecated and ignored. Use `scale` to change the output size.
#' @param scale Numeric output-size multiplier. The default `1` saves the
#'   cartoon at its natural fixed size; `2` saves the same cartoon with twice
#'   the pixel width and height.
#'
#' @details
#' # Why not `width` and `height`?
#'
#' The familiar [ggplot2::ggsave()] interface uses `width`, `height`, and `dpi`
#' because ordinary ggplot2 plots are drawn into a user-chosen device size.
#' glydraw cartoons are different: the natural width and height are calculated
#' from the glycan structure so residues, linkages, labels, and borders stay
#' comparable across different glycans. If users supplied arbitrary `width` and
#' `height`, glydraw would either distort that structure-derived layout or need
#' to guess how to reconcile one requested size with the other.
#'
#' `dpi` is also not the right control here because changing it alters how
#' point- and inch-based ggplot2 elements are rasterized relative to the fixed
#' cartoon canvas. glydraw therefore keeps an internal fixed design scale and
#' uses `scale` as a single multiplier for the final pixel dimensions. This
#' preserves the cartoon's aspect ratio and relative appearance while still
#' allowing larger or smaller output files.
#'
#' @examples
#' \dontrun{
#' cartoon <- draw_cartoon("Gal(b1-3)GalNAc(a1-")
#' save_cartoon(cartoon, "p1.png", scale = 2)
#' }
#' @export
save_cartoon <- function(cartoon, file, ..., dpi = 300, scale = 1) {
  checkmate::assert_class(cartoon, "glydraw_cartoon")
  if (!missing(dpi)) {
    .warn_ignored_dpi()
  }
  .validate_output_scale(scale)
  file_ext <- tools::file_ext(file)
  bg_color <- ifelse(
    file_ext == "jpeg" | file_ext == "jpg",
    "white",
    "transparent"
  )
  size <- attr(cartoon, "glydraw_size_px")
  panel_size <- attr(cartoon, "glydraw_panel_size_px")
  checkmate::assert_numeric(size, names = "strict", any.missing = FALSE)
  checkmate::assert_names(names(size), identical.to = c("width", "height"))
  checkmate::assert_numeric(panel_size, names = "strict", any.missing = FALSE)
  checkmate::assert_names(
    names(panel_size),
    identical.to = c("width", "height")
  )
  border_px <- (size - panel_size) / 2
  render_dpi <- .default_cartoon_dpi
  cartoon <- cartoon |>
    .add_plot_border(border_px[["width"]] / render_dpi * 72) |>
    .set_fixed_panel_size(panel_size, dpi = render_dpi) |>
    .strip_cartoon_class()

  ggplot2::ggsave(
    filename = file,
    plot = cartoon,
    width = size[["width"]] * scale,
    height = size[["height"]] * scale,
    units = "px",
    dpi = render_dpi * scale,
    bg = bg_color
  )
}

#' Export all glycan structures to figures
#'
#' Draw and save one cartoon for each glycan structure in `x`.
#'
#' @param x A [glyrepr::glycan_structure()] vector, or a character vector of
#'   any glycan structure text nomenclatures supported by
#'   [glyparse::auto_parse()].
#' @param dirname Directory name to save the cartoons. If it does not exist,
#'   it is created.
#' @param ... Ignored.
#' @param file_ext File extension supported by [ggplot2::ggsave()]. Defaults to "png".
#' @param dpi Deprecated and ignored. Use `scale` to change the output size.
#' @param scale Numeric output-size multiplier passed to [save_cartoon()].
#' @inheritParams draw_cartoon
#'
#' @return The function returns the list of cartoons implicitly.
#'
#' @details
#' # File names
#' IUPAC-condensed nomenclatures are used as file names. If `x` is a named
#' character vector or named [glyrepr::glycan_structure()] vector, the vector
#' names are used as file names.
#'
#' @inheritSection save_cartoon Why not `width` and `height`?
#'
#' @examples
#' export_cartoons(
#'   c(
#'     "Man(a1-3)Man(b1-4)GlcNAc(b1-",
#'     "Gal(b1-4)GlcNAc(b1-"
#'   ),
#'   "path/to/save"
#' )
#' @export
export_cartoons <- function(
  x,
  dirname,
  ...,
  file_ext = "png",
  dpi = 300,
  scale = 1,
  show_linkage = TRUE,
  orient = c("H", "V"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1
) {
  .validate_node_size(node_size)
  if (!missing(dpi)) {
    .warn_ignored_dpi()
  }
  .validate_output_scale(scale)
  UseMethod("export_cartoons")
}

#' @export
export_cartoons.character <- function(
  x,
  dirname,
  ...,
  file_ext = "png",
  dpi = 300,
  scale = 1,
  show_linkage = TRUE,
  orient = c("H", "V"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1
) {
  glycans <- unique(.as_glycan_structure_input(x))
  .export_cartoon_list(
    glycans,
    dirname,
    file_ext = file_ext,
    scale = scale,
    show_linkage = show_linkage,
    orient = orient,
    red_end = red_end,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size
  )
}

#' @export
export_cartoons.glyrepr_structure <- function(
  x,
  dirname,
  ...,
  file_ext = "png",
  dpi = 300,
  scale = 1,
  show_linkage = TRUE,
  orient = c("H", "V"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1
) {
  glycans <- unique(x)
  .export_cartoon_list(
    glycans,
    dirname,
    file_ext = file_ext,
    scale = scale,
    show_linkage = show_linkage,
    orient = orient,
    red_end = red_end,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size
  )
}

#' Export a vector of glycan cartoons
#'
#' @param glycans A vector of `glyrepr::glycan_structure()` values.
#' @param dirname String path to the output directory.
#' @param file_ext String output file extension without leading dot.
#' @param scale Numeric output-size multiplier passed to `save_cartoon()`.
#' @param show_linkage Logical scalar passed to `draw_cartoon()`.
#' @param orient Drawing orientation, either `"H"` or `"V"`.
#' @param red_end String reducing-end annotation passed to `draw_cartoon()`.
#' @param edge_linewidth Numeric linkage line width passed to
#'   `draw_cartoon()`.
#' @param node_linewidth Numeric node border line width passed to
#'   `draw_cartoon()`.
#' @param node_size Numeric node size multiplier passed to `draw_cartoon()`.
#'
#' @return A list of `glydraw_cartoon` objects, invisibly. Files are written to
#'   `dirname` with sanitized labels and extension `file_ext`.
#' @noRd
.export_cartoon_list <- function(
  glycans,
  dirname,
  file_ext,
  scale,
  show_linkage,
  orient,
  red_end,
  edge_linewidth,
  node_linewidth,
  node_size
) {
  .validate_node_size(node_size)
  cli::cli_alert_info("Exporting {.val {length(glycans)}} glycan cartoons.")
  fs::dir_create(dirname)
  glycan_list <- purrr::map(seq_along(glycans), ~ glycans[[.x]])
  cartoons <- purrr::map(
    glycan_list,
    draw_cartoon,
    show_linkage = show_linkage,
    orient = orient,
    red_end = red_end,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size
  )
  filenames <- fs::path(
    dirname,
    .sanitize_export_filenames(.export_filename_labels(glycans)),
    ext = file_ext
  )
  purrr::walk2(cartoons, filenames, save_cartoon, scale = scale)
  invisible(cartoons)
}

#' Select filename labels for exported cartoons
#'
#' @param glycans A named or unnamed vector of glycan structures.
#'
#' @return A character vector the same length as `glycans`. Non-empty vector
#'   names are used as labels; unnamed entries fall back to the structure text.
#' @noRd
.export_filename_labels <- function(glycans) {
  if (is.null(names(glycans))) {
    glycans
  } else {
    labels <- as.character(glycans)
    glycan_names <- names(glycans)
    has_name <- !is.na(glycan_names) & nzchar(glycan_names)
    labels[has_name] <- glycan_names[has_name]
    labels
  }
}

#' Sanitize glycan labels for export filenames
#'
#' @param glycans A character vector of proposed filename labels.
#'
#' @return A character vector the same length as `glycans`, sanitized for file
#'   paths, truncated to 180 characters, and made unique without extensions.
#' @noRd
.sanitize_export_filenames <- function(glycans) {
  safe_names <- fs::path_sanitize(as.character(glycans), replacement = "_")
  safe_names <- ifelse(
    nchar(safe_names) == 0,
    "glycan",
    safe_names
  )
  # Keep filenames below typical file-system limits after adding extension/suffixes.
  safe_names <- substr(safe_names, 1, 180)
  make.unique(safe_names, sep = "_")
}
