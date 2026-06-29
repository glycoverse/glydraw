#' Draw a Symbol Nomenclature For Glycan (SNFG)
#'
#' @param structure A [glyrepr::glycan_structure()] scalar,
#'   or a string of any glycan structure text nomenclatures supported by [glyparse::auto_parse()].
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
#'   `node_size` is too large to leave enough annotation space.
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
  checkmate::assert_number(node_size, lower = 0)
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
#' @param newpage Draw the plot on a new page.
#' @param vp A grid viewport object or viewport name.
#' @param ... Additional arguments passed to ggplot2's print method.
#'
#' @return The original glycan cartoon, invisibly.
#' @export
print.glydraw_cartoon <- function(
  x,
  newpage = is.null(vp),
  vp = NULL,
  ...
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
#' In theory, you can just use `ggplot2::ggsave()` to save the cartoons plotted by [draw_cartoon()].
#' However, you can have trouble finding the best sizes for each cartoon
#' to make them look alike.
#' This function is designed to save the cartoons with self-adjusted sizes,
#' based on the size of the glycans,
#' so that when glycans with different sizes are put together, they will look alike.
#'
#' @param cartoon A ggplot2 object returned by [draw_cartoon()].
#' @param file File name of glycan cartoon.
#' @param dpi Dots per inch, default = 300.
#'
#' @examples
#' \dontrun{
#' cartoon <- draw_cartoon("Gal(b1-3)GalNAc(a1-")
#' save_cartoon(cartoon, "p1.png", dpi = 300)
#' }
#' @export
save_cartoon <- function(cartoon, file, dpi = 300) {
  checkmate::assert_class(cartoon, "glydraw_cartoon")
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
  cartoon <- cartoon |>
    .add_plot_border(border_px[["width"]] / dpi * 72) |>
    .set_fixed_panel_size(panel_size, dpi = dpi) |>
    .strip_cartoon_class()

  ggplot2::ggsave(
    filename = file,
    plot = cartoon,
    width = size[["width"]],
    height = size[["height"]],
    units = "px",
    dpi = dpi,
    bg = bg_color
  )
}

#' Export all glycan structures to figures
#'
#' This function calls [draw_cartoon()] on each glycan structure in `x`,
#' then calls [save_cartoon()] to save a figure for each of them.
#' IUPAC-condensed nomenclatures are used as file names. If `x` is a named
#' character vector or named [glyrepr::glycan_structure()] vector, the vector
#' names are used as file names.
#'
#' @param x A [glyrepr::glycan_structure()] vector, or a character vector of
#'   any glycan structure text nomenclatures supported by
#'   [glyparse::auto_parse()].
#' @param dirname Directory name to save the cartoons. If it does not exist,
#'   it is created.
#' @param file_ext File extension supported by [ggplot2::ggsave()]. Defaults to "png".
#' @param dpi Dots per inch. Defaults to 300.
#' @inheritParams draw_cartoon
#'
#' @return The function returns the list of cartoons implicitly.
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
  file_ext = "png",
  dpi = 300,
  show_linkage = TRUE,
  orient = c("H", "V"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1
) {
  UseMethod("export_cartoons")
}

#' @export
export_cartoons.character <- function(
  x,
  dirname,
  file_ext = "png",
  dpi = 300,
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
    dpi = dpi,
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
  file_ext = "png",
  dpi = 300,
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
    dpi = dpi,
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
#' @param dpi Numeric dots per inch passed to `save_cartoon()`.
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
  dpi,
  show_linkage,
  orient,
  red_end,
  edge_linewidth,
  node_linewidth,
  node_size
) {
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
  purrr::walk2(cartoons, filenames, save_cartoon, dpi = dpi)
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
