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
#' @returns The function returns the list of cartoons implicitly.
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
#'   tempdir()
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
  fuc_orient = c("flex", "up"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL,
  style = NULL
) {
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
  fuc_orient = c("flex", "up"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL,
  style = NULL
) {
  style <- .resolve_glydraw_style(
    style = style,
    show_linkage = show_linkage,
    orient = orient,
    fuc_orient = fuc_orient,
    red_end = red_end,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size,
    colors = colors,
    .supplied = c(
      show_linkage = !missing(show_linkage),
      orient = !missing(orient),
      fuc_orient = !missing(fuc_orient),
      red_end = !missing(red_end),
      edge_linewidth = !missing(edge_linewidth),
      node_linewidth = !missing(node_linewidth),
      node_size = !missing(node_size),
      colors = !missing(colors)
    )
  )
  .export_cartoon_list(
    unique(.as_glycan_structure_input(x)),
    dirname,
    file_ext,
    scale,
    style$show_linkage,
    style$orient,
    style$fuc_orient,
    style$red_end,
    style$edge_linewidth,
    style$node_linewidth,
    style$node_size,
    style$colors
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
  fuc_orient = c("flex", "up"),
  red_end = "",
  edge_linewidth = 0.8,
  node_linewidth = 0.8,
  node_size = 1,
  colors = NULL,
  style = NULL
) {
  style <- .resolve_glydraw_style(
    style = style,
    show_linkage = show_linkage,
    orient = orient,
    fuc_orient = fuc_orient,
    red_end = red_end,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size,
    colors = colors,
    .supplied = c(
      show_linkage = !missing(show_linkage),
      orient = !missing(orient),
      fuc_orient = !missing(fuc_orient),
      red_end = !missing(red_end),
      edge_linewidth = !missing(edge_linewidth),
      node_linewidth = !missing(node_linewidth),
      node_size = !missing(node_size),
      colors = !missing(colors)
    )
  )
  .export_cartoon_list(
    unique(x),
    dirname,
    file_ext,
    scale,
    style$show_linkage,
    style$orient,
    style$fuc_orient,
    style$red_end,
    style$edge_linewidth,
    style$node_linewidth,
    style$node_size,
    style$colors
  )
}

.export_cartoon_list <- function(
  glycans,
  dirname,
  file_ext,
  scale,
  show_linkage,
  orient,
  fuc_orient,
  red_end,
  edge_linewidth,
  node_linewidth,
  node_size,
  colors
) {
  .validate_node_size(node_size)
  colors <- .validate_custom_colors(colors)
  fuc_orient <- rlang::arg_match(fuc_orient, c("flex", "up"))
  cli::cli_alert_info("Exporting {.val {length(glycans)}} glycan cartoons.")
  fs::dir_create(dirname)
  glycan_list <- purrr::map(seq_along(glycans), ~ glycans[[.x]])
  cartoons <- purrr::map(
    glycan_list,
    draw_cartoon,
    show_linkage = show_linkage,
    orient = orient,
    fuc_orient = fuc_orient,
    red_end = red_end,
    edge_linewidth = edge_linewidth,
    node_linewidth = node_linewidth,
    node_size = node_size,
    colors = colors
  )
  filenames <- fs::path(
    dirname,
    .sanitize_export_filenames(.export_filename_labels(glycans)),
    ext = file_ext
  )
  purrr::walk2(cartoons, filenames, save_cartoon, scale = scale)
  invisible(cartoons)
}

.export_filename_labels <- function(glycans) {
  if (is.null(names(glycans))) {
    return(glycans)
  }
  labels <- as.character(glycans)
  glycan_names <- names(glycans)
  has_name <- !is.na(glycan_names) & nzchar(glycan_names)
  labels[has_name] <- glycan_names[has_name]
  labels
}

.sanitize_export_filenames <- function(glycans) {
  safe_names <- fs::path_sanitize(as.character(glycans), replacement = "_")
  safe_names <- ifelse(nchar(safe_names) == 0, "glycan", safe_names)
  safe_names <- substr(safe_names, 1, 180)
  make.unique(safe_names, sep = "_")
}
