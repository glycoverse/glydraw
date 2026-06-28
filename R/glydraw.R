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
  highlight = NULL
) {
  inputs <- .prepare_cartoon_input(structure, highlight, orient, red_end)
  structure <- inputs$structure
  coor <- inputs$coor
  highlight <- inputs$highlight
  orient <- inputs$orient

  gly_list <- .cartoon_gly_list(structure, coor, highlight)
  polygon_coor <- create_polygon_coor(gly_list, 0.215)
  filled_color <- glycan_color[as.character(polygon_coor$color)]
  annotation_data <- .cartoon_annotation_data(
    structure,
    coor,
    orient,
    red_end,
    highlight
  )
  connect_df <- .cartoon_connection_data(
    structure,
    coor,
    annotation_data$reducing_info$segment,
    gly_list
  )

  .build_cartoon_plot(
    connect_df,
    polygon_coor,
    filled_color,
    annotation_data,
    show_linkage
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
    .apply_border(border_px[["width"]] / dpi * 72) |>
    .apply_fixed_panel_size(panel_size, dpi = dpi) |>
    .strip_glydraw_class()

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
#' @param x A [glyexp::experiment()], a [glyrepr::glycan_structure()] vector,
#'   or a character vector of any glycan structure text nomenclatures
#'   supported by [glyparse::auto_parse()].
#' @param dirname Directory name to save the cartoons. If it does not exist,
#'   it is created.
#' @param file_ext File extension supported by [ggplot2::ggsave()]. Defaults to "png".
#' @param dpi Dots per inch. Defaults to 300.
#' @inheritParams draw_cartoon
#'
#' @return The function returns the list of cartoons implicitly.
#'
#' @examples
#' \dontrun{
#' library(glyexp)
#' export_cartoons(real_experiment, "path/to/save")
#' }
#' @export
export_cartoons <- function(
  x,
  dirname,
  file_ext = "png",
  dpi = 300,
  show_linkage = TRUE,
  orient = c("H", "V"),
  red_end = ""
) {
  UseMethod("export_cartoons")
}

#' @export
export_cartoons.glyexp_experiment <- function(
  x,
  dirname,
  file_ext = "png",
  dpi = 300,
  show_linkage = TRUE,
  orient = c("H", "V"),
  red_end = ""
) {
  if (glyexp::get_exp_type(x) != "glycoproteomics") {
    cli::cli_abort(c(
      "{.arg x} can only be an experiment with {.val glycoproteomics} type.",
      "x" = "Got: {.val {glyexp::get_exp_type(x)}}"
    ))
  }
  if (!"glycan_structure" %in% colnames(glyexp::get_var_info(x))) {
    cli::cli_abort(
      "There must a {.field glycan_structure} column in {.field var_info}."
    )
  }
  glycans <- unique(glyexp::get_var_info(x)$glycan_structure)
  .export_cartoons(
    glycans,
    dirname,
    file_ext = file_ext,
    dpi = dpi,
    show_linkage = show_linkage,
    orient = orient,
    red_end = red_end
  )
}

#' @export
export_cartoons.character <- function(
  x,
  dirname,
  file_ext = "png",
  dpi = 300,
  show_linkage = TRUE,
  orient = c("H", "V"),
  red_end = ""
) {
  glycans <- unique(.ensure_structure(x))
  .export_cartoons(
    glycans,
    dirname,
    file_ext = file_ext,
    dpi = dpi,
    show_linkage = show_linkage,
    orient = orient,
    red_end = red_end
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
  red_end = ""
) {
  glycans <- unique(x)
  .export_cartoons(
    glycans,
    dirname,
    file_ext = file_ext,
    dpi = dpi,
    show_linkage = show_linkage,
    orient = orient,
    red_end = red_end
  )
}

.export_cartoons <- function(
  glycans,
  dirname,
  file_ext,
  dpi,
  show_linkage,
  orient,
  red_end
) {
  cli::cli_alert_info("Exporting {.val {length(glycans)}} glycan cartoons.")
  fs::dir_create(dirname)
  glycan_list <- purrr::map(seq_along(glycans), ~ glycans[[.x]])
  cartoons <- purrr::map(
    glycan_list,
    draw_cartoon,
    show_linkage = show_linkage,
    orient = orient,
    red_end = red_end
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
#' @param glycans A vector of glycan structures.
#' @return A vector of labels for output filenames.
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
