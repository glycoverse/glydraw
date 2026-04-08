#' Draw a Symbol Nomenclature For Glycan (SNFG)
#'
#' @param structure A [glyrepr::glycan_structure()] scalar,
#'   or a string of any glycan structure text nomenclatures supported by [glyparse::auto_parse()].
#' @param show_linkage Show linkage annotation or not. Default is TRUE.
#' @param orient The orientation of glycan structure. "H" for horizontal, "V" for vertical.
#'   Default is "H"
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
  highlight = NULL
) {
  if (!is.null(highlight) && !glyrepr::is_glycan_structure(structure)) {
    cli::cli_abort("{.arg highlight} can only be set when {.arg structure} is a {.fn glyrepr::glycan_structure}.")
  }
  structure <- .ensure_one_structure(structure)
  structure <- glyrepr::get_structure_graphs(structure, return_list = FALSE)
  highlight <- .ensure_highlight_para(highlight, length(structure))
  orient <- rlang::arg_match(orient)

  # Coordinate of Glycans
  if (orient == 'H') {
    coor <- coor_cal(structure)
  } else {
    coor <- coor_cal(structure)
    temp <- coor
    coor[, 1] <- temp[, 2]
    coor[, 2] <- -temp[, 1]
  }

  gly_list <- data.frame(coor, 'glycoform' = glycoform_info(structure))

  # Process highlight points, highlight vertices 1.0, others 0.3
  if (!is.null(highlight)) {
    ver_transparency <- replace(rep(0.3, length(structure)), highlight, 1.0)
    gly_list$transparency <- ver_transparency
  } else {
    gly_list$transparency <- 1.0
  }
  # Rename colnames of gly_list
  colnames(gly_list) <- c('center_x', 'center_y', 'glycoform', 'transparency')
  # Draw Glycan Shape, where gly_list contains center_x, center_y, glycoform 3 columns
  polygon_coor <- create_polygon_coor(gly_list, 0.2)
  filled_color <- glycan_color[as.character(polygon_coor$color)]

  struc_annotation <- gly_annotation(structure, coor)
  reducing_info <- reducing_end_annotation(structure, coor)
  struc_annotation <- dplyr::bind_rows(
    struc_annotation,
    reducing_info$annotation
  )

  if (is.null(highlight)) {
    struc_annotation$transparency <- 1
  } else {
    struc_annotation$transparency <- (struc_annotation$vertice %in% highlight) *
      0.7 +
      0.3
  }

  # Escape '?' to prevent conflict with parse = TRUE
  struc_annotation <- struc_annotation |>
    dplyr::mutate(
      annot_label = dplyr::case_when(
        annot == "?" ~ '~"?"',
        annot == "??" ~ '~"?"',
        # case like '?3' would convert to '?'
        grepl("^\\?\\d+", annot) ~ '~"?"',
        # normal annotation would maintain the same
        TRUE ~ struc_annotation$annot
      )
    )

  # connect information
  gly_connect <- connect_info(structure, coor)
  connect_df <- data.frame(
    start_x = gly_connect$start_x,
    start_y = gly_connect$start_y,
    end_x = gly_connect$end_x,
    end_y = gly_connect$end_y
  )
  connect_df <- dplyr::bind_rows(connect_df, reducing_info$segment)
  connect_df$transparency <- gly_list$transparency

  gly_graph <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = connect_df,
      ggplot2::aes(
        x = .data$start_x,
        y = .data$start_y,
        xend = .data$end_x,
        yend = .data$end_y
      ),
      alpha = connect_df$transparency,
      linewidth = 0.5
    ) +
    ggplot2::geom_polygon(
      data = polygon_coor, # Masking the segment with white color
      ggplot2::aes(x = .data$point_x, y = .data$point_y, group = .data$group),
      fill = "white",
      color = 'white',
      linewidth = 0.5
    ) +
    ggplot2::geom_polygon(
      data = polygon_coor,
      ggplot2::aes(x = .data$point_x, y = .data$point_y, group = .data$group),
      alpha = polygon_coor$alpha,
      fill = filled_color,
      color = scales::alpha("black", polygon_coor$alpha),
      linewidth = 0.5
    ) +
    ggplot2::coord_fixed(ratio = 1, clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
  if (show_linkage) {
    gly_graph <- gly_graph +
      ggplot2::geom_text(
        data = struc_annotation,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$annot_label),
        alpha = struc_annotation$transparency,
        parse = TRUE,
        size = 6,
        hjust = 0.5,
        vjust = 0.5
      )
  }
  gly_graph <- .apply_border(gly_graph, 50 / 300 * 72)
  size <- .decide_size(gly_graph, border_px = 50)
  gly_graph <- gly_graph +
    ggview::canvas(
      width = size$width,
      height = size$height,
      units = "px",
      dpi = 300
    )
  structure(gly_graph, class = c("glydraw_cartoon", class(gly_graph)))
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
  ggview::save_ggplot(
    file = file,
    plot = cartoon,
    units = 'px',
    dpi = dpi,
    bg = bg_color
  )
}

#' Export all glycan structures to figures
#'
#' This function calls [draw_cartoon()] on each glycan structure in `x`,
#' then calls [save_cartoon()] to save a figure for each of them.
#' IUPAC-condensed nonmenclatures are used as file names.
#'
#' @param x A [glyexp::experiment()], a [glyrepr::glycan_structure()] vector,
#'   or a character vector of any glycan structure text nomenclatures
#'   supported by [glyparse::auto_parse()].
#' @param dirname Directory name to save the cartoons.
#' @param file_ext File extention supported by [ggplot2::ggsave()]. Defaults to "png".
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
  orient = c("H", "V")
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
  orient = c("H", "V")
) {
  if (glyexp::get_exp_type(x) != "glycoproteomics") {
    cli::cli_abort(c(
      "{.arg x} can only be an experiment with {.val glycoproteomics} type.",
      "x" = "Got: {.val {glyexp::get_exp_type(x)}}"
    ))
  }
  if (!"glycan_structure" %in% colnames(glyexp::get_var_info(x))) {
    cli::cli_abort("There must a {.field glycan_structure} column in {.field var_info}.")
  }
  glycans <- unique(glyexp::get_var_info(x)$glycan_structure)
  .export_cartoons(
    glycans,
    dirname,
    file_ext = file_ext,
    dpi = dpi,
    show_linkage = show_linkage,
    orient = orient
  )
}

#' @export
export_cartoons.character <- function(
  x,
  dirname,
  file_ext = "png",
  dpi = 300,
  show_linkage = TRUE,
  orient = c("H", "V")
) {
  glycans <- unique(.ensure_structure(x))
  .export_cartoons(
    glycans,
    dirname,
    file_ext = file_ext,
    dpi = dpi,
    show_linkage = show_linkage,
    orient = orient
  )
}

#' @export
export_cartoons.glyrepr_structure <- function(
  x,
  dirname,
  file_ext = "png",
  dpi = 300,
  show_linkage = TRUE,
  orient = c("H", "V")
) {
  glycans <- unique(x)
  .export_cartoons(
    glycans,
    dirname,
    file_ext = file_ext,
    dpi = dpi,
    show_linkage = show_linkage,
    orient = orient
  )
}

.export_cartoons <- function(
  glycans,
  dirname,
  file_ext,
  dpi,
  show_linkage,
  orient
) {
  cli::cli_alert_info("Exporting {.val {length(glycans)}} glycan cartoons.")
  checkmate::assert_directory_exists(dirname)
  glycan_list <- purrr::map(seq_along(glycans), ~ glycans[[.x]])
  cartoons <- purrr::map(glycan_list, draw_cartoon, show_linkage = show_linkage, orient = orient)
  filenames <- fs::path(dirname, .sanitize_export_filenames(glycans), ext = file_ext)
  purrr::walk2(cartoons, filenames, save_cartoon, dpi = dpi)
  invisible(cartoons)
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
