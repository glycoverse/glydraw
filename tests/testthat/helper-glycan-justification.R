.reducing_end_displacement <- function(grob, axis) {
  cartoon <- .glycan_grob_to_plot(grob)
  built <- ggplot2::ggplot_build(cartoon)
  size <- attr(cartoon, "glydraw_size_px")
  justification_offset <- .glycan_justification_offset(
    cartoon,
    size,
    scale = grob$glydraw_scale,
    hjust = grob$glydraw_hjust,
    vjust = grob$glydraw_vjust,
    reducing_end_coor = grob$reducing_end_coor
  )
  panel_range <- built$layout$panel_params[[1]][[paste0(axis, ".range")]]
  dimension <- switch(axis, x = "width", y = "height")
  coordinate_in <- (grob$reducing_end_coor[[axis]] - panel_range[[1]]) /
    diff(panel_range) *
    size[[dimension]] /
    .default_cartoon_dpi *
    grob$glydraw_scale

  justification_offset[[axis]] -
    size[[dimension]] / .default_cartoon_dpi * grob$glydraw_scale / 2 +
    coordinate_in
}
