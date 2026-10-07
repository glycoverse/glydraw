# Fit complete cartoons uniformly without changing their internal geometry.

.glycan_bounds_inches <- function(grob) {
  layout <- .cartoon_grid_layout(grob)
  scale <- grob$glydraw_scale
  if (is.null(scale)) {
    scale <- 1
  }
  size <- layout$size_px / .default_cartoon_dpi * scale
  offset <- .cartoon_grid_justification_offset(
    layout,
    scale,
    grob$glydraw_hjust,
    grob$glydraw_vjust,
    grob$reducing_end_coor
  )
  corners <- expand.grid(
    x = offset[["x"]] + c(-0.5, 0.5) * size[["width"]],
    y = offset[["y"]] + c(-0.5, 0.5) * size[["height"]]
  )
  angle <- grob$glydraw_angle
  if (is.null(angle)) {
    angle <- 0
  }
  radians <- angle * pi / 180
  x <- corners$x * cos(radians) - corners$y * sin(radians)
  y <- corners$x * sin(radians) + corners$y * cos(radians)
  c(left = min(x), right = max(x), bottom = min(y), top = max(y))
}

.glycan_collection_bounds <- function(grobs) {
  t(vapply(grobs, .glycan_bounds_inches, numeric(4)))
}

.glycan_fit_ratio <- function(available, required) {
  available <- rep_len(available, length(required))
  usable <- is.finite(available) & available > 0 & required > 1e-10
  min(1, available[usable] / required[usable])
}

.rescale_glycan_grobs <- function(grobs, factor) {
  lapply(grobs, function(grob) {
    grob$glydraw_scale <- grob$glydraw_scale * factor
    grid::makeContent(grob)
  })
}

# Reserve a bounded label area before the final layout is available. The
# drawing hook below refines this estimate using the actual label viewport.
.fit_glycan_label_size <- function(grobs, vertical) {
  size <- grDevices::dev.size("in")
  along <- if (vertical) 2L else 1L
  across <- 3L - along
  bounds <- .glycan_collection_bounds(grobs)
  along_bounds <- if (vertical) {
    bounds[, 3:4, drop = FALSE]
  } else {
    bounds[, 1:2, drop = FALSE]
  }
  across_extent <- if (vertical) {
    bounds[, 2] - bounds[, 1]
  } else {
    bounds[, 4] - bounds[, 3]
  }
  factor <- min(
    .glycan_fit_ratio(
      0.8 * 0.65 * size[[along]] / length(grobs),
      max(along_bounds[, 2]) - min(along_bounds[, 1])
    ),
    .glycan_fit_ratio(0.2 * size[[across]], across_extent)
  )
  .rescale_glycan_grobs(grobs, factor)
}

.fit_glycan_panel <- function(x) {
  if (is.null(x$glydraw_fit_base)) {
    x$glydraw_fit_base <- x$children
  }
  grobs <- x$glydraw_fit_base
  size <- .current_viewport_size_inches()
  bounds <- .glycan_collection_bounds(grobs)
  positions <- x$glydraw_positions
  positions$x <- positions$x * size[["width"]]
  positions$y <- positions$y * size[["height"]]
  factor <- min(
    .glycan_fit_ratio(0.3 * size[["width"]], bounds[, 2] - bounds[, 1]),
    .glycan_fit_ratio(0.3 * size[["height"]], bounds[, 4] - bounds[, 3])
  )
  # Anchors on or outside an edge cannot be contained by scaling alone.
  # Retain their placement; scale expansion remains the caller's control.
  inside_x <- positions$x > 0 & positions$x < size[["width"]]
  inside_y <- positions$y > 0 & positions$y < size[["height"]]
  factor <- min(
    factor,
    .glycan_fit_ratio(0.95 * positions$x[inside_x], -bounds[inside_x, 1]),
    .glycan_fit_ratio(
      0.95 * (size[["width"]] - positions$x[inside_x]),
      bounds[inside_x, 2]
    ),
    .glycan_fit_ratio(0.95 * positions$y[inside_y], -bounds[inside_y, 3]),
    .glycan_fit_ratio(
      0.95 * (size[["height"]] - positions$y[inside_y]),
      bounds[inside_y, 4]
    )
  )
  # A pair needs separation on at least one axis. Coincident anchors are
  # intentional overplotting and cannot be separated by changing size.
  if (length(grobs) > 1L) {
    for (i in seq_len(length(grobs) - 1L)) {
      j <- seq.int(i + 1L, length(grobs))
      dx <- positions$x[j] - positions$x[[i]]
      dy <- positions$y[j] - positions$y[[i]]
      needed_x <- ifelse(
        dx >= 0,
        bounds[i, 2] - bounds[j, 1],
        bounds[j, 2] - bounds[i, 1]
      )
      needed_y <- ifelse(
        dy >= 0,
        bounds[i, 4] - bounds[j, 3],
        bounds[j, 4] - bounds[i, 3]
      )
      separated <- abs(dx) + abs(dy) > 1e-10
      ratio_x <- ifelse(needed_x > 1e-10, 0.8 * abs(dx) / needed_x, Inf)
      ratio_y <- ifelse(needed_y > 1e-10, 0.8 * abs(dy) / needed_y, Inf)
      factor <- min(factor, pmax(ratio_x, ratio_y)[separated])
    }
  }
  grid::setChildren(
    x,
    do.call(grid::gList, .rescale_glycan_grobs(grobs, factor))
  )
}

#' @noRd
#' @exportS3Method grid::makeContent
makeContent.glycan_panel <- function(x) {
  if (!isTRUE(x$glydraw_fit)) {
    return(x)
  }
  .fit_glycan_panel(x)
}

#' @noRd
#' @exportS3Method grid::makeContent
makeContent.glycan_axis_labels <- function(x) {
  if (!isTRUE(x$glydraw_fit) || length(x$children) == 0L) {
    return(x)
  }
  if (is.null(x$glydraw_fit_base)) {
    x$glydraw_fit_base <- x$children
  }
  vertical <- x$glydraw_vertical
  positions <- x$glydraw_positions
  size <- .current_viewport_size_inches()
  convert <- if (vertical) grid::convertHeight else grid::convertWidth
  positions <- convert(grid::unit(positions, "native"), "in", valueOnly = TRUE)
  along_size <- size[[if (vertical) "height" else "width"]]
  across_size <- size[[if (vertical) "width" else "height"]]
  pitch <- min(c(along_size, diff(sort(unique(positions)))))
  reference <- x$glydraw_fit_reference
  if (is.null(reference)) {
    reference <- x$glydraw_fit_base
  }
  bounds <- .glycan_collection_bounds(reference)
  along_bounds <- if (vertical) {
    bounds[, 3:4, drop = FALSE]
  } else {
    bounds[, 1:2, drop = FALSE]
  }
  along_nudge <- vapply(
    reference,
    function(grob) {
      grob[[if (vertical) "glydraw_nudge_y" else "glydraw_nudge_x"]] / 25.4
    },
    numeric(1)
  )
  across_extent <- if (vertical) {
    bounds[, 2] - bounds[, 1]
  } else {
    bounds[, 4] - bounds[, 3]
  }
  across_nudge <- vapply(
    reference,
    function(grob) {
      abs(grob[[if (vertical) "glydraw_nudge_x" else "glydraw_nudge_y"]]) / 25.4
    },
    numeric(1)
  )
  factor <- min(
    .glycan_fit_ratio(
      0.8 * pitch,
      max(along_bounds[, 2] + along_nudge) -
        min(along_bounds[, 1] + along_nudge)
    ),
    .glycan_fit_ratio(across_size - across_nudge, across_extent)
  )
  children <- .rescale_glycan_grobs(x$glydraw_fit_base, factor)
  # Rotation changes the perpendicular bounds. Keep the rotated content in
  # its reserved area while retaining its along-axis reducing-end anchor.
  children <- lapply(children, function(grob) {
    bounds <- .glycan_bounds_inches(grob)
    justification <- grob[[if (vertical) "glydraw_hjust" else "glydraw_vjust"]]
    if (is.numeric(justification)) {
      low <- bounds[[if (vertical) "left" else "bottom"]]
      high <- bounds[[if (vertical) "right" else "top"]]
      anchor <- justification * (across_size - high + low) - low
      if (vertical) {
        grob$vp$x <- grid::unit(anchor, "in") +
          grid::unit(
            .glycan_axis_viewport_nudge(
              grob$glydraw_axis_position,
              grob$glydraw_hjust,
              grob$glydraw_vjust,
              grob$glydraw_nudge_x,
              grob$glydraw_nudge_y
            )[["x"]],
            "mm"
          )
      } else {
        grob$vp$y <- grid::unit(anchor, "in") +
          grid::unit(
            .glycan_axis_viewport_nudge(
              grob$glydraw_axis_position,
              grob$glydraw_hjust,
              grob$glydraw_vjust,
              grob$glydraw_nudge_x,
              grob$glydraw_nudge_y
            )[["y"]],
            "mm"
          )
      }
    }
    grob
  })
  grid::setChildren(x, do.call(grid::gList, children))
}
