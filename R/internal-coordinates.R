# Internal helpers for calculating, spreading, orienting, and compacting residue
# coordinates from glycan graph topology.

#' Initialize rough residue coordinates
#'
#' @param structure An igraph glycan graph. Vertices are ordered from
#'   non-reducing residues to the reducing-end residue, and vertex attributes
#'   include `mono`.
#'
#' @returns A numeric matrix with one row per vertex and columns `x` and `y`.
#'   `x` is the negative graph distance from the reducing end, and `y` starts
#'   at 0 except for preliminary Fuc-like subtree offsets.
#' @noRd
.initialize_residue_coordinates <- function(structure) {
  ver_num <- length(structure)
  coor <- matrix(
    ini_xy <- rep(0, 2 * ver_num),
    ncol = 2
  )
  colnames(coor) <- c('x', 'y')
  # Gain x coordinate, where initial position of the structure is length(structure)
  init_X <- igraph::distances(structure)[length(structure), ] * (-1)
  coor[, 'x'] <- init_X
  for (i in seq(1, length(structure))) {
    if (
      .is_fucose_like_layout_monosaccharide(igraph::V(structure)[[i]]$mono) &&
        !.is_reducing_end(structure, i)
    ) {
      coor <- .shift_subtree_axis(structure, i, coor, "x", 1)
    }
  }
  return(coor)
}

#' Check whether a vertex is the reducing end
#'
#' @param structure An igraph glycan graph.
#' @param ver A single integer vertex index.
#'
#' @returns A logical scalar: `TRUE` when `ver` is the reducing-end vertex.
#' @noRd
.is_reducing_end <- function(structure, ver) {
  ver == length(structure)
}

#' Get all vertices in a residue subtree
#'
#' @param structure An igraph glycan graph.
#' @param ver A single integer vertex index used as the subtree root.
#'
#' @returns An igraph vertex sequence containing `ver` and every descendant
#'   reachable from `ver` in outgoing edge direction.
#' @noRd
.subtree_vertices <- function(structure, ver) {
  vers <- igraph::bfs(structure, ver, mode = 'out', unreachable = FALSE)$order
  return(vers)
}

#' Shift a residue subtree vertically
#'
#' @param structure An igraph glycan graph.
#' @param ver A single integer vertex index used as the subtree root.
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param offset A numeric amount to add to the subtree `y` coordinates.
#'
#' @returns The same coordinate matrix shape as `coor`, with `y` shifted for
#'   `ver` and its descendants.
#' @noRd
.shift_subtree_y <- function(structure, ver, coor, offset) {
  .shift_subtree_axis(structure, ver, coor, "y", offset)
}

#' Shift one coordinate axis for a residue subtree
#'
#' @param structure An igraph glycan graph.
#' @param ver A single integer vertex index used as the subtree root.
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param axis A string naming the coordinate column to shift, usually `"x"`
#'   or `"y"`.
#' @param offset A numeric amount to add to `axis` for the subtree.
#'
#' @returns The same coordinate matrix shape as `coor`, with `axis` shifted for
#'   `ver` and its descendants.
#' @noRd
.shift_subtree_axis <- function(structure, ver, coor, axis, offset) {
  vers <- .subtree_vertices(structure, ver)
  coor[vers, axis] <- coor[vers, axis] + offset
  return(coor)
}

#' Rotate descendants of an elongated Fuc-like branch
#'
#' @param structure An igraph glycan graph.
#' @param fuc_pos A single integer vertex index whose residue is Fuc-like.
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param branch_offset A numeric vertical offset for the Fuc-like branch. Its
#'   sign determines whether descendants rotate upward or downward.
#'
#' @returns The same coordinate matrix shape as `coor`, with descendants of
#'   `fuc_pos` rotated around the Fuc-like coordinate. If the residue has no
#'   descendants or the offset has no direction, `coor` is returned unchanged.
#' @noRd
.orient_fucose_branch_subtree <- function(
  structure,
  fuc_pos,
  coor,
  branch_offset
) {
  direction <- sign(branch_offset)
  if (!is.finite(direction) || direction == 0) {
    return(coor)
  }

  descendants <- setdiff(
    as.integer(.subtree_vertices(structure, fuc_pos)),
    fuc_pos
  )
  if (length(descendants) == 0) {
    return(coor)
  }

  fuc_x <- coor[fuc_pos, 'x']
  fuc_y <- coor[fuc_pos, 'y']
  relative_x <- coor[descendants, 'x'] - fuc_x
  relative_y <- coor[descendants, 'y'] - fuc_y

  coor[descendants, 'x'] <- fuc_x + relative_y
  coor[descendants, 'y'] <- fuc_y - relative_x * direction
  coor
}

#' Convert Fuc-like linkage positions to vertical branch offsets
#'
#' @param structure An igraph glycan graph whose edge attributes include
#'   `linkage`.
#' @param fuc_pos An integer vector of Fuc-like vertex indices.
#'
#' @returns A numeric vector the same length as `fuc_pos`. Linkages ending in
#'   `2` or `3` return `-1`; other linkage positions return `1`.
#' @noRd
.fucose_branch_y_offsets <- function(structure, fuc_pos) {
  linkage_str <- igraph::E(structure)[fuc_pos]$linkage
  linkage_pos <- purrr::map_chr(strsplit(linkage_str, '-'), 2)
  offset <- rep(1, length(linkage_pos))
  offset[linkage_pos %in% c('2', '3')] <- -1
  return(offset)
}

#' Check whether a residue is nested under a Fuc-like side branch
#'
#' @param structure An igraph glycan graph whose vertices include `mono`.
#' @param ver A single integer vertex index.
#'
#' @returns A logical scalar: `TRUE` when any ancestor between the reducing end
#'   and `ver` uses Fuc-like branch layout.
#' @noRd
.has_fucose_like_layout_ancestor <- function(structure, ver) {
  path <- as.integer(igraph::shortest_paths(
    structure,
    length(structure),
    ver
  )$vpath[[1]])
  ancestors <- setdiff(path[-length(path)], length(structure))
  if (length(ancestors) == 0) {
    return(FALSE)
  }

  any(.is_fucose_like_layout_monosaccharide(
    igraph::V(structure)[ancestors]$mono
  ))
}

#' Order child vertices by linkage position
#'
#' @param structure An igraph glycan graph whose edge attributes include
#'   `linkage` and whose vertices include `mono`.
#' @param ver A single integer vertex index.
#'
#' @returns An igraph vertex sequence of neighbors for `ver`, excluding
#'   Fuc-like neighbors. Numeric linkage positions are sorted decreasing;
#'   unknown or compound linkage values are placed at the bottom.
#'
#' @noRd
.order_child_vertices_by_linkage <- function(structure, ver) {
  neigh_pos <- igraph::neighbors(structure, ver) # Get neighbor vertices
  fuc_pos <- neigh_pos[.is_fucose_like_layout_monosaccharide(neigh_pos$mono)]
  if (length(fuc_pos) != 0) {
    neigh_pos <- neigh_pos[!neigh_pos %in% c(fuc_pos)] # Exclude Fuc-like neighbor vertices
  }
  linkage_chars <- sub('.*-', '', igraph::E(structure)$linkage[neigh_pos])
  neigh_linkage <- suppressWarnings(as.numeric(linkage_chars))

  if (!all(is.na(neigh_linkage))) {
    neigh_linkage[is.na(neigh_linkage)] <- -Inf # Set '?' or '3/6' type as bottom
    arrange_neigh_pos <- neigh_pos[order(neigh_linkage, decreasing = TRUE)]
    return(arrange_neigh_pos)
  } else {
    return(neigh_pos) # No sorting if all linkages are '?' or '3/6' type
  }
}

#' Spread two child subtrees around their parent
#'
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param structure An igraph glycan graph.
#' @param ver A single integer parent vertex with two non-Fuc child neighbors.
#'
#' @returns The same coordinate matrix shape as `coor`, with one child subtree
#'   shifted up by 0.5 and the other shifted down by 0.5.
#' @noRd
.spread_two_child_subtrees <- function(coor, structure, ver) {
  arrange_neigh_pos <- .order_child_vertices_by_linkage(structure, ver)
  coor <- .shift_subtree_y(structure, arrange_neigh_pos[1], coor, 0.5)
  coor <- .shift_subtree_y(structure, arrange_neigh_pos[2], coor, -0.5)
  return(coor)
}

#' Spread three child subtrees around their parent
#'
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param structure An igraph glycan graph.
#' @param ver A single integer parent vertex with three non-Fuc child
#'   neighbors.
#'
#' @returns The same coordinate matrix shape as `coor`, with the outer child
#'   subtrees shifted to `+1` and `-1` around the parent.
#' @noRd
.spread_three_child_subtrees <- function(coor, structure, ver) {
  arrange_neigh_pos <- .order_child_vertices_by_linkage(structure, ver)
  coor <- .shift_subtree_y(structure, arrange_neigh_pos[1], coor, 1)
  coor <- .shift_subtree_y(structure, arrange_neigh_pos[3], coor, -1)
  return(coor)
}

#' Spread non-Fuc-like child subtrees when a parent also has Fuc-like children
#'
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param structure An igraph glycan graph.
#' @param ver A single integer parent vertex with Fuc-like and non-Fuc-like
#'   child neighbors.
#'
#' @returns The same coordinate matrix shape as `coor`, with the two
#'   non-Fuc-like child subtrees shifted to `+0.5` and `-0.5`.
#' @noRd
.spread_non_fucose_children <- function(coor, structure, ver) {
  arrange_other_neigh_pos <- .order_child_vertices_by_linkage(structure, ver)
  coor <- .shift_subtree_y(structure, arrange_other_neigh_pos[1], coor, 0.5)
  coor <- .shift_subtree_y(structure, arrange_other_neigh_pos[2], coor, -0.5)
  return(coor)
}

#' Calculate the vertical shift needed between two subtree profiles
#'
#' @param lower_profile A data frame with columns `vertex`, `x`, and `y`
#'   describing occupied coordinates for a lower subtree.
#' @param upper_profile A data frame with columns `vertex`, `x`, and `y`
#'   describing occupied coordinates for an upper subtree.
#' @param min_gap A numeric minimum vertical gap in any shared `x` column.
#'
#' @returns A numeric scalar giving the smallest upward shift needed so
#'   `upper_profile` is at least `min_gap` above `lower_profile` in every
#'   shared `x` column. Returns 0 when profiles do not share a column.
#' @noRd
.subtree_profile_gap <- function(lower_profile, upper_profile, min_gap = 1) {
  lower_split <- split(lower_profile$y, lower_profile$x)
  upper_split <- split(upper_profile$y, upper_profile$x)
  shared_x <- intersect(names(lower_split), names(upper_split))
  if (length(shared_x) == 0) {
    return(0)
  }

  max(purrr::map_dbl(
    shared_x,
    \(x) max(lower_split[[x]]) + min_gap - min(upper_split[[x]])
  ))
}

#' Shift a compact subtree layout vertically
#'
#' @param layout A list with numeric vector `y` and data frame `profile`, as
#'   returned by `.compact_subtree_coordinates()`.
#' @param offset A numeric vertical offset to add to all `y` values.
#'
#' @returns A layout list with the same fields as `layout`, with every `y`
#'   value shifted by `offset`.
#' @noRd
.shift_subtree_layout <- function(layout, offset) {
  layout$y <- layout$y + offset
  layout$profile$y <- layout$profile$y + offset
  layout
}

#' Pack child subtree layouts as tightly as possible
#'
#' @param layouts A list of child subtree layout lists in bottom-to-top branch
#'   order. Each layout has vector `y` and data frame `profile`.
#' @param preferred_shifts Numeric vector with one preferred child-root shift per
#'   layout. Defaults to zero shifts.
#' @param fixed_shifts Logical vector with one value per layout. `TRUE` entries
#'   are kept at their preferred shifts unless there is no flexible sibling that
#'   can resolve a collision.
#' @param min_gap A numeric minimum vertical gap in any shared `x` column.
#'
#' @returns A numeric vector with one vertical shift per layout. Shifts stay as
#'   compact as possible while preserving anchored Fuc-like child positions.
#' @noRd
.pack_child_subtree_layouts <- function(
  layouts,
  preferred_shifts = NULL,
  fixed_shifts = NULL,
  min_gap = 1
) {
  layout_num <- length(layouts)
  if (layout_num == 0) {
    return(numeric(0))
  }

  if (is.null(preferred_shifts)) {
    preferred_shifts <- rep(0, layout_num)
  }
  if (is.null(fixed_shifts)) {
    fixed_shifts <- rep(FALSE, layout_num)
  }
  checkmate::assert_numeric(
    preferred_shifts,
    any.missing = FALSE,
    len = layout_num
  )
  checkmate::assert_logical(
    fixed_shifts,
    any.missing = FALSE,
    len = layout_num
  )

  # Keep the historical compact layout unless a caller has a child that must
  # retain its rough-layout side. This avoids applying rough offsets broadly:
  # they are branch-order hints, not final coordinates for ordinary children.
  shifts <- .pack_child_subtree_layouts_tightly(layouts, min_gap)
  if (!any(fixed_shifts)) {
    return(shifts)
  }

  # Fixed shifts are reserved for children whose rough-layout offset already
  # carries semantic information. For now that means leaf Fuc-like children,
  # whose a1-3/a1-6 linkages decide whether they sit below or above the parent.
  shifts[fixed_shifts] <- as.numeric(preferred_shifts[fixed_shifts])
  shifts <- .apply_isolated_preferred_shifts(
    layouts,
    shifts,
    preferred_shifts,
    fixed_shifts
  )
  for (iter in seq_len(layout_num * layout_num)) {
    result <- .pack_child_subtree_layouts_once(
      layouts,
      shifts,
      fixed_shifts,
      min_gap
    )
    shifts <- result$shifts
    if (!result$changed) {
      break
    }
  }

  shifts
}

#' Pack child subtree layouts without preferred anchors
#'
#' @inheritParams .pack_child_subtree_layouts
#'
#' @returns A numeric vector with one vertical shift per layout. Shifts are
#'   centered around zero so the packed group stays near the parent.
#' @noRd
.pack_child_subtree_layouts_tightly <- function(layouts, min_gap = 1) {
  layout_num <- length(layouts)
  shifts <- rep(0, layout_num)

  for (i in seq_len(layout_num)) {
    if (i == 1) {
      next
    }

    shifts[i] <- max(purrr::map_dbl(
      seq_len(i - 1),
      \(j) {
        shifts[j] +
          .subtree_profile_gap(
            layouts[[j]]$profile,
            layouts[[i]]$profile,
            min_gap
          )
      }
    ))
  }

  shifts - mean(range(shifts))
}

#' Apply preferred shifts to isolated flexible child layouts
#'
#' @param layouts A list of child subtree layout lists in bottom-to-top branch
#'   order.
#' @param shifts Numeric vector with one current vertical shift per layout.
#' @param preferred_shifts Numeric vector with one preferred child-root shift per
#'   layout.
#' @param fixed_shifts Logical vector with one anchoring flag per layout.
#'
#' @returns A numeric vector of updated `shifts`. Flexible centered children are
#'   restored to their preferred shift only when they cannot collide with any
#'   sibling subtree columns.
#' @noRd
.apply_isolated_preferred_shifts <- function(
  layouts,
  shifts,
  preferred_shifts,
  fixed_shifts
) {
  flexible_centered <- which(
    !fixed_shifts &
      abs(preferred_shifts) <= sqrt(.Machine$double.eps)
  )
  if (length(flexible_centered) == 0) {
    return(shifts)
  }

  for (child in flexible_centered) {
    # This replaces the old post-pack `.restore_isolated_centered_child()`
    # repair. A centered arm may return to its preferred y = 0 only when its
    # subtree columns cannot collide with sibling subtree columns.
    child_x <- unique(layouts[[child]]$profile$x)
    sibling_x <- unique(unlist(purrr::map(
      layouts[-child],
      \(layout) layout$profile$x
    )))
    if (length(intersect(child_x, sibling_x)) == 0) {
      shifts[child] <- preferred_shifts[child]
    }
  }

  shifts
}

#' Run one sibling-spacing pass for child subtree layouts
#'
#' @param layouts A list of child subtree layout lists in bottom-to-top branch
#'   order.
#' @param shifts Numeric vector with one current vertical shift per layout.
#' @param fixed_shifts Logical vector with one anchoring flag per layout.
#' @param min_gap A numeric minimum vertical gap in any shared `x` column.
#'
#' @returns A list with adjusted `shifts` and logical `changed`.
#' @noRd
.pack_child_subtree_layouts_once <- function(
  layouts,
  shifts,
  fixed_shifts,
  min_gap
) {
  layout_num <- length(layouts)
  changed <- FALSE
  tolerance <- sqrt(.Machine$double.eps)

  for (i in seq_len(layout_num)) {
    if (i == 1) {
      next
    }

    for (j in seq_len(i - 1)) {
      required_shift <- shifts[j] +
        .subtree_profile_gap(
          layouts[[j]]$profile,
          layouts[[i]]$profile,
          min_gap
        )
      violation <- required_shift - shifts[i]
      if (violation <= tolerance) {
        next
      }

      # Move a flexible subtree when possible. If the upper subtree is fixed,
      # push the lower flexible sibling down instead; if both are fixed, keep
      # bottom-to-top ordering by moving the upper subtree as the last resort.
      if (!fixed_shifts[i]) {
        shifts[i] <- shifts[i] + violation
      } else if (!fixed_shifts[j]) {
        shifts[j] <- shifts[j] - violation
      } else {
        shifts[i] <- shifts[i] + violation
      }
      changed <- TRUE
    }
  }

  list(shifts = shifts, changed = changed)
}

#' Infer whether a child subtree should stay below or above its parent
#'
#' @param rough_offset Numeric rough child root offset from the parent before
#'   compaction.
#' @param shift Numeric compacted child root shift from the parent.
#' @param index Integer child index in bottom-to-top order.
#' @param child_num Integer number of child subtrees.
#'
#' @returns A numeric scalar: `-1` means the child is treated as below the
#'   parent; `1` means above the parent.
#' @noRd
.child_subtree_side <- function(
  rough_offset,
  shift,
  index,
  child_num
) {
  direction <- sign(rough_offset)
  if (!is.finite(direction) || direction == 0) {
    direction <- sign(shift)
  }
  if (!is.finite(direction) || direction == 0) {
    direction <- ifelse(index <= child_num / 2, -1, 1)
  }
  direction
}

#' Enforce spacing between child subtrees and their parent column
#'
#' @param layouts A list of child subtree layout lists in bottom-to-top branch
#'   order.
#' @param shifts A numeric vector of proposed vertical shifts, one per layout.
#' @param rough_offsets A numeric vector of child root offsets from the parent
#'   before compaction.
#' @param parent_x Numeric `x` coordinate of the parent residue.
#' @param min_gap A numeric minimum vertical gap in the parent `x` column.
#'
#' @returns A numeric vector of adjusted shifts, the same length as `shifts`.
#' @noRd
.separate_children_from_parent_column <- function(
  layouts,
  shifts,
  rough_offsets,
  parent_x,
  min_gap = 1
) {
  layout_num <- length(layouts)
  if (layout_num == 0) {
    return(shifts)
  }

  for (iter in seq_len(layout_num + 1)) {
    result <- .separate_parent_column_once(
      layouts,
      shifts,
      rough_offsets,
      parent_x,
      min_gap
    )
    shifts <- result$shifts

    if (!result$changed) {
      break
    }
  }

  shifts
}

#' Run one parent-column spacing pass for child subtrees
#'
#' @param layouts A list of child subtree layout lists in bottom-to-top branch
#'   order.
#' @param shifts A numeric vector of proposed vertical shifts, one per layout.
#' @param rough_offsets A numeric vector of child root offsets from the parent
#'   before compaction.
#' @param parent_x Numeric `x` coordinate of the parent residue.
#' @param min_gap A numeric minimum vertical gap in the parent `x` column.
#'
#' @returns A list with `shifts`, the adjusted numeric shift vector, and
#'   `changed`, a logical scalar indicating whether any shift changed.
#' @noRd
.separate_parent_column_once <- function(
  layouts,
  shifts,
  rough_offsets,
  parent_x,
  min_gap
) {
  shifted <- purrr::map2(layouts, shifts, .shift_subtree_layout)
  changed <- FALSE
  layout_num <- length(layouts)

  for (i in seq_len(layout_num)) {
    profile <- shifted[[i]]$profile
    parent_column_y <- profile$y[profile$x == parent_x]
    direction <- .child_subtree_side(
      rough_offsets[i],
      shifts[i],
      i,
      layout_num
    )
    adjustment <- .parent_column_shift_adjustment(
      parent_column_y,
      direction,
      min_gap
    )

    if (adjustment < 0) {
      shifts[seq_len(i)] <- shifts[seq_len(i)] + adjustment
      changed <- TRUE
    }
    if (adjustment > 0) {
      shifts[seq(i, layout_num)] <- shifts[seq(i, layout_num)] + adjustment
      changed <- TRUE
    }
  }

  list(shifts = shifts, changed = changed)
}

#' Calculate the signed shift needed away from the parent column
#'
#' @param parent_column_y A numeric vector of child-subtree y-coordinates that
#'   occupy the parent `x` column after the current shifts.
#' @param direction Numeric child side from `.child_subtree_side()`: negative
#'   for below the parent, positive for above.
#' @param min_gap A numeric minimum vertical gap from the parent coordinate.
#'
#' @returns A numeric scalar. Negative values mean lower child subtrees should
#'   move downward, positive values mean upper child subtrees should move
#'   upward, and zero means no movement is needed.
#' @noRd
.parent_column_shift_adjustment <- function(
  parent_column_y,
  direction,
  min_gap
) {
  if (length(parent_column_y) == 0) {
    return(0)
  }

  if (direction < 0) {
    violation <- max(parent_column_y) + min_gap
    if (violation > sqrt(.Machine$double.eps)) {
      return(-violation)
    }
    return(0)
  }

  violation <- min_gap - min(parent_column_y)
  if (violation > sqrt(.Machine$double.eps)) {
    return(violation)
  }
  0
}

#' Identify child subtrees whose root shift should stay anchored
#'
#' @param structure An igraph glycan graph whose vertices include `mono`.
#' @param child_pos Integer vector of child vertex indices in bottom-to-top order.
#'
#' @returns A logical vector the same length as `child_pos`. Single-residue
#'   Fuc-like children are anchored because their rough shift already encodes
#'   the linkage-specific side.
#' @noRd
.fixed_child_subtree_shifts <- function(structure, child_pos) {
  purrr::map_lgl(child_pos, function(child) {
    # Only leaf Fuc-like children are anchored. Elongated branches need normal
    # compaction because their descendants can create real subtree collisions.
    .is_fucose_like_layout_monosaccharide(igraph::V(structure)[[child]]$mono) &&
      length(igraph::neighbors(structure, child, mode = "out")) == 0
  })
}

#' Recursively compact coordinates for one residue subtree
#'
#' @param structure An igraph glycan graph.
#' @param ver A single integer vertex index used as the subtree root.
#' @param coor A numeric coordinate matrix with columns `x` and `y`, usually
#'   from the rough coordinate pass.
#' @param min_gap A numeric minimum vertical gap in any shared `x` column.
#'
#' @returns A list with `y`, a named numeric vector of compacted relative
#'   y-coordinates by vertex id, and `profile`, a data frame with columns
#'   `vertex`, `x`, and `y` describing occupied columns for the subtree.
#' @noRd
.compact_subtree_coordinates <- function(structure, ver, coor, min_gap = 1) {
  child_pos <- as.integer(igraph::neighbors(structure, ver, mode = "out"))
  vertex_profile <- data.frame(
    vertex = ver,
    x = as.numeric(coor[ver, "x"]),
    y = 0
  )
  if (length(child_pos) == 0) {
    return(list(
      y = stats::setNames(0, as.character(ver)),
      profile = vertex_profile
    ))
  }

  rough_offsets <- as.numeric(coor[child_pos, "y"] - coor[ver, "y"])
  child_order <- order(rough_offsets, child_pos)
  child_pos <- child_pos[child_order]
  rough_offsets <- rough_offsets[child_order]
  layouts <- purrr::map(
    child_pos,
    \(child) .compact_subtree_coordinates(structure, child, coor, min_gap)
  )
  # Rough offsets choose branch side and ordering. The packer may use them as
  # fixed positions for leaf Fuc-like children, but ordinary child subtrees
  # still use the compact centered packing path.
  shifts <- .pack_child_subtree_layouts(
    layouts,
    preferred_shifts = rough_offsets,
    fixed_shifts = .fixed_child_subtree_shifts(structure, child_pos),
    min_gap = min_gap
  )
  shifts <- .separate_children_from_parent_column(
    layouts,
    shifts,
    rough_offsets,
    parent_x = as.numeric(coor[ver, "x"]),
    min_gap = min_gap
  )
  shifted_layouts <- purrr::map2(layouts, shifts, .shift_subtree_layout)

  list(
    y = c(
      stats::setNames(0, as.character(ver)),
      unlist(purrr::map(shifted_layouts, "y"))
    ),
    profile = dplyr::bind_rows(
      vertex_profile,
      purrr::map(shifted_layouts, "profile")
    )
  )
}

#' Compact a complete coordinate matrix after rough layout
#'
#' @param structure An igraph glycan graph.
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param min_gap A numeric minimum vertical gap in any shared `x` column.
#'
#' @returns A coordinate matrix with the same dimensions and column names as
#'   `coor`, with `y` replaced by compacted values.
#' @noRd
.compact_coordinates <- function(structure, coor, min_gap = 1) {
  layout <- .compact_subtree_coordinates(
    structure,
    length(structure),
    coor,
    min_gap = min_gap
  )
  vertex_pos <- as.integer(names(layout$y))
  coor[vertex_pos, "y"] <- as.numeric(layout$y)
  coor
}

#' Get vertices from reducing end to non-reducing end
#'
#' @param structure An igraph glycan graph.
#'
#' @returns An integer vector of vertex indices in reverse graph order, starting
#'   with the reducing-end vertex and ending with vertex 1.
#' @noRd
.reverse_vertex_order <- function(structure) {
  seq(length(structure), 1)
}

#' Apply the rough branch-spreading pass to all vertices
#'
#' @param structure An igraph glycan graph whose vertices include `mono`.
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#'
#' @returns The same coordinate matrix shape as `coor`, with two-child,
#'   three-child, and Fuc-like-containing branch subtrees vertically spread
#'   before final compaction.
#' @noRd
.spread_rough_child_subtrees <- function(structure, coor) {
  for (ver in .reverse_vertex_order(structure)) {
    coor <- .spread_child_subtrees_for_vertex(coor, structure, ver)
  }
  coor
}

#' Spread rough child subtree positions for one parent vertex
#'
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param structure An igraph glycan graph whose vertices include `mono`.
#' @param ver A single integer parent vertex index.
#'
#' @returns The same coordinate matrix shape as `coor`. Branching vertices may
#'   have child subtrees shifted up or down; non-branching vertices return
#'   `coor` unchanged.
#' @noRd
.spread_child_subtrees_for_vertex <- function(coor, structure, ver) {
  gly_neighbors <- igraph::neighbors(structure, ver)
  is_fucose_like <- .is_fucose_like_layout_monosaccharide(gly_neighbors$mono)
  has_fucose <- any(is_fucose_like)
  neighbor_num <- length(gly_neighbors)
  non_fucose_num <- sum(!is_fucose_like)

  if (neighbor_num == 2 && !has_fucose) {
    return(.spread_two_child_subtrees(coor, structure, ver))
  }
  if (neighbor_num == 3 && !has_fucose) {
    return(.spread_three_child_subtrees(coor, structure, ver))
  }
  if (has_fucose && non_fucose_num == 2) {
    return(.spread_non_fucose_children(coor, structure, ver))
  }
  coor
}

#' Apply Fuc-like branch offsets and orientation to all vertices
#'
#' @param structure An igraph glycan graph whose vertices include `mono` and
#'   whose edges include `linkage`.
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#'
#' @returns The same coordinate matrix shape as `coor`, with Fuc-like branches
#'   moved to the linkage-specific side and any elongated descendants rotated
#'   with the branch.
#' @noRd
.orient_fucose_branch_subtrees <- function(structure, coor) {
  for (ver in .reverse_vertex_order(structure)) {
    coor <- .orient_fucose_children_for_vertex(structure, coor, ver)
  }
  coor
}

#' Apply Fuc-like branch offsets and orientation for one parent vertex
#'
#' @param structure An igraph glycan graph whose vertices include `mono` and
#'   whose edges include `linkage`.
#' @param coor A numeric coordinate matrix with columns `x` and `y`.
#' @param ver A single integer parent vertex index.
#'
#' @returns The same coordinate matrix shape as `coor`. If `ver` has Fuc-like
#'   children, each subtree is shifted and elongated descendants are oriented
#'   with the branch; otherwise `coor` is returned unchanged.
#' @noRd
.orient_fucose_children_for_vertex <- function(structure, coor, ver) {
  neigh_pos <- igraph::neighbors(structure, ver)
  fuc_pos <- neigh_pos[.is_fucose_like_layout_monosaccharide(neigh_pos$mono)]
  if (length(fuc_pos) == 0) {
    return(coor)
  }
  fuc_pos <- fuc_pos[
    !purrr::map_lgl(
      as.integer(fuc_pos),
      \(fuc_vertex) .has_fucose_like_layout_ancestor(structure, fuc_vertex)
    )
  ]
  if (length(fuc_pos) == 0) {
    return(coor)
  }

  purrr::reduce2(
    as.integer(fuc_pos),
    .fucose_branch_y_offsets(structure, fuc_pos),
    function(coor_acc, fuc_vertex, offset) {
      coor_acc <- .shift_subtree_y(structure, fuc_vertex, coor_acc, offset)
      .orient_fucose_branch_subtree(structure, fuc_vertex, coor_acc, offset)
    },
    .init = coor
  )
}

#' Calculate final residue coordinates for a glycan graph
#'
#' @param structure An igraph glycan graph. Vertices must be in glyrepr order
#'   and include `mono`; edges must include `linkage`.
#'
#' @returns A numeric matrix with one row per vertex and columns `x` and `y`.
#'   Coordinates are horizontal drawing coordinates before optional vertical
#'   orientation rotation.
#' @noRd
.calculate_residue_coordinates <- function(structure) {
  coor <- .initialize_residue_coordinates(structure)
  coor <- .spread_rough_child_subtrees(structure, coor)
  coor <- .orient_fucose_branch_subtrees(structure, coor)
  coor <- .compact_coordinates(structure, coor)
  return(coor)
}

#' Build line-segment coordinates for glycosidic connections
#'
#' @param structure An igraph glycan graph.
#' @param coor A numeric coordinate matrix with columns `x` and `y`, one row
#'   per graph vertex.
#'
#' @returns A list with numeric vectors `start_x`, `start_y`, `end_x`, and
#'   `end_y`, one element per edge. Empty graphs return zero-length vectors.
#' @noRd
.connection_segment_data <- function(structure, coor) {
  edges <- igraph::as_edgelist(structure, names = FALSE)
  if (nrow(edges) == 0) {
    return(list(
      start_x = numeric(0),
      start_y = numeric(0),
      end_x = numeric(0),
      end_y = numeric(0)
    ))
  }

  list(
    start_x = coor[edges[, 1], 'x'],
    start_y = coor[edges[, 1], 'y'],
    end_x = coor[edges[, 2], 'x'],
    end_y = coor[edges[, 2], 'y']
  )
}

#' Get drawable residue shape names for graph vertices
#'
#' @param structure An igraph glycan graph whose vertices include `mono`.
#' @param coor A numeric coordinate matrix with rendered columns `x` and `y`,
#'   one row per graph vertex.
#' @param fuc_orient Fuc-like triangle orientation, either `"flex"` or `"up"`.
#'
#' @returns A character vector with one value per vertex. Most values are copied
#'   from `mono`; in flexible Fuc-like orientation, non-reducing Fuc-like
#'   orientable residues are mapped to directional shape names so the triangle
#'   apex points toward the rendered linkage direction.
#' @noRd
.residue_glycoforms <- function(
  structure,
  coor = NULL,
  fuc_orient = c("flex", "up")
) {
  fuc_orient <- rlang::arg_match(fuc_orient)
  glycoform <- igraph::V(structure)$mono
  if (fuc_orient == "up") {
    return(glycoform)
  }
  if (is.null(coor)) {
    coor <- .calculate_residue_coordinates(structure)
  }

  for (i in which(.is_fucose_like_orient_monosaccharide(glycoform))) {
    if (!.is_reducing_end(structure, i)) {
      glycoform[i] <- .fucose_directional_glycoform(structure, coor, i)
    }
  }
  return(glycoform)
}

#' Get directional Fuc-like shape name from rendered linkage direction
#'
#' @param structure An igraph glycan graph.
#' @param coor A numeric coordinate matrix with rendered columns `x` and `y`.
#' @param fuc_pos A single integer vertex index whose residue is Fuc-like.
#'
#' @returns A glycoform string with no suffix for up-pointing shapes, an `"Up"`
#'   suffix for down-pointing shapes, a `"Right"` suffix for right-pointing
#'   shapes, and a `"Left"` suffix for left-pointing shapes.
#' @noRd
.fucose_directional_glycoform <- function(structure, coor, fuc_pos) {
  mono <- igraph::V(structure)[[fuc_pos]]$mono
  parent_pos <- as.integer(igraph::neighbors(structure, fuc_pos, mode = "in"))
  if (length(parent_pos) == 0) {
    return(mono)
  }

  linkage_vec <- coor[parent_pos[1], c("x", "y")] - coor[fuc_pos, c("x", "y")]
  if (abs(linkage_vec[["x"]]) > abs(linkage_vec[["y"]])) {
    if (linkage_vec[["x"]] > 0) {
      return(paste0(mono, "Right"))
    }
    return(paste0(mono, "Left"))
  }
  if (linkage_vec[["y"]] < 0) {
    return(paste0(mono, "Up"))
  }
  mono
}
