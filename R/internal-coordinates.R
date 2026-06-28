#' Initialize Vertices Coordinate
#'
#' @param structure an igraph object
#'
#' @returns Initialized Coordinate which Y=0, X are correct
#' @noRd
.coor_initialization <- function(structure) {
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
      igraph::V(structure)[[i]]$mono == 'Fuc' && !.is_reducing_end(structure, i)
    ) {
      coor <- .offset_chil_axis(structure, i, coor, "x", 1)
    }
  }
  return(coor)
}

#' Check whether a vertex is the reducing end.
#'
#' @param structure an igraph object.
#' @param ver an integer vertex index.
#'
#' @returns A logical scalar.
#' @noRd
.is_reducing_end <- function(structure, ver) {
  ver == length(structure)
}

#' Find Sequence Number of Sub-module
#'
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns the child vertices of structure
#'
#' @examples .chil_coor(structure, 3)
#' @noRd
.chil_coor <- function(structure, ver) {
  vers <- igraph::bfs(structure, ver, mode = 'out', unreachable = FALSE)$order
  return(vers)
}

#' Offset y-Coordinate of Vertex Sub-module
#'
#' @param structure an igraph object
#' @param ver an integer
#' @param coor a matrix
#' @param offset a float
#'
#' @returns offset coordinate
#'
#' @examples .offset_chil_coor(structure, 3, coor, 0.5)
#' @noRd
.offset_chil_coor <- function(structure, ver, coor, offset) {
  .offset_chil_axis(structure, ver, coor, "y", offset)
}

#' Offset one coordinate axis for a vertex subtree.
#'
#' @param structure an igraph object
#' @param ver an integer vertex index
#' @param coor a coordinate matrix
#' @param axis coordinate axis to offset
#' @param offset a numeric offset
#'
#' @returns offset coordinate matrix
#' @noRd
.offset_chil_axis <- function(structure, ver, coor, axis, offset) {
  vers <- .chil_coor(structure, ver)
  coor[vers, axis] <- coor[vers, axis] + offset
  return(coor)
}

#' Rotate a Fuc subtree into the Fuc branch direction.
#'
#' @param structure an igraph object
#' @param fuc_pos an integer Fuc vertex index
#' @param coor a coordinate matrix
#' @param branch_offset a numeric vertical offset for the Fuc branch
#'
#' @returns coordinate matrix with the Fuc descendants rotated
#' @noRd
.orient_fucose_subtree <- function(structure, fuc_pos, coor, branch_offset) {
  direction <- sign(branch_offset)
  if (!is.finite(direction) || direction == 0) {
    return(coor)
  }

  descendants <- setdiff(as.integer(.chil_coor(structure, fuc_pos)), fuc_pos)
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

#' Process the y-Coordinate of 'Fucose' Vertices
#'
#' @param structure an igraph object
#' @param fuc_pos An integer vector of 'Fucose' vertex indices.
#'
#' @returns A numeric vector of vertical offsets for the specified vertices.
#'
#' @examples .fuc_offset(structure, c(1, 2))
#' @noRd
.fuc_offset <- function(structure, fuc_pos) {
  linkage_str <- igraph::E(structure)[fuc_pos]$linkage
  linkage_pos <- purrr::map_chr(strsplit(linkage_str, '-'), 2)
  offset <- rep(1, length(linkage_pos))
  offset[linkage_pos %in% c('2', '3')] <- -1
  return(offset)
}

#' Sorting the linkage locations of neighbor vertices
#'
#' @param structure an igraph object
#' @param ver the vertex position
#'
#' @returns The sorting neighbor positions
#'
#' @noRd
.neigh_pos_order <- function(structure, ver) {
  neigh_pos <- igraph::neighbors(structure, ver) # Get neighbor vertices
  fuc_pos <- neigh_pos[which(neigh_pos$mono == 'Fuc')]
  if (length(fuc_pos) != 0) {
    neigh_pos <- neigh_pos[!neigh_pos %in% c(fuc_pos)] # Exclude Fucose in neighbor vertices
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

#' Process the Vertices which Out-degree =2 along the Path
#'
#' @param coor a matrix
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns Processed coordinate
#'
#' @examples .process_two_neighbors(coor, structure, 5)
#' @noRd
.process_two_neighbors <- function(coor, structure, ver) {
  arrange_neigh_pos <- .neigh_pos_order(structure, ver)
  coor <- .offset_chil_coor(structure, arrange_neigh_pos[1], coor, 0.5)
  coor <- .offset_chil_coor(structure, arrange_neigh_pos[2], coor, -0.5)
  return(coor)
}

#' Process the Vertices which Out-degree =3 along the Path
#'
#' @param coor a matrix
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns Processed coordinate
#'
#' @examples .process_three_neighbors(coor, structure, 6)
#' @noRd
.process_three_neighbors <- function(coor, structure, ver) {
  arrange_neigh_pos <- .neigh_pos_order(structure, ver)
  coor <- .offset_chil_coor(structure, arrange_neigh_pos[1], coor, 1)
  coor <- .offset_chil_coor(structure, arrange_neigh_pos[3], coor, -1)
  return(coor)
}

#' Process the Vertices adjacent to 'Fucose'
#'
#' @param coor a matrix
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns Processed coordinate
#'
#' @examples .process_contain_fucose_neighbors(coor, structure, 7)
#' @noRd
.process_contain_fucose_neighbors <- function(coor, structure, ver) {
  arrange_other_neigh_pos <- .neigh_pos_order(structure, ver)
  coor <- .offset_chil_coor(structure, arrange_other_neigh_pos[1], coor, 0.5)
  coor <- .offset_chil_coor(structure, arrange_other_neigh_pos[2], coor, -0.5)
  return(coor)
}

#' Calculate the vertical gap needed between two subtree profiles.
#'
#' @param lower_profile a subtree profile data frame
#' @param upper_profile a subtree profile data frame
#' @param min_gap minimum vertical gap in a shared column
#'
#' @returns minimum shift needed to place `upper_profile` above `lower_profile`
#' @noRd
.profile_gap <- function(lower_profile, upper_profile, min_gap = 1) {
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

#' Shift a compact subtree layout vertically.
#'
#' @param layout a compact subtree layout
#' @param offset vertical offset
#'
#' @returns shifted compact subtree layout
#' @noRd
.shift_compact_layout <- function(layout, offset) {
  layout$y <- layout$y + offset
  layout$profile$y <- layout$profile$y + offset
  layout
}

#' Pack child subtree layouts as tightly as possible.
#'
#' @param layouts ordered child subtree layouts
#' @param min_gap minimum vertical gap in a shared column
#'
#' @returns numeric vector of vertical shifts
#' @noRd
.pack_child_layouts <- function(layouts, min_gap = 1) {
  layout_num <- length(layouts)
  if (layout_num == 0) {
    return(numeric(0))
  }

  shifts <- rep(0, layout_num)
  for (i in seq_len(layout_num)) {
    if (i == 1) {
      next
    }

    shifts[i] <- max(purrr::map_dbl(
      seq_len(i - 1),
      \(j) {
        shifts[j] +
          .profile_gap(
            layouts[[j]]$profile,
            layouts[[i]]$profile,
            min_gap
          )
      }
    ))
  }

  shifts - mean(range(shifts))
}

#' Infer whether a child subtree should sit below or above its parent.
#'
#' @param rough_offset rough child root offset from the parent
#' @param shift compacted child root shift from the parent
#' @param index child index in bottom-to-top order
#' @param child_num number of children
#'
#' @returns -1 for below the parent, 1 for above the parent
#' @noRd
.child_compaction_direction <- function(
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

#' Keep child subtrees away from their parent in a shared column.
#'
#' @param layouts ordered child subtree layouts
#' @param shifts child layout shifts
#' @param rough_offsets rough child root offsets from the parent
#' @param parent_x parent x coordinate
#' @param min_gap minimum vertical gap in a shared column
#'
#' @returns adjusted child layout shifts
#' @noRd
.enforce_parent_profile_gap <- function(
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
    shifted <- purrr::map2(layouts, shifts, .shift_compact_layout)
    changed <- FALSE

    for (i in seq_len(layout_num)) {
      profile <- shifted[[i]]$profile
      parent_column_y <- profile$y[profile$x == parent_x]
      if (length(parent_column_y) == 0) {
        next
      }

      direction <- .child_compaction_direction(
        rough_offsets[i],
        shifts[i],
        i,
        layout_num
      )
      if (direction < 0) {
        violation <- max(parent_column_y) + min_gap
        if (violation > sqrt(.Machine$double.eps)) {
          shifts[seq_len(i)] <- shifts[seq_len(i)] - violation
          changed <- TRUE
        }
      } else {
        violation <- min_gap - min(parent_column_y)
        if (violation > sqrt(.Machine$double.eps)) {
          shifts[seq(i, layout_num)] <- shifts[seq(i, layout_num)] + violation
          changed <- TRUE
        }
      }
    }

    if (!changed) {
      break
    }
  }

  shifts
}

#' Compact a subtree layout while preserving the current branch order.
#'
#' @param structure an igraph object
#' @param ver subtree root vertex
#' @param coor current coordinate matrix
#' @param min_gap minimum vertical gap in a shared column
#'
#' @returns a compact subtree layout
#' @noRd
.compact_subtree_layout <- function(structure, ver, coor, min_gap = 1) {
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
    \(child) .compact_subtree_layout(structure, child, coor, min_gap)
  )
  shifts <- .pack_child_layouts(layouts, min_gap)
  shifts <- .enforce_parent_profile_gap(
    layouts,
    shifts,
    rough_offsets,
    parent_x = as.numeric(coor[ver, "x"]),
    min_gap = min_gap
  )
  shifted_layouts <- purrr::map2(layouts, shifts, .shift_compact_layout)

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

#' Compact a complete coordinate matrix after rough layout.
#'
#' @param structure an igraph object
#' @param coor current coordinate matrix
#' @param min_gap minimum vertical gap in a shared column
#'
#' @returns compacted coordinate matrix
#' @noRd
.compact_coor <- function(structure, coor, min_gap = 1) {
  layout <- .compact_subtree_layout(
    structure,
    length(structure),
    coor,
    min_gap = min_gap
  )
  vertex_pos <- as.integer(names(layout$y))
  coor[vertex_pos, "y"] <- as.numeric(layout$y)
  coor
}

#' Process the Coordinate of all Vertices
#'
#' @param structure an igraph object
#'
#' @returns Processed Coordinate
#'
#' @examples .coor_cal(structure)
#' @noRd
.coor_cal <- function(structure) {
  coor <- .coor_initialization(structure)
  structure_length <- seq(length(structure), 1)
  for (i in structure_length) {
    gly_neighbors <- igraph::neighbors(structure, i)
    if (
      length(gly_neighbors) == 2 &&
        !('Fuc' %in% gly_neighbors$mono)
    ) {
      coor <- .process_two_neighbors(coor, structure, i)
    }
    if (
      length(gly_neighbors) == 3 &&
        !('Fuc' %in% gly_neighbors$mono)
    ) {
      coor <- .process_three_neighbors(coor, structure, i)
    }
    if (
      length(gly_neighbors) == 3 &&
        'Fuc' %in% gly_neighbors$mono
    ) {
      coor <- .process_contain_fucose_neighbors(coor, structure, i)
    }
  }
  for (i in structure_length) {
    if ('Fuc' %in% igraph::neighbors(structure, i)$mono) {
      neigh_pos <- igraph::neighbors(structure, i)
      fuc_pos <- neigh_pos[which(neigh_pos$mono == 'Fuc')]
      coor <- purrr::reduce2(
        as.integer(fuc_pos),
        .fuc_offset(structure, fuc_pos),
        function(coor_acc, fuc_vertex, offset) {
          coor_acc <- .offset_chil_coor(structure, fuc_vertex, coor_acc, offset)
          .orient_fucose_subtree(structure, fuc_vertex, coor_acc, offset)
        },
        .init = coor
      )
    }
  }
  coor <- .compact_coor(structure, coor)
  return(coor)
}

#' Analysis the Connection Information of all Vertices
#'
#' @param structure an igraph object
#' @param coor a matrix
#'
#' @returns Connection Information
#'
#' @examples .connect_info(structure, coor)
#' @noRd
.connect_info <- function(structure, coor) {
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

#' Gain the glycoforms corresponding to the vertices
#'
#' @param structure an igraph object
#'
#' @returns list of glycoform
#'
#' @examples .glycoform_info(structure)
#' @noRd
.glycoform_info <- function(structure) {
  glycoform <- igraph::V(structure)$mono
  for (i in c(which(glycoform == 'Fuc'))) {
    if (!.is_reducing_end(structure, i) && .fuc_offset(structure, i) > 0) {
      glycoform[i] <- 'FucUp'
    }
  }
  return(glycoform)
}
