# glycan mapping
glycan_color <- c(
  'glyWhite' = '#FFFFFF',
  'glyBlue' = '#0072BC',
  'glyGreen' = '#00A651',
  'glyYellow' = '#FFD400',
  'glyOrange' = '#F47920',
  'glyPink' = '#F69EA1',
  'glyPurple' = '#A54399',
  'glyLightBlue' = '#8FCCE9',
  'glyBrown' = '#A17A4D',
  'glyRed' = '#ED1C24'
)

glycan_dict <- list(
  'Hex' = c('Hex', 'glyWhite'),
  'Glc' = c('Hex', 'glyBlue'),
  'Man' = c('Hex', 'glyGreen'),
  'Gal' = c('Hex', 'glyYellow'),
  'Gul' = c('Hex', 'glyOrange'),
  'Alt' = c('Hex', 'glyPink'),
  'All' = c('Hex', 'glyPurple'),
  'Tal' = c('Hex', 'glyLightBlue'),
  'Ido' = c('Hex', 'glyBrown'),

  'HexNAc' = c('HexNAc', 'glyWhite'),
  'GlcNAc' = c('HexNAc', 'glyBlue'),
  'ManNAc' = c('HexNAc', 'glyGreen'),
  'GalNAc' = c('HexNAc', 'glyYellow'),
  'GulNAc' = c('HexNAc', 'glyOrange'),
  'AltNAc' = c('HexNAc', 'glyPink'),
  'AllNAc' = c('HexNAc', 'glyPurple'),
  'TalNAc' = c('HexNAc', 'glyLightBlue'),
  'IdoNAc' = c('HexNAc', 'glyBrown'),

  'HexN' = c('HexN', 'glyWhite', 'glyWhite'),
  'GlcN' = c('HexN', 'glyBlue', 'glyWhite'),
  'ManN' = c('HexN', 'glyGreen', 'glyWhite'),
  'GalN' = c('HexN', 'glyYellow', 'glyWhite'),
  'GulN' = c('HexN', 'glyOrange', 'glyWhite'),
  'AltN' = c('HexN', 'glyPink', 'glyWhite'),
  'AllN' = c('HexN', 'glyPurple', 'glyWhite'),
  'TalN' = c('HexN', 'glyLightBlue', 'glyWhite'),
  'IdoN' = c('HexN', 'glyBrown', 'glyWhite'),

  'HexA' = c('HexA', 'glyWhite', 'glyWhite'),
  'GlcA' = c('HexA', 'glyBlue', 'glyWhite'),
  'ManA' = c('HexA', 'glyGreen', 'glyWhite'),
  'GalA' = c('HexA', 'glyYellow', 'glyWhite'),
  'GulA' = c('HexA', 'glyOrange', 'glyWhite'),
  'AltA' = c('HexA', 'glyPink', 'glyWhite'),
  'AllA' = c('HexA', 'glyPurple', 'glyWhite'),
  'TalA' = c('HexA', 'glyLightBlue', 'glyWhite'),
  'IdoA' = c('HexA', 'glyBrown', 'glyWhite'),

  'dHex' = c('dHex', 'glyWhite'),
  'Qui' = c('dHex', 'glyBlue'),
  'Rha' = c('dHex', 'glyGreen'),
  '6dGul' = c('dHex', 'glyOrange'),
  '6dAlt' = c('dHex', 'glyPink'),
  '6dTal' = c('dHex', 'glyLightBlue'),
  'Fuc' = c('dHex', 'glyRed'),
  'FucUp' = c('dHexUp', 'glyRed'),

  'dHexNAc' = c('dHexNAc', 'glyWhite', 'glyWhite'),
  'QuiNAc' = c('dHexNAc', 'glyBlue', 'glyWhite'),
  'RhaNAc' = c('dHexNAc', 'glyGreen', 'glyWhite'),
  '6dAltNAc' = c('dHexNAc', 'glyPink', 'glyWhite'),
  '6dTalNAc' = c('dHexNAc', 'glyLightBlue', 'glyWhite'),
  'FucNAc' = c('dHexNAc', 'glyRed', 'glyWhite'),

  'Pen' = c('Pen', 'glyWhite'),
  'Ara' = c('Pen', 'glyGreen'),
  'Lyx' = c('Pne', 'glyYellow'),
  'Xyl' = c('Pen', 'glyOrange'),
  'Rib' = c('Pen', 'glyPink'),

  'dNon' = c('dNon', 'glyWhite'),
  'Kdn' = c('dNon', 'glyGreen'),
  'Neu5Ac' = c('dNon', 'glyPurple'),
  'Neu5Gc' = c('dNon', 'glyLightBlue'),
  'NeuAc' = c('dNon', 'glyWhite'),
  'NeuGc' = c('dNon', 'glyWhite'),
  'Neu' = c('dNon', 'glyBrown'),
  'Sia' = c('dNon', 'glyRed'),

  'ddNon' = c('ddNon', 'glyWhite'),
  'Pse' = c('ddNon', 'glyGreen'),
  'Leg' = c('ddNon', 'glyYellow'),
  'Aci' = c('ddNon', 'glyPink'),
  '4eLeg' = c('ddNon', 'glyLightBlue'),

  'UnKnown' = c('UnKnown', 'glyWhite'),
  'Bac' = c('UnKnown', 'glyBlue'),
  'LDmanHep' = c('UnKnown', 'glyGreen'),
  'Kdo' = c('UnKnown', 'glyYellow'),
  'Dha' = c('UnKnown', 'glyOrange'),
  'DDmanHep' = c('UnKnown', 'glyPink'),
  'MurNAc' = c('UnKnown', 'glyPurple'),
  'MurNGc' = c('UnKnown', 'glyLightBlue'),
  'Mur' = c('UnKnown', 'glyBrown'),

  'Assigned' = c('Assigned', 'glyWhite'),
  'Api' = c('Assigned', 'glyBlue'),
  'Fru' = c('Assigned', 'glyGreen'),
  'Tag' = c('Assigned', 'glyYellow'),
  'Sor' = c('Assigned', 'glyOrange'),
  'Psi' = c('Assigned', 'glyPink')
)

glycan_shape <- list(
  'Hex' = data.frame(
    x = cos(seq(0, 2 * pi, length.out = 50)), # The center is the Core of shape
    y = sin(seq(0, 2 * pi, length.out = 50))
  ),
  'HexNAc' = data.frame(x = c(-1, -1, 1, 1, -1), y = c(-1, 1, 1, -1, -1)),
  'HexN' = data.frame(
    x = c(-1, 1, 1, -1),
    y = c(1, 1, -1, 1),
    xx = c(-1, 1, -1, -1),
    yy = c(1, -1, -1, 1)
  ),
  'HexA' = data.frame(
    x = c(-1, 0, 1, -1),
    y = c(0, 1, 0, 0),
    xx = c(1, 0, -1, 1),
    yy = c(0, -1, 0, 0)
  ),
  'dHex' = data.frame(
    x = c(-1, 0, 1, -1),
    y = c(-0.33 * sqrt(3), 0.67 * sqrt(3), -0.33 * sqrt(3), -0.33 * sqrt(3))
  ), # The center is the Midpoint of the Base
  'dHexUp' = data.frame(
    x = c(-1, 0, 1, -1),
    y = c(0.33 * sqrt(3), -0.67 * sqrt(3), 0.33 * sqrt(3), 0.33 * sqrt(3))
  ),
  'dHexNAc' = data.frame(
    x = c(0, 1, 0, 0),
    y = c(0.67 * sqrt(3), -0.33 * sqrt(3), -0.33 * sqrt(3), 0.67 * sqrt(3)),
    xx = c(0, -1, 0, 0),
    yy = c(0.67 * sqrt(3), -0.33 * sqrt(3), -0.33 * sqrt(3), 0.67 * sqrt(3))
  ),
  'ddHex' = data.frame(x = c(-1, 1, 1, -1), y = c(0.5, 0.5, -0.5, -0.5)),
  'Pen' = data.frame(
    x = c(
      0,
      0.2345,
      0.9516,
      0.3798,
      0.5872,
      0,
      -0.5872,
      -0.3798,
      -0.9516,
      -0.2345,
      0
    ),
    y = c(
      0.905,
      0.2289,
      0.2132,
      -0.219,
      -0.905,
      -0.4961,
      -0.905,
      -0.219,
      0.2132,
      0.2289,
      0.905
    )
  ),
  'dNon' = data.frame(x = c(0, 1, 0, -1, 0), y = c(1, 0, -1, 0, 1)),
  'ddNon' = data.frame(x = c(0, 1.2, 0, -1.2, 0), y = c(0.8, 0, -0.8, 0, 0.8)),
  'UnKnown' = data.frame(
    x = c(0.6, 1, 0.6, -0.6, -1, -0.6, 0.6),
    y = c(0.6, 0, -0.6, -0.6, 0, 0.6, 0.6)
  ),
  'Assigned' = data.frame(
    x = c(0, 0.9516, 0.5872, -0.5872, -0.9516, 0),
    y = c(0.905, 0.2132, -0.905, -0.905, 0.2132, 0.905)
  )
)

#' Title Initialize Vertices Coordinate
#'
#' @param structure an igraph object
#'
#' @returns Initialized Coordinate which Y=0, X are correct
#' @noRd
coor_initialization <- function(structure) {
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
      coor <- offset_chil_axis(structure, i, coor, "x", 1)
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

#' Title Find Sequence Number of Sub-module
#'
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns the child vertices of structure
#'
#' @examples chil_coor(structure, 3)
#' @noRd
chil_coor <- function(structure, ver) {
  vers <- igraph::bfs(structure, ver, mode = 'out', unreachable = FALSE)$order
  return(vers)
}

#' Title Offset y-Coordinate of Vertex Sub-module
#'
#' @param structure an igraph object
#' @param ver an integer
#' @param coor a matrix
#' @param offset a float
#'
#' @returns offset coordinate
#'
#' @examples offset_chil_coor(structure, 3, coor, 0.5)
#' @noRd
offset_chil_coor <- function(structure, ver, coor, offset) {
  offset_chil_axis(structure, ver, coor, "y", offset)
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
offset_chil_axis <- function(structure, ver, coor, axis, offset) {
  vers <- chil_coor(structure, ver)
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
orient_fucose_subtree <- function(structure, fuc_pos, coor, branch_offset) {
  direction <- sign(branch_offset)
  if (!is.finite(direction) || direction == 0) {
    return(coor)
  }

  descendants <- setdiff(as.integer(chil_coor(structure, fuc_pos)), fuc_pos)
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

#' Title Process the y-Coordinate of 'Fucose' Vertices
#'
#' @param structure an igraph object
#' @param fuc_pos An integer vector of 'Fucose' vertex indices.
#'
#' @returns A numeric vector of vertical offsets for the specified vertices.
#'
#' @examples fuc_offset(structure, c(1, 2))
#' @noRd
fuc_offset <- function(structure, fuc_pos) {
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

#' Title Process the Vertices which Out-degree =2 along the Path
#'
#' @param coor a matrix
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns Processed coordinate
#'
#' @examples process_two_neighbors(coor, structure, 5)
#' @noRd
process_two_neighbors <- function(coor, structure, ver) {
  arrange_neigh_pos <- .neigh_pos_order(structure, ver)
  coor <- offset_chil_coor(structure, arrange_neigh_pos[1], coor, 0.5)
  coor <- offset_chil_coor(structure, arrange_neigh_pos[2], coor, -0.5)
  return(coor)
}

#' Title Process the Vertices which Out-degree =3 along the Path
#'
#' @param coor a matrix
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns Processed coordinate
#'
#' @examples process_three_neighbors(coor, structure, 6)
#' @noRd
process_three_neighbors <- function(coor, structure, ver) {
  arrange_neigh_pos <- .neigh_pos_order(structure, ver)
  coor <- offset_chil_coor(structure, arrange_neigh_pos[1], coor, 1)
  coor <- offset_chil_coor(structure, arrange_neigh_pos[3], coor, -1)
  return(coor)
}

#' Title Process the Vertices adjacent to 'Fucose'
#'
#' @param coor a matrix
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns Processed coordinate
#'
#' @examples process_contain_fucose_neighbors(coor, structure, 7)
#' @noRd
process_contain_fucose_neighbors <- function(coor, structure, ver) {
  arrange_other_neigh_pos <- .neigh_pos_order(structure, ver)
  coor <- offset_chil_coor(structure, arrange_other_neigh_pos[1], coor, 0.5)
  coor <- offset_chil_coor(structure, arrange_other_neigh_pos[2], coor, -0.5)
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
profile_gap <- function(lower_profile, upper_profile, min_gap = 1) {
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
shift_compact_layout <- function(layout, offset) {
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
pack_child_layouts <- function(layouts, min_gap = 1) {
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
          profile_gap(
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
child_compaction_direction <- function(
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
enforce_parent_profile_gap <- function(
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
    shifted <- purrr::map2(layouts, shifts, shift_compact_layout)
    changed <- FALSE

    for (i in seq_len(layout_num)) {
      profile <- shifted[[i]]$profile
      parent_column_y <- profile$y[profile$x == parent_x]
      if (length(parent_column_y) == 0) {
        next
      }

      direction <- child_compaction_direction(
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
compact_subtree_layout <- function(structure, ver, coor, min_gap = 1) {
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
    \(child) compact_subtree_layout(structure, child, coor, min_gap)
  )
  shifts <- pack_child_layouts(layouts, min_gap)
  shifts <- enforce_parent_profile_gap(
    layouts,
    shifts,
    rough_offsets,
    parent_x = as.numeric(coor[ver, "x"]),
    min_gap = min_gap
  )
  shifted_layouts <- purrr::map2(layouts, shifts, shift_compact_layout)

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
compact_coor <- function(structure, coor, min_gap = 1) {
  layout <- compact_subtree_layout(
    structure,
    length(structure),
    coor,
    min_gap = min_gap
  )
  vertex_pos <- as.integer(names(layout$y))
  coor[vertex_pos, "y"] <- as.numeric(layout$y)
  coor
}

#' Title Process the Coordinate of all Vertices
#'
#' @param structure an igraph object
#'
#' @returns Processed Coordinate
#'
#' @examples coor_cal(structure)
#' @noRd
coor_cal <- function(structure) {
  coor <- coor_initialization(structure)
  structure_length <- seq(length(structure), 1)
  for (i in structure_length) {
    gly_neighbors <- igraph::neighbors(structure, i)
    if (
      length(gly_neighbors) == 2 &&
        !('Fuc' %in% gly_neighbors$mono)
    ) {
      coor <- process_two_neighbors(coor, structure, i)
    }
    if (
      length(gly_neighbors) == 3 &&
        !('Fuc' %in% gly_neighbors$mono)
    ) {
      coor <- process_three_neighbors(coor, structure, i)
    }
    if (
      length(gly_neighbors) == 3 &&
        'Fuc' %in% gly_neighbors$mono
    ) {
      coor <- process_contain_fucose_neighbors(coor, structure, i)
    }
  }
  for (i in structure_length) {
    if ('Fuc' %in% igraph::neighbors(structure, i)$mono) {
      neigh_pos <- igraph::neighbors(structure, i)
      fuc_pos <- neigh_pos[which(neigh_pos$mono == 'Fuc')]
      coor <- purrr::reduce2(
        as.integer(fuc_pos),
        fuc_offset(structure, fuc_pos),
        function(coor_acc, fuc_vertex, offset) {
          coor_acc <- offset_chil_coor(structure, fuc_vertex, coor_acc, offset)
          orient_fucose_subtree(structure, fuc_vertex, coor_acc, offset)
        },
        .init = coor
      )
    }
  }
  coor <- compact_coor(structure, coor)
  return(coor)
}

#' Title Analysis the Connection Information of all Vertices
#'
#' @param structure an igraph object
#' @param coor a matrix
#'
#' @returns Connection Information
#'
#' @examples connect_info(structure, coor)
#' @noRd
connect_info <- function(structure, coor) {
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

#' Title Gain the glycoforms corresponding to the vertices
#'
#' @param structure an igraph object
#'
#' @returns list of glycoform
#'
#' @examples glycoform_info(structure)
#' @noRd
glycoform_info <- function(structure) {
  glycoform <- igraph::V(structure)$mono
  for (i in c(which(glycoform == 'Fuc'))) {
    if (!.is_reducing_end(structure, i) && fuc_offset(structure, i) > 0) {
      glycoform[i] <- 'FucUp'
    }
  }
  return(glycoform)
}

#' Title Adjust the coordinate of glycan annotation text
#'
#' @param chil_glyx a float
#' @param chil_glyy a float
#' @param par_glyx a float
#' @param par_glyy a float
#' @param chil_offset annotation distance from the child residue center
#' @param par_offset annotation distance from the parent residue center
#'
#' @returns coordinate list of annotations
#'
#' @examples annotation_coordinate(chil_glyx, chil_glyy, par_glyx, par_glyy)
#' @noRd
annotation_coordinate <- function(
  chil_glyx,
  chil_glyy,
  par_glyx,
  par_glyy,
  chil_offset = 0.4,
  par_offset = 0.4
) {
  chil_direction <- matrix(
    c(par_glyx - chil_glyx, par_glyy - chil_glyy),
    ncol = 1,
    byrow = FALSE
  )
  par_direction <- matrix(
    c(chil_glyx - par_glyx, chil_glyy - par_glyy),
    ncol = 1,
    byrow = FALSE
  )
  chil_location <- chil_offset *
    chil_direction /
    norm(chil_direction, type = '2')
  par_location <- par_offset * par_direction / norm(par_direction, type = '2')
  rotate_angle <- 1 / 10 * pi
  chil_rotate_matrix <- matrix(
    c(
      cos(rotate_angle),
      sin(rotate_angle),
      -sin(rotate_angle),
      cos(rotate_angle)
    ),
    ncol = 2,
    byrow = TRUE
  )
  par_rotate_matrix <- matrix(
    c(
      cos(rotate_angle),
      -sin(rotate_angle),
      sin(rotate_angle),
      cos(rotate_angle)
    ),
    ncol = 2,
    byrow = TRUE
  )
  chil_annot_loc <- chil_rotate_matrix %*% chil_location
  par_annot_loc <- par_rotate_matrix %*% par_location
  annot_loc <- list("chil" = chil_annot_loc, "par" = par_annot_loc)
  return(annot_loc)
}

#' Calculate linkage annotation distance from a residue center
#'
#' @param structure an igraph object
#' @param anchor_ver an integer vertex index for the annotated residue
#' @param anchor_x x coordinate of the annotated residue
#' @param anchor_y y coordinate of the annotated residue
#' @param other_x x coordinate of the linked residue
#' @param other_y y coordinate of the linked residue
#' @param role whether the annotation is on the child or parent residue
#'
#' @returns numeric annotation offset distance
#' @noRd
annotation_offset <- function(
  structure,
  anchor_ver,
  anchor_x,
  anchor_y,
  other_x,
  other_y,
  role = c("child", "parent")
) {
  role <- rlang::arg_match(role)
  base_offset <- 0.4
  diagonal_hexnac_offset <- 0.45
  mono <- igraph::V(structure)[[anchor_ver]]$mono
  glycoform <- glycan_dict[[mono]][[1]]
  needs_extra_offset <- if (role == "child") {
    other_x > anchor_x && anchor_y > other_y
  } else {
    other_x < anchor_x && other_y < anchor_y
  }

  if (identical(glycoform, "HexNAc") && needs_extra_offset) {
    return(diagonal_hexnac_offset)
  }

  base_offset
}

#' Title Map the glycan coordinate and annotation text
#'
#' @param structure an igraph object
#' @param coor a matrix
#'
#' @returns dataframe of glycan annotation and coordinate
#'
#' @examples gly_annotation(structure,coor)
#' @noRd
gly_annotation <- function(structure, coor) {
  structure_length <- length(structure)
  if (igraph::ecount(structure) == 0) {
    return(data.frame(
      vertice = character(0),
      annot = character(0),
      x = numeric(0),
      y = numeric(0),
      segment_start_x = numeric(0),
      segment_start_y = numeric(0),
      segment_end_x = numeric(0),
      segment_end_y = numeric(0)
    ))
  }
  struc_annot_coor <- data.frame(
    vertice = character(),
    annot = character(),
    x = numeric(),
    y = numeric(),
    segment_start_x = numeric(),
    segment_start_y = numeric(),
    segment_end_x = numeric(),
    segment_end_y = numeric(),
    stringsAsFactors = FALSE
  )
  for (ver in seq_len(structure_length - 1)) {
    par_ver <- dplyr::nth(
      as.vector(igraph::shortest_paths(
        structure,
        length(structure),
        ver
      )$vpath[[1]]),
      -2
    )
    # Read annotation information and relative position
    linkage_str <- igraph::E(structure)[ver]$linkage
    gly_annot_coor <- annotation_coordinate(
      coor[ver, 1],
      coor[ver, 2],
      coor[par_ver, 1],
      coor[par_ver, 2],
      chil_offset = annotation_offset(
        structure,
        ver,
        coor[ver, 1],
        coor[ver, 2],
        coor[par_ver, 1],
        coor[par_ver, 2],
        role = "child"
      ),
      par_offset = annotation_offset(
        structure,
        par_ver,
        coor[par_ver, 1],
        coor[par_ver, 2],
        coor[ver, 1],
        coor[ver, 2],
        role = "parent"
      )
    )
    # Calculate annotation coordinate >> c(annotate_information, x, y)
    chil_annotation <- c(ver, strsplit(linkage_str, '-')[[1]][1])
    chil_annotation <- c(
      chil_annotation,
      as.vector(gly_annot_coor$chil) + coor[ver, ],
      coor[ver, ],
      coor[par_ver, ]
    )
    par_annotation <- c(ver, strsplit(linkage_str, '-')[[1]][2])
    par_annotation <- c(
      par_annotation,
      as.vector(gly_annot_coor$par) + coor[par_ver, ],
      coor[ver, ],
      coor[par_ver, ]
    )
    # Bind to data.frame
    struc_annot_coor <- rbind(struc_annot_coor, chil_annotation)
    struc_annot_coor <- rbind(struc_annot_coor, par_annotation)
  }
  colnames(struc_annot_coor) <- c(
    'vertice',
    'annot',
    'x',
    'y',
    'segment_start_x',
    'segment_start_y',
    'segment_end_x',
    'segment_end_y'
  )
  struc_annot_coor$annot[
    tolower(substr(struc_annot_coor$annot, 1, 1)) == "b"
  ] <- "beta"
  struc_annot_coor$annot[
    tolower(substr(struc_annot_coor$annot, 1, 1)) == "a"
  ] <- "alpha"
  struc_annot_coor$x <- as.numeric(struc_annot_coor$x)
  struc_annot_coor$y <- as.numeric(struc_annot_coor$y)
  struc_annot_coor$segment_start_x <- as.numeric(
    struc_annot_coor$segment_start_x
  )
  struc_annot_coor$segment_start_y <- as.numeric(
    struc_annot_coor$segment_start_y
  )
  struc_annot_coor$segment_end_x <- as.numeric(struc_annot_coor$segment_end_x)
  struc_annot_coor$segment_end_y <- as.numeric(struc_annot_coor$segment_end_y)
  return(struc_annot_coor)
}

#' Reflect a point across a line segment
#'
#' @param point a numeric vector with x and y values
#' @param segment_start a numeric vector with x and y values
#' @param segment_end a numeric vector with x and y values
#'
#' @returns reflected point coordinates
#' @noRd
reflect_point_across_segment <- function(point, segment_start, segment_end) {
  segment <- segment_end - segment_start
  segment_length <- sum(segment^2)
  if (!is.finite(segment_length) || segment_length <= .Machine$double.eps) {
    return(point)
  }

  projection <- segment_start +
    segment * sum((point - segment_start) * segment) / segment_length
  point + 2 * (projection - point)
}

#' Find the shortest distance among annotation coordinates
#'
#' @param coords a numeric matrix with x and y columns
#'
#' @returns minimum pairwise distance
#' @noRd
min_annotation_distance <- function(coords) {
  finite <- stats::complete.cases(coords)
  if (sum(finite) < 2) {
    return(Inf)
  }

  min(stats::dist(coords[finite, , drop = FALSE]))
}

#' Check whether two annotation coordinates are sufficiently separated
#'
#' @param coords a numeric matrix with x and y columns
#' @param i row index of first annotation
#' @param j row index of second annotation
#' @param min_distance minimum distance between annotation centers
#'
#' @returns logical scalar
#' @noRd
are_annotations_separated <- function(coords, i, j, min_distance) {
  delta <- coords[i, ] - coords[j, ]
  distance <- sqrt(sum(delta^2))
  distance >= min_distance
}

#' Compute the reflection of annotation coordinates across their segments
#'
#' @param coords a numeric matrix with x and y columns
#' @param segment a numeric matrix with segment start/end columns
#'
#' @returns numeric matrix of reflected coordinates
#' @noRd
reflect_annotation_coords <- function(coords, segment) {
  can_reflect <- stats::complete.cases(segment)
  reflected_coords <- coords
  for (i in which(can_reflect)) {
    reflected_coords[i, ] <- reflect_point_across_segment(
      point = coords[i, ],
      segment_start = segment[i, c("segment_start_x", "segment_start_y")],
      segment_end = segment[i, c("segment_end_x", "segment_end_y")]
    )
  }
  reflected_coords
}

#' Build annotation groups that share a reflection segment
#'
#' @param annotation a data frame returned by annotation mapping helpers
#' @param segment a numeric matrix with segment start/end columns
#' @param segment_lengths numeric vector of segment lengths
#'
#' @returns list with keys, rows, and lengths for each group
#' @noRd
build_annotation_groups <- function(annotation, segment, segment_lengths) {
  can_reflect <- stats::complete.cases(segment)
  group_keys <- rep(NA_character_, nrow(annotation))
  group_keys[can_reflect] <- paste(
    annotation$vertice[can_reflect],
    segment[can_reflect, "segment_start_x"],
    segment[can_reflect, "segment_start_y"],
    segment[can_reflect, "segment_end_x"],
    segment[can_reflect, "segment_end_y"],
    sep = "\r"
  )
  group_rows <- split(which(can_reflect), group_keys[can_reflect])
  group_lengths <- purrr::map_dbl(group_rows, ~ max(segment_lengths[.x]))

  list(
    keys = group_keys,
    rows = group_rows,
    lengths = group_lengths
  )
}

#' Choose the best annotation group to reflect for resolving an overlap
#'
#' @param i row index of first annotation
#' @param j row index of second annotation
#' @param coords current annotation coordinates
#' @param reflected_coords candidate reflected coordinates
#' @param groups list returned by build_annotation_groups
#' @param reflected_groups logical vector tracking reflected groups
#'
#' @returns character group key, or NULL if no candidate exists
#' @noRd
choose_reflection_group <- function(
  i,
  j,
  coords,
  reflected_coords,
  groups,
  reflected_groups
) {
  candidate_groups <- unique(groups$keys[c(i, j)])
  candidate_groups <- candidate_groups[
    !is.na(candidate_groups) & !reflected_groups[candidate_groups]
  ]
  if (length(candidate_groups) == 0) {
    return(NULL)
  }

  candidate_scores <- purrr::map_dbl(
    candidate_groups,
    function(candidate) {
      candidate_coords <- coords
      rows <- groups$rows[[candidate]]
      candidate_coords[rows, ] <- reflected_coords[rows, ]
      min_annotation_distance(candidate_coords)
    }
  )

  best_score <- max(candidate_scores)
  best_candidates <- candidate_groups[
    abs(candidate_scores - best_score) <= sqrt(.Machine$double.eps)
  ]
  best_candidates[which.max(groups$lengths[best_candidates])]
}

#' Iteratively resolve overlaps by reflecting annotation groups
#'
#' @param coords current annotation coordinates
#' @param reflected_coords candidate reflected coordinates
#' @param finite_index integer vector of rows with finite coordinates
#' @param groups list returned by build_annotation_groups
#' @param min_distance minimum distance between annotation centers
#' @param max_iter maximum number of overlap resolution passes
#'
#' @returns numeric matrix of resolved coordinates
#' @noRd
resolve_overlaps <- function(
  coords,
  reflected_coords,
  finite_index,
  groups,
  min_distance,
  max_iter
) {
  reflected_groups <- rep(FALSE, length(groups$rows))
  names(reflected_groups) <- names(groups$rows)

  for (iter in seq_len(max_iter)) {
    shifted <- FALSE
    for (i_pos in seq_len(length(finite_index) - 1)) {
      i <- finite_index[i_pos]
      for (j in finite_index[(i_pos + 1):length(finite_index)]) {
        if (are_annotations_separated(coords, i, j, min_distance)) {
          next
        }

        best <- choose_reflection_group(
          i,
          j,
          coords,
          reflected_coords,
          groups,
          reflected_groups
        )
        if (is.null(best)) {
          next
        }

        rows <- groups$rows[[best]]
        coords[rows, ] <- reflected_coords[rows, ]
        reflected_groups[best] <- TRUE
        shifted <- TRUE
      }
    }

    if (!shifted) {
      break
    }
  }

  coords
}

#' Separate annotations that are too close to each other
#'
#' @param annotation a data frame returned by annotation mapping helpers
#' @param min_distance minimum distance between annotation centers
#' @param max_iter maximum number of overlap resolution passes
#'
#' @returns annotation data frame with adjusted coordinates
#' @noRd
resolve_annotation_overlap <- function(
  annotation,
  min_distance = 0.2,
  max_iter = 20
) {
  if (nrow(annotation) < 2) {
    return(annotation)
  }

  coords <- as.matrix(annotation[, c("x", "y")])
  finite <- is.finite(coords[, "x"]) & is.finite(coords[, "y"])
  finite_index <- which(finite)
  if (length(finite_index) < 2) {
    return(annotation)
  }

  segment_cols <- c(
    "segment_start_x",
    "segment_start_y",
    "segment_end_x",
    "segment_end_y"
  )
  if (!all(segment_cols %in% names(annotation))) {
    annotation$x <- as.numeric(coords[, "x"])
    annotation$y <- as.numeric(coords[, "y"])
    return(annotation)
  }

  segment <- as.matrix(annotation[, segment_cols])
  segment_lengths <- sqrt(
    (segment[, "segment_end_x"] - segment[, "segment_start_x"])^2 +
      (segment[, "segment_end_y"] - segment[, "segment_start_y"])^2
  )

  reflected_coords <- reflect_annotation_coords(coords, segment)
  groups <- build_annotation_groups(annotation, segment, segment_lengths)
  coords <- resolve_overlaps(
    coords,
    reflected_coords,
    finite_index,
    groups,
    min_distance,
    max_iter
  )

  annotation$x <- as.numeric(coords[, "x"])
  annotation$y <- as.numeric(coords[, "y"])
  annotation
}

#' Map the coordinate of substituent annotation text
#'
#' @param structure an igraph object
#' @param coor a matrix
#' @param orient glycan drawing orientation
#'
#' @returns dataframe of substituent annotation and coordinate
#' @noRd
substituent_annotation <- function(structure, coor, orient) {
  sub <- igraph::V(structure)$sub
  if (length(sub) == 0) {
    return(data.frame(
      vertice = character(0),
      annot = character(0),
      x = numeric(0),
      y = numeric(0)
    ))
  }

  sub[is.na(sub)] <- ""
  sub_pos <- which(sub != "")
  if (length(sub_pos) == 0) {
    return(data.frame(
      vertice = character(0),
      annot = character(0),
      x = numeric(0),
      y = numeric(0)
    ))
  }

  offset <- if (orient == "H") {
    c(x = 0, y = 0.4)
  } else {
    c(x = 0.4, y = 0)
  }

  data.frame(
    vertice = as.character(sub_pos),
    annot = sub("^\\?+", "", sub[sub_pos]),
    x = as.numeric(coor[sub_pos, "x"] + offset["x"]),
    y = as.numeric(coor[sub_pos, "y"] + offset["y"]),
    stringsAsFactors = FALSE
  )
}

#' Check whether an annotation can be parsed as plotmath
#'
#' @param annot annotation text
#'
#' @returns a logical vector
#' @noRd
is_parseable_annotation <- function(annot) {
  purrr::map_lgl(annot, function(x) {
    !inherits(try(parse(text = x), silent = TRUE), "try-error")
  })
}

#' Quote annotation text for plotmath parsing
#'
#' @param annot annotation text
#'
#' @returns a quoted character vector
#' @noRd
quote_annotation <- function(annot) {
  paste0('"', gsub('"', '\\"', annot, fixed = TRUE), '"')
}

#' Title Map the coordinate of reducing end annotation and segment
#'
#' @param structure an igraph object
#' @param coor a matrix
#' @param orient glycan drawing orientation, "H" or "V"
#' @param red_end reducing-end annotation
#'
#' @returns list of reducing end annotation and segment
#'
#' @examples reducing_end_annotation(structure, coor, orient, red_end)
#' @noRd
reducing_end_annotation <- function(
  structure,
  coor,
  orient = c("H", "V"),
  red_end = ""
) {
  orient <- rlang::arg_match(orient)
  anomer <- igraph::graph_attr(structure, "anomer")
  if (length(anomer) == 0 || is.na(anomer) || anomer == "") {
    return(list(
      annotation = data.frame(
        vertice = character(0),
        annot = character(0),
        x = numeric(0),
        y = numeric(0)
      ),
      segment = data.frame(
        start_x = numeric(0),
        start_y = numeric(0),
        end_x = numeric(0),
        end_y = numeric(0)
      ),
      wave = data.frame(
        x = numeric(0),
        y = numeric(0)
      )
    ))
  }
  label <- tolower(substr(anomer, 1, 1))
  if (label == "a") {
    label <- "alpha"
  } else if (label == "b") {
    label <- "beta"
  } else {
    label <- '~"?"'
  }
  root <- length(structure)
  root_coor <- c(x = as.numeric(coor[root, 1]), y = as.numeric(coor[root, 2]))
  line_length <- 0.6
  label_offset <- 0.2
  line_vec <- if (orient == "H") {
    c(x = line_length, y = 0)
  } else {
    c(x = 0, y = -line_length)
  }
  line_end <- root_coor + line_vec
  rotate_angle <- 1 / 10 * pi
  rotate_matrix <- matrix(
    c(
      cos(rotate_angle),
      sin(rotate_angle),
      -sin(rotate_angle),
      cos(rotate_angle)
    ),
    ncol = 2,
    byrow = TRUE
  )
  label_vec <- if (orient == "H") {
    c(x = line_length + label_offset, y = 0)
  } else {
    c(x = 0, y = -(line_length + label_offset))
  }
  annot_loc <- 0.6 * rotate_matrix %*% matrix(label_vec, ncol = 1)
  annot_coor <- root_coor + as.vector(annot_loc)
  red_end_annotation <- reducing_end_text_annotation(
    red_end,
    line_end,
    line_vec,
    root
  )
  list(
    annotation = dplyr::bind_rows(
      data.frame(
        vertice = as.character(root),
        annot = label,
        x = as.numeric(annot_coor["x"]),
        y = as.numeric(annot_coor["y"])
      ),
      red_end_annotation
    ),
    segment = data.frame(
      start_x = as.numeric(root_coor["x"]),
      start_y = as.numeric(root_coor["y"]),
      end_x = as.numeric(line_end["x"]),
      end_y = as.numeric(line_end["y"])
    ),
    wave = reducing_end_wave(red_end, line_end, line_vec)
  )
}

#' Map reducing-end text coordinates
#'
#' @param red_end reducing-end annotation
#' @param line_end reducing-end line endpoint
#' @param line_vec reducing-end line vector
#' @param root reducing-end vertex index
#'
#' @returns a data frame
#' @noRd
reducing_end_text_annotation <- function(red_end, line_end, line_vec, root) {
  if (red_end %in% c("", "~")) {
    return(data.frame(
      vertice = character(0),
      annot = character(0),
      x = numeric(0),
      y = numeric(0)
    ))
  }
  line_unit <- line_vec / sqrt(sum(line_vec^2))
  text_offset <- 0.1
  text_coor <- line_end + line_unit * text_offset
  data.frame(
    vertice = as.character(root),
    annot = quote_annotation(red_end),
    x = as.numeric(text_coor["x"]),
    y = as.numeric(text_coor["y"])
  )
}

#' Map reducing-end wave coordinates
#'
#' @param red_end reducing-end annotation
#' @param line_end reducing-end line endpoint
#' @param line_vec reducing-end line vector
#'
#' @returns a data frame
#' @noRd
reducing_end_wave <- function(red_end, line_end, line_vec) {
  if (!identical(red_end, "~")) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  line_unit <- line_vec / sqrt(sum(line_vec^2))
  wave_unit <- c(x = -line_unit["y"], y = line_unit["x"])
  wave_t <- seq(0, 1, length.out = 25)
  wave_length <- 0.45
  wave_amplitude <- 0.03
  wave_coor <- purrr::map_dfr(wave_t, function(t) {
    line_end +
      wave_unit * ((t - 0.5) * wave_length) +
      line_unit * (sin(2 * pi * t) * wave_amplitude)
  })
  data.frame(
    x = as.numeric(wave_coor$x),
    y = as.numeric(wave_coor$y)
  )
}

#' Title Match the coordinates of glycan shape
#'
#' @param gly_list a list
#'
#' @returns the coordinate list of glycan shape
#'
#' @examples create_polygon_coor(gly_list)
#' @noRd
create_polygon_coor <- function(gly_list, point_size) {
  # Progressively read and process lines in gly_list
  polygon_coor <- gly_list |>
    purrr::pmap_dfr(function(center_x, center_y, glycoform, transparency) {
      composition <- glycan_dict[[glycoform]][1] # Mapping the Composition of Glycoform, e.g.'Fuc'->'dHex'
      df1 <- data.frame(
        point_x = c(point_size * glycan_shape[[composition]]$x + center_x),
        point_y = c(point_size * glycan_shape[[composition]]$y + center_y),
        # For Distinguishing the Coordinates of each point
        group = paste0(glycoform, center_x, "_", center_y),
        color = glycan_dict[[glycoform]][2],
        alpha = transparency
      )
      if (length(glycan_dict[[glycoform]]) > 2) {
        df2 <- data.frame(
          point_x = c(point_size * glycan_shape[[composition]]$xx + center_x),
          point_y = c(point_size * glycan_shape[[composition]]$yy + center_y),
          # For Distinguishing the Coordinates of each point
          group = paste0(glycoform, center_x, "_", center_y, 'remain'),
          color = glycan_dict[[glycoform]][3],
          alpha = transparency
        )
        df1 <- dplyr::bind_rows(df1, df2)
      }
      return(df1)
    })
  return(polygon_coor)
}

.decide_size <- function(cartoon, border_px = 0) {
  panel_width <- 3 *
    118 *
    diff(ggplot2::get_panel_scales(cartoon)$x$range$range)
  panel_height <- 3 *
    118 *
    diff(ggplot2::get_panel_scales(cartoon)$y$range$range)
  width <- panel_width + 2 * border_px
  height <- panel_height + 2 * border_px
  return(list(width = width, height = height))
}

.apply_border <- function(plot, border_pt) {
  if (is.null(border_pt) || border_pt <= 0) {
    return(plot)
  }
  plot +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        t = border_pt,
        r = border_pt,
        b = border_pt,
        l = border_pt,
        unit = "pt"
      )
    )
}

#' Apply fixed ggplot2 panel size
#'
#' @param plot A ggplot2 object.
#' @param panel_size_px A named list or vector with `width` and `height` panel
#'   size in pixels.
#' @param dpi Dots per inch used to convert pixels to physical panel units.
#'
#' @return A ggplot2 object with fixed panel width and height.
#' @noRd
.apply_fixed_panel_size <- function(plot, panel_size_px, dpi) {
  plot +
    ggplot2::theme(
      panel.widths = grid::unit(panel_size_px[["width"]] / dpi, "in"),
      panel.heights = grid::unit(panel_size_px[["height"]] / dpi, "in")
    )
}

#' Render a glycan cartoon to a fixed-size raster image
#'
#' @param cartoon A glydraw cartoon.
#' @param dpi Dots per inch used by the cartoon size metadata.
#' @param bg Background color passed to [ggplot2::ggsave()].
#'
#' @return A native raster matrix.
#' @noRd
.render_cartoon_raster <- function(cartoon, dpi = 300, bg = "transparent") {
  size <- attr(cartoon, "glydraw_size_px")
  checkmate::assert_numeric(size, names = "strict", any.missing = FALSE)
  checkmate::assert_names(names(size), identical.to = c("width", "height"))

  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)

  ggplot2::ggsave(
    filename = file,
    plot = .strip_glydraw_class(cartoon),
    width = size[["width"]],
    height = size[["height"]],
    units = "px",
    dpi = dpi,
    bg = bg
  )
  png::readPNG(file, native = TRUE)
}

#' Draw a fixed-size cartoon raster to the current graphics device
#'
#' @param raster A native raster matrix.
#' @param size_px A named numeric vector with `width` and `height` in pixels.
#' @param dpi Dots per inch used by the cartoon size metadata.
#' @param newpage Draw on a new grid page.
#' @param vp A grid viewport object or viewport name.
#'
#' @return The raster object, invisibly.
#' @noRd
.draw_cartoon_raster <- function(
  raster,
  size_px,
  dpi = 300,
  newpage = TRUE,
  vp = NULL
) {
  if (newpage) {
    grid::grid.newpage()
  }
  pushed_viewport <- FALSE
  if (!is.null(vp)) {
    if (is.character(vp)) {
      grid::seekViewport(vp)
    } else {
      grid::pushViewport(vp)
      pushed_viewport <- TRUE
    }
  }
  if (pushed_viewport) {
    on.exit(grid::upViewport(), add = TRUE)
  }

  viewport_size <- .current_viewport_size_in()
  cartoon_size <- size_px / dpi
  scale <- min(
    1,
    viewport_size[["width"]] / cartoon_size[["width"]],
    viewport_size[["height"]] / cartoon_size[["height"]]
  )
  if (!is.finite(scale) || scale <= 0) {
    scale <- 1
  }

  grid::grid.raster(
    raster,
    width = grid::unit(cartoon_size[["width"]] * scale, "in"),
    height = grid::unit(cartoon_size[["height"]] * scale, "in"),
    interpolate = TRUE
  )
  invisible(raster)
}

#' Get the current grid viewport size
#'
#' @return A named numeric vector with `width` and `height` in inches.
#' @noRd
.current_viewport_size_in <- function() {
  size <- c(
    width = grid::convertWidth(grid::unit(1, "npc"), "in", valueOnly = TRUE),
    height = grid::convertHeight(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  )
  if (any(!is.finite(size)) || any(size <= 0)) {
    device_size <- grDevices::dev.size("in")
    size <- c(width = device_size[[1]], height = device_size[[2]])
  }
  size
}

.ensure_highlight_para <- function(x, gly_vertices) {
  if (!is.numeric(x) && !is.null(x)) {
    cli::cli_abort("highlight parameter {.emph {x}} must be numeric")
  } else if (!all(x %in% seq(1, gly_vertices))) {
    cli::cli_abort(
      "highlight parameter {.emph {x}} was out of range {.emph 1~{gly_vertices}}"
    )
  }
  x
}

.strip_glydraw_class <- function(x) {
  class(x) <- setdiff(class(x), "glydraw_cartoon")
  x
}

.ensure_structure <- function(x) {
  if (glyrepr::is_glycan_structure(x)) {
    x <- x
  } else if (is.character(x)) {
    x <- glyparse::auto_parse(x)
  } else {
    cli::cli_abort(c(
      "{.arg structure} must be either a structure string or {.fn glyrepr::glycan_structure}.",
      "x" = "Got: {.cls {class(x)}}."
    ))
  }
  x
}

.ensure_one_structure <- function(x) {
  x <- .ensure_structure(x)
  if (length(x) > 1) {
    cli::cli_abort(c(
      "Must provide exactly one glycan structure.",
      "x" = "Get length: {.val {length(x)}}."
    ))
  }
  x
}
