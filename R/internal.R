# glycan mapping
glycan_color <- c(
  'glyWhite' = '#FFFFFF',
  'glyBlue' = '#0385AE',
  'glyGreen' = '#058F60',
  'glyYellow' = '#FCC326',
  'glyOrange' = '#EF6130',
  'glyPink' = '#F39EA0',
  'glyPurple' = '#A15989',
  'glyLightBlue' = '#91D3E3',
  'glyBrown' = '#9F6D55',
  'glyRed' = '#C23537'
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

#' Title Calculate the Amount and Sequence Number of Vertices that Out-degree >= 2 along the Path
#'
#' @param structure an igraph integer
#' @param ver the Sequence Number of Vertices
#'
#' @returns the number of vertices which neighbors >=2 on the path between vertex and begin vertex
#'
#' @examples out_degree(structure, 1)
#' @noRd
out_degree <- function(structure, ver) {
  path_vertex <- igraph::shortest_paths(
    structure,
    length(structure),
    ver
  )$vpath[[1]]
  num <- 0
  pos <- c()
  for (i in path_vertex) {
    if (
      length(igraph::neighbors(structure, i)) >= 2 &&
        !('Fuc' %in% igraph::neighbors(structure, i)$mono)
    ) {
      num <- num + 1
      pos <- c(pos, i)
    }
  }
  return(c(num, pos))
}

#' Title Judge whether the Vertex is in the Middle Position of Sub-module
#'
#' @param structure an igraph object
#' @param coor a matrix
#' @param ver an integer
#' @param par_ver an integer
#'
#' @returns bool
#'
#' @examples mid_pos(structure, coor, 1, 3)
#' @noRd
mid_pos <- function(structure, coor, ver, par_ver) {
  pos_mid <- FALSE
  vers <- chil_coor(structure, par_ver)
  col_vers <- vers[which(coor[vers, 'x'] == coor[ver, 'x'])]
  ver_y <- coor[ver, 'y']
  col_y <- sort(coor[col_vers, 'y'])
  if (ver_y != min(col_y) && ver_y != max(col_y)) {
    pos_mid <- TRUE
  }
  return(pos_mid)
}

#' Title Judge whether the 'Fucose' Vertex is in the Middle Position of Sub-module
#'
#' @param structure an igraph object
#' @param coor a matrix
#' @param ver an integer
#' @param par_ver an integer
#'
#' @returns bool
#'
#' @examples fuc_mid_pos(structure, coor, 1, 3)
#' @noRd
fuc_mid_pos <- function(structure, coor, ver, par_ver) {
  vers <- chil_coor(structure, par_ver)
  col_vers <- vers[which(coor[vers, 'x'] == coor[ver, 'x'])]
  ver_y <- coor[ver, 'y']
  col_y <- sort(coor[col_vers, 'y'])
  ver_index <- which(col_y == ver_y)
  lag_diff <- abs(col_y - dplyr::lag(col_y)) < 1
  lead_diff <- abs(col_y - dplyr::lead(col_y)) < 1
  result <- lag_diff & lead_diff
  result[is.na(result)] <- FALSE
  return(result[ver_index])
}

#' Title Calculate the Amount and Sequence Number of Vertices that Out-degree >= 2 from Specified 'Fucose' vertex along the Path
#'
#' @param structure an igraph object
#' @param ver an integer
#' @param coor a matrix
#'
#' @returns the Amount and Sequence Number of vertices that Out-degree >= 2
#'
#' @examples fuc_out_degree(structure, 6, coor)
#' @noRd
fuc_out_degree <- function(structure, ver, coor) {
  path_vertex <- igraph::shortest_paths(
    structure,
    length(structure),
    ver
  )$vpath[[1]]
  num <- 0
  pos <- c()
  for (i in path_vertex) {
    if (
      length(igraph::neighbors(structure, i)) %in%
        c(2, 3) &&
        !('Fuc' %in% igraph::neighbors(structure, i)$mono) &&
        fuc_mid_pos(structure, coor, ver, i)
    ) {
      num <- num + 1
      pos <- c(pos, i)
    }
  }
  return(c(num, pos))
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
  offset <- rep(0.99, length(linkage_pos))
  offset[linkage_pos %in% c('2', '3')] <- -0.99
  return(offset)
}

#' Check whether an ancestor branch needs extra spacing.
#'
#' @param structure an igraph object
#' @param ver the descendant branch vertex being processed
#' @param ancestor an ancestor branch vertex
#' @param neighbor_pos child vertices of `ancestor`
#'
#' @returns A logical scalar.
#' @noRd
has_deep_sibling_branch <- function(
  structure,
  ver,
  ancestor,
  neighbor_pos
) {
  path <- igraph::shortest_paths(
    structure,
    ancestor,
    ver,
    mode = "out"
  )$vpath[[1]]
  path <- as.integer(path)
  if (length(path) < 2) {
    return(TRUE)
  }

  path_child <- path[2]
  sibling_pos <- setdiff(as.integer(neighbor_pos), path_child)
  if (length(sibling_pos) == 0) {
    return(TRUE)
  }

  branch_child_pos <- as.integer(igraph::neighbors(
    structure,
    ver,
    mode = "out"
  ))
  if (length(branch_child_pos) == 0) {
    return(FALSE)
  }

  depth <- igraph::distances(structure)[length(structure), ]
  path_depth <- max(depth[branch_child_pos])
  sibling_depth <- purrr::map_dbl(
    sibling_pos,
    ~ max(depth[chil_coor(structure, .x)])
  )

  any(sibling_depth >= path_depth)
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

#' Title Process the Vertices which Out-degree >=2 along the Path
#'
#' @param coor a matrix
#' @param structure an igraph object
#' @param fuc_pos an integer
#' @param temp_coor a matrix
#'
#' @returns Processed Coordinate
#'
#' @examples process_fucose_branches(coor, structure, 2, temp_coor)
#' @noRd
process_fucose_branches <- function(coor, structure, fuc_pos, temp_coor) {
  num <- fuc_out_degree(structure, fuc_pos, temp_coor)[1]
  for (i in seq(1, num)) {
    par_pos <- fuc_out_degree(structure, fuc_pos, temp_coor)[i + 1]
    arrange_neigh_pos <- .neigh_pos_order(structure, par_pos)

    if (length(arrange_neigh_pos) == 2) {
      coor <- offset_chil_coor(
        structure,
        arrange_neigh_pos[1],
        coor,
        1 / (2**(num - i + 1))
      )
      coor <- offset_chil_coor(
        structure,
        arrange_neigh_pos[2],
        coor,
        -1 / (2**(num - i + 1))
      )
    } else if (length(arrange_neigh_pos) == 3) {
      coor <- offset_chil_coor(
        structure,
        arrange_neigh_pos[1],
        coor,
        1 / (2**(num - i + 1))
      )
      coor <- offset_chil_coor(
        structure,
        arrange_neigh_pos[3],
        coor,
        -1 / (2**(num - i + 1))
      )
    }
  }
  return(coor)
}

#' Title Process the Vertices which Out-degree >=2 and more than 2 Father Vertices that Out-degree >= 2 along the Path
#'
#' @param coor a matrix
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns Processed coordinate
#'
#' @examples process_multiple_branches(coor, structure, 3)
#' @noRd
process_multiple_branches <- function(coor, structure, ver) {
  num <- out_degree(structure, ver)[1] # Numbers of vertices which out-degree > 1 (e.g. branch vertices)
  for (j in seq(1, num - 1)) {
    # Traverse all branch vertices except for self
    pos <- out_degree(structure, ver)[j + 1]
    pos_max <- out_degree(structure, ver)[num] # Position of branch vertices with second largest index
    arrange_neigh_pos <- .neigh_pos_order(structure, pos)
    # Judge whether child vertex is in the middle
    ver_neigh_pos <- igraph::neighbors(structure, ver)
    num_neigh <- length(ver_neigh_pos)
    chil_in_middle <- mid_pos(structure, coor, min(ver_neigh_pos), pos) |
      mid_pos(structure, coor, max(ver_neigh_pos), pos)
    if (length(arrange_neigh_pos) == 2) {
      # Different number of ver's neighbor leads to different offset rule.
      # (3-num_neigh) means when the number of ver's neighbor=3, value=0; 2->1.
      if (has_deep_sibling_branch(structure, ver, pos, arrange_neigh_pos)) {
        coor <- offset_chil_coor(
          structure,
          arrange_neigh_pos[1],
          coor,
          1 /
            (2**(num - j + (3 - num_neigh))) +
            0.25 * mid_pos(structure, coor, ver, pos) * (pos != pos_max)
        )
        coor <- offset_chil_coor(
          structure,
          arrange_neigh_pos[2],
          coor,
          -1 /
            (2**(num - j + (3 - num_neigh))) -
            0.25 * mid_pos(structure, coor, ver, pos) * (pos != pos_max)
        )
      }
    } else if (length(arrange_neigh_pos) == 3 && chil_in_middle) {
      # For neighbors=3 vertex, the offset should not be consistent.
      # Only offset the neighbor vertex included in the path along the specified vertex to start vertex.
      path_vertex <- igraph::shortest_paths(
        structure,
        length(structure),
        ver
      )$vpath[[1]]
      offset_index <- which(arrange_neigh_pos %in% path_vertex) # Index of the neighbor to be offset.
      # (2-offset_index) means when offset_index=1, value=1; 2->0; 3->-1, adjusting offset direction.
      # (2-num_neigh) means when the number of ver's neighbor=3, value=-1; 2->0.
      coor <- offset_chil_coor(
        structure,
        arrange_neigh_pos[offset_index],
        coor,
        (2 - offset_index) *
          (1 /
            (2**(num - j + (2 - num_neigh))) +
            0.25 * mid_pos(structure, coor, ver, pos) * (pos != pos_max))
      )
    }
  }
  return(coor)
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
  if (out_degree(structure, ver)[1] == 1) {
    coor <- offset_chil_coor(structure, arrange_neigh_pos[1], coor, 0.5)
    coor <- offset_chil_coor(structure, arrange_neigh_pos[2], coor, -0.5)
  } else if (out_degree(structure, ver)[1] >= 2) {
    # More than 1 branch along the path
    coor <- offset_chil_coor(structure, arrange_neigh_pos[1], coor, 0.5)
    coor <- offset_chil_coor(structure, arrange_neigh_pos[2], coor, -0.5)
    coor <- process_multiple_branches(coor, structure, ver)
  }
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
  if (out_degree(structure, ver)[1] == 1) {
    coor <- offset_chil_coor(structure, arrange_neigh_pos[1], coor, 1)
    coor <- offset_chil_coor(structure, arrange_neigh_pos[3], coor, -1)
  } else if (out_degree(structure, ver)[1] >= 2) {
    # More than 1 branch along the path
    coor <- offset_chil_coor(structure, arrange_neigh_pos[1], coor, 1)
    coor <- offset_chil_coor(structure, arrange_neigh_pos[3], coor, -1)
    coor <- process_multiple_branches(coor, structure, ver)
  }
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
  # Process other vertices coordinate
  if (out_degree(structure, ver)[1] == 1) {
    coor <- offset_chil_coor(structure, arrange_other_neigh_pos[1], coor, 0.5)
    coor <- offset_chil_coor(structure, arrange_other_neigh_pos[2], coor, -0.5)
  } else if (out_degree(structure, ver)[1] >= 2) {
    # More than 1 branch along the path
    coor <- offset_chil_coor(structure, arrange_other_neigh_pos[1], coor, 0.5)
    coor <- offset_chil_coor(structure, arrange_other_neigh_pos[2], coor, -0.5)
    coor <- process_multiple_branches(coor, structure, ver)
  }
  return(coor)
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
  fuc_list <- c()
  for (i in structure_length) {
    if ('Fuc' %in% igraph::neighbors(structure, i)$mono) {
      neigh_pos <- igraph::neighbors(structure, i)
      fuc_pos <- neigh_pos[which(neigh_pos$mono == 'Fuc')]
      fuc_list <- c(fuc_list, fuc_pos)
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
  temp_coor <- coor
  for (i in fuc_list) {
    if (fuc_out_degree(structure, i, temp_coor)[1] >= 1) {
      coor <- process_fucose_branches(coor, structure, i, temp_coor)
    }
  }
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
    if (!.is_reducing_end(structure, i) && fuc_offset(structure, i) == '0.99') {
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
#'
#' @returns coordinate list of annotations
#'
#' @examples annotation_coordinate(chil_glyx, chil_glyy, par_glyx, par_glyy)
#' @noRd
annotation_coordinate <- function(chil_glyx, chil_glyy, par_glyx, par_glyy) {
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
  chil_location <- 0.4 * chil_direction / norm(chil_direction, type = '2')
  par_location <- 0.4 * par_direction / norm(par_direction, type = '2')
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
      coor[par_ver, 2]
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
#'
#' @returns list of reducing end annotation and segment
#'
#' @examples reducing_end_annotation(structure, coor, orient)
#' @noRd
reducing_end_annotation <- function(structure, coor, orient = c("H", "V")) {
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
  list(
    annotation = data.frame(
      vertice = as.character(root),
      annot = label,
      x = as.numeric(annot_coor["x"]),
      y = as.numeric(annot_coor["y"])
    ),
    segment = data.frame(
      start_x = as.numeric(root_coor["x"]),
      start_y = as.numeric(root_coor["y"]),
      end_x = as.numeric(line_end["x"]),
      end_y = as.numeric(line_end["y"])
    )
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
