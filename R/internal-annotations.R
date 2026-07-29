# Internal helpers for deriving text annotations, label positions, overlap
# resolution, substituent labels, and reducing-end annotation geometry.

#' Calculate label positions for one glycosidic linkage
#'
#' @param chil_glyx,chil_glyy Numeric `x` and `y` coordinates of the child
#'   residue.
#' @param par_glyx,par_glyy Numeric `x` and `y` coordinates of the parent
#'   residue.
#' @param chil_offset Numeric distance from the child residue center to the
#'   child-side linkage label.
#' @param par_offset Numeric distance from the parent residue center to the
#'   parent-side linkage label.
#' @param chil_perpendicular_nudge Numeric distance to move the child-side
#'   label perpendicular to and away from the linkage segment.
#' @param node_size Numeric node-size multiplier used to push labels along the
#'   linkage segment away from scaled residue polygons.
#'
#' @returns A list with two numeric 2-row matrices, `chil` and `par`. Each
#'   matrix is an `(x, y)` offset vector to add to the child or parent residue
#'   coordinate.
#' @noRd
.linkage_label_positions <- function(
  chil_glyx,
  chil_glyy,
  par_glyx,
  par_glyy,
  chil_offset = 0.4,
  par_offset = 0.4,
  chil_perpendicular_nudge = 0,
  node_size = 1
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
  chil_annot_loc <- .nudge_child_label_perpendicular(
    label_offset = chil_annot_loc,
    direction = chil_direction,
    nudge = chil_perpendicular_nudge
  )
  extra_offset <- .linkage_annotation_extra_offset(node_size)
  chil_annot_loc <- .push_label_position_along_segment(
    label_offset = chil_annot_loc,
    anchor = c(x = chil_glyx, y = chil_glyy),
    other = c(x = par_glyx, y = par_glyy),
    extra_offset = extra_offset
  )
  par_annot_loc <- .push_label_position_along_segment(
    label_offset = par_annot_loc,
    anchor = c(x = par_glyx, y = par_glyy),
    other = c(x = chil_glyx, y = chil_glyy),
    extra_offset = extra_offset
  )
  annot_loc <- list("chil" = chil_annot_loc, "par" = par_annot_loc)
  return(annot_loc)
}

.beta_annotation_perpendicular_nudge <- 0.025

#' Resolve the perpendicular nudge for a beta linkage label
#'
#' @param label A child-side linkage label.
#' @param child_x,parent_x Numeric horizontal coordinates of the child and
#'   parent residues.
#'
#' @returns The beta nudge for a non-vertical linkage, otherwise zero.
#' @noRd
.beta_perpendicular_nudge_for_linkage <- function(
  label,
  child_x,
  parent_x
) {
  is_beta <- .normalize_linkage_labels(label) == "beta"
  is_vertical <- isTRUE(all.equal(child_x, parent_x))
  if (is_beta && !is_vertical) {
    return(.beta_annotation_perpendicular_nudge)
  }
  0
}

#' Nudge a child-side label perpendicular to its linkage segment
#'
#' @param label_offset A two-row matrix giving the current label offset from
#'   the child residue.
#' @param direction A two-row matrix pointing from the child residue to the
#'   parent residue.
#' @param nudge Numeric perpendicular distance to add.
#'
#' @returns A two-row matrix with the adjusted label offset.
#' @noRd
.nudge_child_label_perpendicular <- function(
  label_offset,
  direction,
  nudge
) {
  direction_norm <- norm(direction, type = "2")
  if (nudge <= 0 || direction_norm <= .Machine$double.eps) {
    return(label_offset)
  }

  clockwise_normal <- matrix(c(direction[[2]], -direction[[1]]), ncol = 1)
  label_offset + nudge * clockwise_normal / direction_norm
}

#' Calculate the extra annotation clearance for scaled nodes
#'
#' @param node_size Numeric node-size multiplier.
#'
#' @returns A non-negative numeric scalar.
#' @noRd
.annotation_extra_offset <- function(node_size) {
  .default_node_point_size * pmax(node_size - 1, 0)
}

#' Calculate the inward shift for linkage labels beside scaled nodes
#'
#' The extra node radius reduces three clearances along a linkage: the two
#' node-to-label gaps and the gap between the labels. Shifting each label by
#' one-third of the extra radius distributes that reduction evenly instead of
#' concentrating it between the labels.
#'
#' @param node_size Numeric node-size multiplier.
#'
#' @returns A non-negative numeric scalar.
#' @noRd
.linkage_annotation_extra_offset <- function(node_size) {
  .annotation_extra_offset(node_size) / 3
}

#' Push a linkage label offset along its linkage segment
#'
#' @param label_offset A two-row matrix giving the current label offset from
#'   the anchor residue.
#' @param anchor Numeric `x` and `y` coordinates of the anchor residue.
#' @param other Numeric `x` and `y` coordinates of the linked residue.
#' @param extra_offset Numeric distance to add along the segment.
#'
#' @returns A two-row matrix with the adjusted label offset.
#' @noRd
.push_label_position_along_segment <- function(
  label_offset,
  anchor,
  other,
  extra_offset
) {
  if (extra_offset <= 0) {
    return(label_offset)
  }

  direction <- matrix(other - anchor, ncol = 1)
  direction_norm <- norm(direction, type = "2")
  if (direction_norm <= .Machine$double.eps) {
    return(label_offset)
  }

  label_offset + extra_offset * direction / direction_norm
}

#' Choose the label offset for one side of a linkage
#'
#' @param structure An igraph glycan graph whose vertices include `mono`.
#' @param anchor_ver A single integer vertex index for the residue receiving
#'   the label.
#' @param anchor_x,anchor_y Numeric coordinates of the residue receiving the
#'   label.
#' @param other_x,other_y Numeric coordinates of the residue on the other side
#'   of the linkage.
#' @param role Either `"child"` or `"parent"`, naming which side of the linkage
#'   the label belongs to.
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#'
#' @returns A numeric scalar offset distance from the anchor residue center.
#' @noRd
.linkage_label_offset <- function(
  structure,
  anchor_ver,
  anchor_x,
  anchor_y,
  other_x,
  other_y,
  role = c("child", "parent"),
  orient = c("left", "right", "up", "down")
) {
  role <- rlang::arg_match(role)
  orient <- rlang::arg_match(orient)
  base_offset <- 0.4
  diagonal_hexnac_offset <- 0.45
  mono <- igraph::V(structure)[[anchor_ver]]$mono
  glycoform <- glycan_dict[[mono]][[1]]
  needs_extra_offset <- .needs_diagonal_hexnac_offset(
    anchor_x,
    anchor_y,
    other_x,
    other_y,
    role,
    orient
  )

  if (identical(glycoform, "HexNAc") && needs_extra_offset) {
    return(diagonal_hexnac_offset)
  }

  base_offset
}

#' Check whether a diagonal HexNAc label needs extra clearance
#'
#' @param anchor_x,anchor_y Numeric coordinates of the residue receiving the
#'   label.
#' @param other_x,other_y Numeric coordinates of the residue on the other side
#'   of the linkage.
#' @param role Either `"child"` or `"parent"`, naming which side of the linkage
#'   the label belongs to.
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#'
#' @returns A logical scalar.
#' @noRd
.needs_diagonal_hexnac_offset <- function(
  anchor_x,
  anchor_y,
  other_x,
  other_y,
  role,
  orient
) {
  coor <- matrix(
    c(anchor_x, anchor_y, other_x, other_y),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("anchor", "other"), c("x", "y"))
  ) |>
    .cartoon_coordinates_as_left(orient)
  anchor_x <- coor["anchor", "x"]
  anchor_y <- coor["anchor", "y"]
  other_x <- coor["other", "x"]
  other_y <- coor["other", "y"]

  if (role == "child") {
    return(other_x > anchor_x && other_y < anchor_y)
  }
  other_x < anchor_x && other_y < anchor_y
}

#' Build linkage annotation rows for every glycosidic edge
#'
#' @param structure An igraph glycan graph whose edges include `linkage`.
#' @param coor A numeric coordinate matrix with columns `x` and `y`, one row
#'   per graph vertex.
#' @param node_size Numeric node-size multiplier used to keep labels outside
#'   scaled residue polygons.
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#'
#' @returns A data frame with one or two rows per edge and columns `vertice`,
#'   `annot`, `x`, `y`, `segment_start_x`, `segment_start_y`, `segment_end_x`,
#'   and `segment_end_y`. `annot` contains normalized labels such as `alpha`,
#'   `beta`, or linkage position text.
#' @noRd
.linkage_annotation_data <- function(
  structure,
  coor,
  node_size = 1,
  orient = c("left", "right", "up", "down")
) {
  if (igraph::ecount(structure) == 0) {
    return(.empty_linkage_annotation_data())
  }
  orient <- rlang::arg_match(orient)

  child_vertices <- seq_len(length(structure) - 1)
  parent_vertices <- .parent_vertices_for_annotations(structure)
  linkage_labels <- strsplit(
    igraph::E(structure)$linkage[child_vertices],
    "-",
    fixed = TRUE
  )
  row_count <- 2L * length(child_vertices)
  annotation <- data.frame(
    vertice = rep(as.character(child_vertices), each = 2L),
    annot = character(row_count),
    x = numeric(row_count),
    y = numeric(row_count),
    segment_start_x = numeric(row_count),
    segment_start_y = numeric(row_count),
    segment_end_x = numeric(row_count),
    segment_end_y = numeric(row_count),
    stringsAsFactors = FALSE
  )

  for (ver in child_vertices) {
    par_ver <- parent_vertices[[ver]]
    offsets <- .linkage_label_offsets(
      structure,
      coor,
      child_ver = ver,
      parent_ver = par_ver,
      orient = orient
    )
    label_positions <- .linkage_label_positions(
      coor[ver, "x"],
      coor[ver, "y"],
      coor[par_ver, "x"],
      coor[par_ver, "y"],
      chil_offset = offsets[["child"]],
      par_offset = offsets[["parent"]],
      chil_perpendicular_nudge = .beta_perpendicular_nudge_for_linkage(
        linkage_labels[[ver]][[1]],
        coor[ver, "x"],
        coor[par_ver, "x"]
      ),
      node_size = node_size
    )
    rows <- 2L * ver - c(1L, 0L)
    annotation$annot[rows] <- linkage_labels[[ver]][1:2]
    annotation$x[rows] <- c(
      label_positions$chil[[1]] + coor[ver, "x"],
      label_positions$par[[1]] + coor[par_ver, "x"]
    )
    annotation$y[rows] <- c(
      label_positions$chil[[2]] + coor[ver, "y"],
      label_positions$par[[2]] + coor[par_ver, "y"]
    )
    annotation$segment_start_x[rows] <- coor[ver, "x"]
    annotation$segment_start_y[rows] <- coor[ver, "y"]
    annotation$segment_end_x[rows] <- coor[par_ver, "x"]
    annotation$segment_end_y[rows] <- coor[par_ver, "y"]
  }

  annotation$annot <- .normalize_linkage_labels(annotation$annot)
  annotation
}

#' Build an empty linkage annotation table
#'
#' @returns A data frame with linkage annotation columns and zero rows. Columns
#'   are `vertice`, `annot`, `x`, `y`, `segment_start_x`, `segment_start_y`,
#'   `segment_end_x`, and `segment_end_y`.
#' @noRd
.empty_linkage_annotation_data <- function() {
  data.frame(
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
}

#' Build the two linkage annotation rows for one child residue
#'
#' @param structure An igraph glycan graph whose edges include `linkage`.
#' @param coor A numeric coordinate matrix with columns `x` and `y`, one row
#'   per graph vertex.
#' @param ver A single integer child vertex index.
#' @param node_size Numeric node-size multiplier used to keep labels outside
#'   scaled residue polygons.
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#'
#' @returns A two-row data frame with linkage annotation columns. The first row
#'   is the child-side anomer label and the second row is the parent-side
#'   linkage-position label.
#' @noRd
.linkage_annotation_rows <- function(
  structure,
  coor,
  ver,
  node_size = 1,
  orient = c("left", "right", "up", "down")
) {
  orient <- rlang::arg_match(orient)
  par_ver <- .parent_vertex_for_annotation(structure, ver)
  labels <- strsplit(igraph::E(structure)[ver]$linkage, '-')[[1]]
  offsets <- .linkage_label_offsets(
    structure,
    coor,
    child_ver = ver,
    parent_ver = par_ver,
    orient = orient
  )
  label_positions <- .linkage_label_positions(
    coor[ver, "x"],
    coor[ver, "y"],
    coor[par_ver, "x"],
    coor[par_ver, "y"],
    chil_offset = offsets[["child"]],
    par_offset = offsets[["parent"]],
    chil_perpendicular_nudge = .beta_perpendicular_nudge_for_linkage(
      labels[[1]],
      coor[ver, "x"],
      coor[par_ver, "x"]
    ),
    node_size = node_size
  )

  dplyr::bind_rows(
    .linkage_annotation_row(
      ver = ver,
      annot = labels[1],
      annot_coor = as.vector(label_positions$chil) + coor[ver, ],
      segment_start = coor[ver, ],
      segment_end = coor[par_ver, ]
    ),
    .linkage_annotation_row(
      ver = ver,
      annot = labels[2],
      annot_coor = as.vector(label_positions$par) + coor[par_ver, ],
      segment_start = coor[ver, ],
      segment_end = coor[par_ver, ]
    )
  )
}

#' Find the parent vertex used for linkage annotations
#'
#' @param structure An igraph glycan graph.
#' @param ver A single integer child vertex index.
#'
#' @returns A single integer parent vertex index. The value is the penultimate
#'   vertex on the shortest path from the reducing end to `ver`.
#' @noRd
.parent_vertex_for_annotation <- function(structure, ver) {
  dplyr::nth(
    as.vector(igraph::shortest_paths(
      structure,
      length(structure),
      ver
    )$vpath[[1]]),
    -2
  )
}

#' Find parent vertices used for all linkage annotations
#'
#' @param structure An igraph glycan graph.
#'
#' @returns An integer vector with one parent vertex for each non-reducing
#'   vertex, ordered by child vertex index.
#' @noRd
.parent_vertices_for_annotations <- function(structure) {
  traversal <- igraph::bfs(
    structure,
    root = length(structure),
    mode = "all",
    unreachable = FALSE,
    parent = TRUE
  )
  as.integer(traversal$parent[seq_len(length(structure) - 1)])
}

#' Calculate child-side and parent-side linkage label offsets
#'
#' @param structure An igraph glycan graph whose vertices include `mono`.
#' @param coor A numeric coordinate matrix with columns `x` and `y`, one row
#'   per graph vertex.
#' @param child_ver A single integer child vertex index.
#' @param parent_ver A single integer parent vertex index.
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#'
#' @returns A named numeric vector with values `child` and `parent`, giving the
#'   distance from each residue center to its label.
#' @noRd
.linkage_label_offsets <- function(
  structure,
  coor,
  child_ver,
  parent_ver,
  orient = c("left", "right", "up", "down")
) {
  orient <- rlang::arg_match(orient)
  c(
    child = .linkage_label_offset(
      structure,
      child_ver,
      coor[child_ver, "x"],
      coor[child_ver, "y"],
      coor[parent_ver, "x"],
      coor[parent_ver, "y"],
      role = "child",
      orient = orient
    ),
    parent = .linkage_label_offset(
      structure,
      parent_ver,
      coor[parent_ver, "x"],
      coor[parent_ver, "y"],
      coor[child_ver, "x"],
      coor[child_ver, "y"],
      role = "parent",
      orient = orient
    )
  )
}

#' Scale one annotation offset for larger residue nodes
#'
#' @param offset Numeric default label offset.
#' @param node_size Numeric node-size multiplier.
#'
#' @returns A numeric scalar offset. The value grows with the extra node radius
#'   but remains below the midpoint between unit-spaced residue centers.
#' @noRd
.scaled_annotation_offset <- function(offset, node_size) {
  pmin(offset + .annotation_extra_offset(node_size), 0.49)
}

#' Build one linkage annotation row
#'
#' @param ver A single integer child vertex index.
#' @param annot A string linkage label for this row.
#' @param annot_coor A numeric vector with `x` and `y` annotation coordinates.
#' @param segment_start A numeric vector with `x` and `y` coordinates for the
#'   child-side segment endpoint.
#' @param segment_end A numeric vector with `x` and `y` coordinates for the
#'   parent-side segment endpoint.
#'
#' @returns A one-row data frame with linkage annotation columns.
#' @noRd
.linkage_annotation_row <- function(
  ver,
  annot,
  annot_coor,
  segment_start,
  segment_end
) {
  data.frame(
    vertice = as.character(ver),
    annot = annot,
    x = as.numeric(annot_coor[["x"]]),
    y = as.numeric(annot_coor[["y"]]),
    segment_start_x = as.numeric(segment_start[["x"]]),
    segment_start_y = as.numeric(segment_start[["y"]]),
    segment_end_x = as.numeric(segment_end[["x"]]),
    segment_end_y = as.numeric(segment_end[["y"]]),
    stringsAsFactors = FALSE
  )
}

#' Normalize linkage labels for plotmath
#'
#' @param labels A character vector of raw linkage labels.
#'
#' @returns A character vector the same length as `labels`. Labels beginning
#'   with `a` become `alpha`, labels beginning with `b` become `beta`, and all
#'   other labels are returned unchanged.
#' @noRd
.normalize_linkage_labels <- function(labels) {
  normalized <- labels
  normalized[tolower(substr(normalized, 1, 1)) == "b"] <- "beta"
  normalized[tolower(substr(normalized, 1, 1)) == "a"] <- "alpha"
  normalized
}

#' Reflect one point across a line segment
#'
#' @param point A numeric vector with `x` and `y` values.
#' @param segment_start A numeric vector with `x` and `y` values for the start
#'   of the segment.
#' @param segment_end A numeric vector with `x` and `y` values for the end of
#'   the segment.
#'
#' @returns A numeric vector with the reflected `x` and `y` coordinates. If the
#'   segment length is zero, returns `point` unchanged.
#' @noRd
.reflect_point_over_segment <- function(point, segment_start, segment_end) {
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
#' @param coords A numeric matrix with columns `x` and `y`.
#' @param group_keys A character vector mapping rows to linkage groups.
#'   Distances within the same non-missing group are ignored.
#'
#' @returns A numeric scalar minimum pairwise Euclidean distance among complete
#'   rows from different groups. Returns `Inf` when fewer than two eligible rows
#'   are available.
#' @noRd
.minimum_annotation_distance <- function(
  coords,
  group_keys = rep(NA_character_, nrow(coords))
) {
  finite <- stats::complete.cases(coords)
  if (sum(finite) < 2) {
    return(Inf)
  }

  distances <- as.matrix(stats::dist(coords[finite, , drop = FALSE]))
  finite_groups <- group_keys[finite]
  same_group <- outer(finite_groups, finite_groups, "==")
  same_group[is.na(same_group)] <- FALSE
  distances[same_group] <- Inf
  diag(distances) <- Inf
  min(distances)
}

#' Check whether two annotation rows are sufficiently separated
#'
#' @param coords A numeric matrix with columns `x` and `y`.
#' @param i,j Integer row indices in `coords`.
#' @param min_distance Numeric minimum distance between annotation centers.
#'
#' @returns A logical scalar: `TRUE` when rows `i` and `j` are at least
#'   `min_distance` apart.
#' @noRd
.annotations_are_separated <- function(coords, i, j, min_distance) {
  delta <- coords[i, ] - coords[j, ]
  distance <- sqrt(sum(delta^2))
  distance >= min_distance
}

#' Reflect annotation coordinates across their linkage segments
#'
#' @param coords A numeric matrix with columns `x` and `y`.
#' @param segment A numeric matrix with columns `segment_start_x`,
#'   `segment_start_y`, `segment_end_x`, and `segment_end_y`.
#'
#' @returns A numeric matrix with the same dimensions as `coords`. Rows with
#'   complete segment coordinates are reflected; other rows are unchanged.
#' @noRd
.reflected_annotation_coordinates <- function(coords, segment) {
  can_reflect <- stats::complete.cases(segment)
  reflected_coords <- coords
  for (i in which(can_reflect)) {
    reflected_coords[i, ] <- .reflect_point_over_segment(
      point = coords[i, ],
      segment_start = segment[i, c("segment_start_x", "segment_start_y")],
      segment_end = segment[i, c("segment_end_x", "segment_end_y")]
    )
  }
  reflected_coords
}

#' Group annotations that should reflect together
#'
#' @param annotation A data frame with `vertice` and segment coordinate columns.
#' @param segment A numeric matrix with segment start/end columns.
#' @param segment_lengths A numeric vector of segment lengths, one per
#'   annotation row.
#'
#' @returns A list with `keys`, a character vector mapping rows to group ids;
#'   `rows`, a list of integer row indices by group; and `lengths`, a numeric
#'   vector of maximum segment length by group.
#' @noRd
.annotation_reflection_groups <- function(
  annotation,
  segment,
  segment_lengths
) {
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

#' Choose which annotation group to reflect for one overlap
#'
#' @param i,j Integer row indices for two overlapping annotation rows.
#' @param coords A numeric matrix with current `x` and `y` annotation
#'   coordinates.
#' @param reflected_coords A numeric matrix with candidate reflected
#'   coordinates, same dimensions as `coords`.
#' @param groups A list returned by `.annotation_reflection_groups()`.
#' @param reflected_groups A named logical vector marking groups that have
#'   already been reflected.
#'
#' @returns A single character group key, or `NULL` if neither row belongs to an
#'   unreflected candidate group.
#' @noRd
.choose_annotation_group_to_reflect <- function(
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
      .minimum_annotation_distance(candidate_coords, groups$keys)
    }
  )

  best_score <- max(candidate_scores)
  best_candidates <- candidate_groups[
    abs(candidate_scores - best_score) <= sqrt(.Machine$double.eps)
  ]
  best_candidates[which.max(groups$lengths[best_candidates])]
}

#' Resolve annotation overlaps by reflecting whole label groups
#'
#' @param coords A numeric matrix with current `x` and `y` annotation
#'   coordinates.
#' @param reflected_coords A numeric matrix with candidate reflected
#'   coordinates, same dimensions as `coords`.
#' @param finite_index An integer vector of row indices in `coords` that have
#'   finite coordinates.
#' @param groups A list returned by `.annotation_reflection_groups()`.
#' @param min_distance Numeric minimum distance between annotation centers.
#' @param max_iter Integer maximum number of overlap resolution passes.
#'
#' @returns A numeric matrix with the same dimensions as `coords`, after any
#'   selected row groups have been replaced with reflected coordinates.
#' @noRd
.resolve_annotation_group_overlaps <- function(
  coords,
  reflected_coords,
  finite_index,
  groups,
  min_distance,
  max_iter
) {
  reflected_groups <- rep(FALSE, length(groups$rows))
  names(reflected_groups) <- names(groups$rows)
  pairs <- .annotation_overlap_pairs(finite_index)

  for (iter in seq_len(max_iter)) {
    result <- .resolve_annotation_overlap_pass(
      coords,
      reflected_coords,
      groups,
      reflected_groups,
      pairs,
      min_distance
    )
    coords <- result$coords
    reflected_groups <- result$reflected_groups

    if (!result$shifted) {
      break
    }
  }

  coords
}

#' Build all finite annotation row pairs to compare
#'
#' @param finite_index An integer vector of annotation row indices with finite
#'   `x` and `y` coordinates.
#'
#' @returns A data frame with integer columns `i` and `j`. Each row is a unique
#'   pair from `finite_index`, with `i` preceding `j` in the input order.
#' @noRd
.annotation_overlap_pairs <- function(finite_index) {
  if (length(finite_index) < 2) {
    return(data.frame(i = integer(0), j = integer(0)))
  }

  purrr::map_dfr(
    seq_len(length(finite_index) - 1),
    function(i_pos) {
      data.frame(
        i = finite_index[i_pos],
        j = finite_index[(i_pos + 1):length(finite_index)]
      )
    }
  )
}

#' Run one annotation-overlap resolution pass
#'
#' @param coords A numeric matrix with current `x` and `y` annotation
#'   coordinates.
#' @param reflected_coords A numeric matrix with candidate reflected
#'   coordinates, same dimensions as `coords`.
#' @param groups A list returned by `.annotation_reflection_groups()`.
#' @param reflected_groups A named logical vector marking groups that have
#'   already been reflected.
#' @param pairs A data frame returned by `.annotation_overlap_pairs()`.
#' @param min_distance Numeric minimum distance between annotation centers.
#'
#' @returns A list with updated `coords`, updated `reflected_groups`, and
#'   `shifted`, a logical scalar indicating whether any group was reflected.
#' @noRd
.resolve_annotation_overlap_pass <- function(
  coords,
  reflected_coords,
  groups,
  reflected_groups,
  pairs,
  min_distance
) {
  shifted <- FALSE
  for (pair in seq_len(nrow(pairs))) {
    i <- pairs$i[pair]
    j <- pairs$j[pair]
    same_group <- !is.na(groups$keys[[i]]) &&
      !is.na(groups$keys[[j]]) &&
      groups$keys[[i]] == groups$keys[[j]]
    if (same_group) {
      next
    }
    if (.annotations_are_separated(coords, i, j, min_distance)) {
      next
    }

    result <- .reflect_best_annotation_group(
      i,
      j,
      coords,
      reflected_coords,
      groups,
      reflected_groups
    )
    coords <- result$coords
    reflected_groups <- result$reflected_groups
    shifted <- shifted || result$shifted
  }

  list(
    coords = coords,
    reflected_groups = reflected_groups,
    shifted = shifted
  )
}

#' Reflect the best available annotation group for one overlap
#'
#' @param i,j Integer row indices for two overlapping annotation rows.
#' @param coords A numeric matrix with current `x` and `y` annotation
#'   coordinates.
#' @param reflected_coords A numeric matrix with candidate reflected
#'   coordinates, same dimensions as `coords`.
#' @param groups A list returned by `.annotation_reflection_groups()`.
#' @param reflected_groups A named logical vector marking groups that have
#'   already been reflected.
#'
#' @returns A list with updated `coords`, updated `reflected_groups`, and
#'   `shifted`, a logical scalar indicating whether a group was reflected.
#' @noRd
.reflect_best_annotation_group <- function(
  i,
  j,
  coords,
  reflected_coords,
  groups,
  reflected_groups
) {
  best <- .choose_annotation_group_to_reflect(
    i,
    j,
    coords,
    reflected_coords,
    groups,
    reflected_groups
  )
  if (is.null(best)) {
    return(list(
      coords = coords,
      reflected_groups = reflected_groups,
      shifted = FALSE
    ))
  }

  rows <- groups$rows[[best]]
  coords[rows, ] <- reflected_coords[rows, ]
  reflected_groups[best] <- TRUE
  list(
    coords = coords,
    reflected_groups = reflected_groups,
    shifted = TRUE
  )
}

#' Separate overlapping annotation labels
#'
#' @param annotation A data frame with at least `x` and `y` columns. If present,
#'   segment columns `segment_start_x`, `segment_start_y`, `segment_end_x`, and
#'   `segment_end_y` are used to reflect labels across their linkage segments.
#' @param min_distance Numeric minimum distance between annotation centers.
#' @param max_iter Integer maximum number of overlap resolution passes.
#'
#' @returns The same data frame columns as `annotation`, with adjusted numeric
#'   `x` and `y` columns when overlaps can be resolved.
#' @noRd
.separate_overlapping_annotations <- function(
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

  reflected_coords <- .reflected_annotation_coordinates(coords, segment)
  groups <- .annotation_reflection_groups(annotation, segment, segment_lengths)
  coords <- .resolve_annotation_group_overlaps(
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

#' Build substituent annotation rows
#'
#' @param structure An igraph glycan graph whose vertices may include `sub`.
#' @param coor A numeric coordinate matrix with columns `x` and `y`, one row
#'   per graph vertex.
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#' @param node_size Numeric node-size multiplier used to keep labels outside
#'   scaled residue polygons.
#'
#' @returns A data frame with columns `vertice`, `annot`, `x`, `y`, `hjust`,
#'   and `vjust`. Unknown linkage prefixes such as `?` are removed from
#'   `annot`. Returns an empty data frame with the same columns when no
#'   substituents are present.
#' @noRd
.substituent_annotation_data <- function(
  structure,
  coor,
  orient,
  node_size = 1
) {
  sub <- igraph::V(structure)$sub
  if (length(sub) == 0) {
    return(data.frame(
      vertice = character(0),
      annot = character(0),
      x = numeric(0),
      y = numeric(0),
      hjust = numeric(0),
      vjust = numeric(0)
    ))
  }

  sub[is.na(sub)] <- ""
  sub_pos <- which(sub != "")
  if (length(sub_pos) == 0) {
    return(data.frame(
      vertice = character(0),
      annot = character(0),
      x = numeric(0),
      y = numeric(0),
      hjust = numeric(0),
      vjust = numeric(0)
    ))
  }

  offset_distance <- .scaled_annotation_offset(0.28, node_size)
  offset <- .rotate_cartoon_vector(
    c(x = 0, y = offset_distance),
    orient
  )
  hjust <- if (offset[["x"]] > 0) {
    0
  } else if (offset[["x"]] < 0) {
    1
  } else {
    0.5
  }
  vjust <- if (offset[["y"]] > 0) {
    0
  } else if (offset[["y"]] < 0) {
    1
  } else {
    0.5
  }

  data.frame(
    vertice = as.character(sub_pos),
    annot = sub("^\\?+", "", sub[sub_pos]),
    x = as.numeric(coor[sub_pos, "x"] + offset["x"]),
    y = as.numeric(coor[sub_pos, "y"] + offset["y"]),
    hjust = hjust,
    vjust = vjust,
    stringsAsFactors = FALSE
  )
}

#' Build invisible bounds for substituent labels
#'
#' @param annotation A substituent annotation data frame returned by
#'   `.substituent_annotation_data()`.
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#'
#' @returns A data frame with numeric columns `x` and `y`. Horizontal labels
#'   return top bound points; vertical labels return right-side bound points.
#' @noRd
.substituent_annotation_bounds <- function(
  annotation,
  orient = c("left", "right", "up", "down")
) {
  orient <- rlang::arg_match(orient)
  if (nrow(annotation) == 0) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }

  direction <- .rotate_cartoon_vector(c(x = 0, y = 1), orient)
  data.frame(
    x = annotation$x +
      direction[["x"]] * .substituent_label_width(annotation$annot),
    y = annotation$y +
      direction[["y"]] * .substituent_label_height(annotation$annot)
  )
}

#' Approximate substituent label width in coordinate units
#'
#' @param label A character vector of substituent labels.
#'
#' @returns A numeric vector with one width per label.
#' @noRd
.substituent_label_width <- function(label) {
  pmax(nchar(label), 1) * 0.18
}

#' Approximate substituent label height in coordinate units
#'
#' @param label A character vector of substituent labels.
#'
#' @returns A numeric vector with one height per label.
#' @noRd
.substituent_label_height <- function(label) {
  rep(0.36, length(label))
}

#' Check whether text is already valid plotmath
#'
#' @param annot A character vector of annotation labels.
#'
#' @returns A logical vector the same length as `annot`; `TRUE` means
#'   `parse(text = annot)` succeeds.
#' @noRd
.can_parse_plotmath <- function(annot) {
  purrr::map_lgl(annot, function(x) {
    !inherits(try(parse(text = x), silent = TRUE), "try-error")
  })
}

#' Quote plain text for plotmath parsing
#'
#' @param annot A character vector of annotation labels.
#'
#' @returns A character vector the same length as `annot`, with each value
#'   wrapped in double quotes and embedded quotes escaped.
#' @noRd
.quote_plotmath_text <- function(annot) {
  encodeString(annot, quote = '"')
}

# Recover plain text previously stored for plotmath parsing.
.unquote_plotmath_text <- function(annot) {
  quoted <- !is.na(annot) &
    startsWith(annot, '"') &
    endsWith(annot, '"') &
    nchar(annot) >= 2L
  annot[quoted] <- substr(annot[quoted], 2L, nchar(annot[quoted]) - 1L)
  annot[quoted] <- gsub('\\"', '"', annot[quoted], fixed = TRUE)
  annot[quoted] <- gsub("\\\\", "\\", annot[quoted], fixed = TRUE)
  annot
}

#' Parse a tagged reducing-end amino-acid sequence
#'
#' @param red_end A reducing-end annotation string.
#'
#' @returns `NULL` when `red_end` is ordinary custom text, otherwise a named
#'   list containing `prefix`, `site`, and `suffix`.
#' @noRd
.parse_reducing_end_aa_sequence <- function(red_end) {
  has_site_tag <- grepl("<site>|</site>", red_end, perl = TRUE)
  if (!has_site_tag) {
    return(NULL)
  }

  matched <- regexec(
    "^([A-Za-z]*)<site>(.)</site>([A-Za-z]*)$",
    red_end,
    perl = TRUE
  )
  parts <- regmatches(red_end, matched)[[1]]
  if (length(parts) != 4L || nchar(parts[[3]], type = "chars") != 1L) {
    cli::cli_abort(c(
      "{.arg red_end} has an invalid amino-acid site annotation.",
      "i" = paste(
        "Use one {.code <site></site>} pair containing exactly one",
        "character, for example {.code ABC<site>D</site>EFG}."
      )
    ))
  }

  stats::setNames(as.list(parts[2:4]), c("prefix", "site", "suffix"))
}

#' Format a tagged amino-acid sequence as plotmath
#'
#' @param sequence A parsed sequence from
#'   `.parse_reducing_end_aa_sequence()`.
#'
#' @returns A length-one plotmath string with a bold site.
#' @noRd
.format_reducing_end_aa_sequence <- function(sequence) {
  paste0(
    .quote_plotmath_text(sequence$prefix),
    "~bold(",
    .quote_plotmath_text(sequence$site),
    ")~",
    .quote_plotmath_text(sequence$suffix)
  )
}

#' Calculate the tagged-site position within an amino-acid sequence
#'
#' @param sequence A parsed sequence from
#'   `.parse_reducing_end_aa_sequence()`.
#'
#' @returns A numeric justification between zero and one.
#' @noRd
.reducing_end_aa_sequence_hjust <- function(sequence) {
  prefix_width <- nchar(sequence$prefix, type = "width")
  suffix_width <- nchar(sequence$suffix, type = "width")
  (prefix_width + 1.5) / (prefix_width + suffix_width + 3)
}

#' Calculate amino-acid sequence text rotation
#'
#' @param orient Drawing orientation.
#'
#' @returns Zero for vertical glycans, 90 for leftward glycans, and -90 for
#'   rightward glycans.
#' @noRd
.reducing_end_aa_sequence_angle <- function(orient) {
  switch(
    orient,
    left = 90,
    right = -90,
    up = 0,
    down = 0
  )
}

#' Build reducing-end segment, label, wave, and bounds data
#'
#' @param structure An igraph glycan graph. The graph attribute `anomer`
#'   supplies the reducing-end alpha/beta/unknown label.
#' @param coor A numeric coordinate matrix with columns `x` and `y`, one row
#'   per graph vertex.
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#' @param red_end A string. `""` draws only the current reducing-end line,
#'   `"~"` draws a wavy end, a string with one `<site>` tag draws an amino-acid
#'   sequence, and any other string draws custom text. Ignored when
#'   `red_end_length` is `0`.
#' @param red_end_length Length of the reducing-end line in plot coordinate
#'   units. At `0`, the line and all `red_end` decorations are omitted while
#'   the core anomer annotation remains.
#' @param red_end_size Size of custom reducing-end text.
#'
#' @returns A list with data frames `annotation`, `segment`, `wave`, and
#'   `bounds`. `annotation` contains text rows; `segment` contains one line
#'   segment row when `red_end_length` is positive; `wave` contains path
#'   coordinates for `"~"`; `bounds` contains invisible points used to reserve
#'   space for custom text.
#' @noRd
.reducing_end_annotation_data <- function(
  structure,
  coor,
  orient = c("left", "right", "up", "down"),
  red_end = "",
  red_end_length = 0.6,
  red_end_size = 6
) {
  orient <- rlang::arg_match(orient)
  checkmate::assert_string(red_end, na.ok = FALSE)
  anomer <- igraph::graph_attr(structure, "anomer")
  if (.has_no_reducing_end_anomer(anomer)) {
    return(.empty_reducing_end_annotation_data())
  }

  label <- .reducing_end_anomer_label(anomer)
  root <- length(structure)
  geometry <- .reducing_end_geometry(
    coor[root, ],
    orient,
    label = label,
    line_length = red_end_length
  )
  anomer_annotation <- .reducing_end_anomer_row(
    root,
    label,
    geometry$label_coor
  )
  if (red_end_length == 0) {
    result <- .empty_reducing_end_annotation_data()
    result$annotation <- anomer_annotation
    return(result)
  }
  red_end_annotation <- .reducing_end_text_data(
    red_end,
    geometry$line_end,
    geometry$line_vec,
    orient,
    root
  )
  red_end_bounds <- .reducing_end_text_bounds(
    red_end,
    geometry$line_end,
    geometry$line_vec,
    orient,
    red_end_size
  )
  list(
    annotation = dplyr::bind_rows(
      anomer_annotation,
      red_end_annotation
    ),
    segment = .reducing_end_segment_data(
      geometry$root_coor,
      geometry$line_end
    ),
    wave = .reducing_end_wave_data(
      red_end,
      geometry$line_end,
      geometry$line_vec
    ),
    bounds = red_end_bounds
  )
}

#' Check whether a graph has no reducing-end anomer label
#'
#' @param anomer The `anomer` graph attribute from an igraph glycan graph.
#'
#' @returns A logical scalar: `TRUE` when `anomer` is missing, `NA`, or an
#'   empty string.
#' @noRd
.has_no_reducing_end_anomer <- function(anomer) {
  length(anomer) == 0 || is.na(anomer) || anomer == ""
}

#' Build an empty reducing-end annotation result
#'
#' @returns A list with zero-row data frames `annotation`, `segment`, `wave`,
#'   and `bounds`. The columns match the non-empty result from
#'   `.reducing_end_annotation_data()`.
#' @noRd
.empty_reducing_end_annotation_data <- function() {
  list(
    annotation = data.frame(
      vertice = character(0),
      annot = character(0),
      x = numeric(0),
      y = numeric(0),
      hjust = numeric(0),
      vjust = numeric(0),
      is_red_end_text = logical(0),
      is_aa_sequence = logical(0),
      angle = numeric(0)
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
    ),
    bounds = data.frame(
      x = numeric(0),
      y = numeric(0)
    )
  )
}

#' Convert a reducing-end anomer attribute to a plot label
#'
#' @param anomer A non-empty string graph attribute such as `"a"`, `"b"`,
#'   `"alpha"`, `"beta"`, or an unknown value.
#'
#' @returns A string. Values beginning with `a` return `alpha`, values
#'   beginning with `b` return `beta`, and all other values return the plotmath
#'   expression for an unknown label.
#' @noRd
.reducing_end_anomer_label <- function(anomer) {
  label <- tolower(substr(anomer, 1, 1))
  if (label == "a") {
    return("alpha")
  }
  if (label == "b") {
    return("beta")
  }
  '~"?"'
}

#' Calculate reducing-end line and anomer-label coordinates
#'
#' @param root_coor A numeric vector with `x` and `y` coordinates for the
#'   reducing-end residue.
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#' @param label The reducing-end anomer label.
#' @param line_length Numeric length of the reducing-end line segment.
#' @param label_distance Numeric distance from the reducing-end residue to the
#'   anomer label anchor before rotation.
#'
#' @returns A list with named numeric vectors `root_coor`, `line_vec`,
#'   `line_end`, and `label_coor`, each containing `x` and `y` values.
#' @noRd
.reducing_end_geometry <- function(
  root_coor,
  orient,
  label = "",
  line_length = 0.6,
  label_distance = 0.7
) {
  root_coor <- c(
    x = as.numeric(root_coor[["x"]]),
    y = as.numeric(root_coor[["y"]])
  )
  line_vec <- .reducing_end_line_vector(orient, line_length)
  label_vec <- .reducing_end_line_vector(orient, label_distance)
  if (line_length == 0) {
    label_position_offset <- matrix(0.6 * label_vec, ncol = 1)
  } else {
    label_position_offset <- matrix(
      .rotated_reducing_end_label_vector(label_vec),
      ncol = 1
    )
    label_position_offset <- .nudge_child_label_perpendicular(
      label_offset = label_position_offset,
      direction = matrix(label_vec, ncol = 1),
      nudge = .beta_perpendicular_nudge_for_linkage(
        label,
        root_coor[["x"]],
        root_coor[["x"]] + label_vec[["x"]]
      )
    )
  }
  label_coor <- root_coor +
    c(
      x = as.numeric(label_position_offset[1, 1]),
      y = as.numeric(label_position_offset[2, 1])
    )

  list(
    root_coor = root_coor,
    line_vec = line_vec,
    line_end = root_coor + line_vec,
    label_coor = label_coor
  )
}

#' Build an orientation-specific reducing-end line vector
#'
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#' @param length Numeric vector length in plot coordinate units.
#'
#' @returns A named numeric vector `c(x, y)` pointing away from the glycan.
#' @noRd
.reducing_end_line_vector <- function(orient, length) {
  .rotate_cartoon_vector(c(x = length, y = 0), orient)
}

#' Rotate the reducing-end anomer label away from the line
#'
#' @param label_vec A named numeric vector `c(x, y)` pointing from the reducing
#'   end toward the unrotated label position.
#'
#' @returns A named numeric vector `c(x, y)` containing the rotated label offset.
#' @noRd
.rotated_reducing_end_label_vector <- function(label_vec) {
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
  rotated <- 0.6 * rotate_matrix %*% matrix(label_vec, ncol = 1)
  c(x = as.numeric(rotated[1, 1]), y = as.numeric(rotated[2, 1]))
}

#' Build the reducing-end anomer annotation row
#'
#' @param root A single integer reducing-end vertex index.
#' @param label A string plotmath label for the reducing-end anomer.
#' @param label_coor A named numeric vector `c(x, y)` for the label position.
#'
#' @returns A one-row data frame with columns `vertice`, `annot`, `x`, `y`,
#'   `hjust`, `vjust`, and `is_red_end_text`.
#' @noRd
.reducing_end_anomer_row <- function(root, label, label_coor) {
  data.frame(
    vertice = as.character(root),
    annot = label,
    x = as.numeric(label_coor[["x"]]),
    y = as.numeric(label_coor[["y"]]),
    hjust = 0.5,
    vjust = 0.5,
    is_red_end_text = FALSE,
    is_aa_sequence = FALSE,
    angle = 0
  )
}

#' Build the reducing-end line segment row
#'
#' @param root_coor A named numeric vector `c(x, y)` for the reducing-end
#'   residue coordinate.
#' @param line_end A named numeric vector `c(x, y)` for the line endpoint.
#'
#' @returns A one-row data frame with columns `start_x`, `start_y`, `end_x`,
#'   and `end_y`.
#' @noRd
.reducing_end_segment_data <- function(root_coor, line_end) {
  data.frame(
    start_x = as.numeric(root_coor[["x"]]),
    start_y = as.numeric(root_coor[["y"]]),
    end_x = as.numeric(line_end[["x"]]),
    end_y = as.numeric(line_end[["y"]])
  )
}

#' Build custom reducing-end text rows
#'
#' @param red_end A string reducing-end annotation.
#' @param line_end A named numeric vector `c(x, y)` for the reducing-end line
#'   endpoint.
#' @param line_vec A named numeric vector `c(x, y)` for the reducing-end line
#'   direction.
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#' @param root Integer reducing-end vertex index.
#'
#' @returns A data frame with columns `vertice`, `annot`, `x`, `y`, `hjust`,
#'   `vjust`, and `is_red_end_text`. Returns an empty data frame with those
#'   columns for `""` and `"~"`.
#' @noRd
.reducing_end_text_data <- function(
  red_end,
  line_end,
  line_vec,
  orient,
  root
) {
  if (red_end %in% c("", "~")) {
    return(data.frame(
      vertice = character(0),
      annot = character(0),
      x = numeric(0),
      y = numeric(0),
      hjust = numeric(0),
      vjust = numeric(0),
      is_red_end_text = logical(0),
      is_aa_sequence = logical(0),
      angle = numeric(0)
    ))
  }
  sequence <- .parse_reducing_end_aa_sequence(red_end)
  line_unit <- line_vec / sqrt(sum(line_vec^2))
  text_offset <- 0.02
  text_coor <- line_end + line_unit * text_offset
  hjust <- if (line_unit[["x"]] > 0) {
    0
  } else if (line_unit[["x"]] < 0) {
    1
  } else {
    0.5
  }
  vjust <- if (line_unit[["y"]] > 0) {
    0
  } else if (line_unit[["y"]] < 0) {
    1
  } else {
    0.5
  }
  if (!is.null(sequence)) {
    hjust <- .reducing_end_aa_sequence_hjust(sequence)
    vjust <- if (.is_horizontal_glycan_orientation(orient)) 1 else vjust
  }
  data.frame(
    vertice = as.character(root),
    annot = if (is.null(sequence)) {
      .quote_plotmath_text(red_end)
    } else {
      .format_reducing_end_aa_sequence(sequence)
    },
    x = as.numeric(text_coor["x"]),
    y = as.numeric(text_coor["y"]),
    hjust = hjust,
    vjust = vjust,
    is_red_end_text = TRUE,
    is_aa_sequence = !is.null(sequence),
    angle = if (is.null(sequence)) {
      0
    } else {
      .reducing_end_aa_sequence_angle(orient)
    }
  )
}

#' Build invisible bounds for custom reducing-end text
#'
#' @param red_end A string reducing-end annotation.
#' @param line_end A named numeric vector `c(x, y)` for the reducing-end line
#'   endpoint.
#' @param line_vec A named numeric vector `c(x, y)` for the reducing-end line
#'   direction.
#' @param orient Drawing orientation, one of `"left"`, `"right"`, `"up"`, or
#'   `"down"`.
#' @param red_end_size Size of custom reducing-end text.
#'
#' @returns A data frame with numeric columns `x` and `y`. Horizontal text
#'   returns one bound point; vertical text returns two bound points; `""` and
#'   `"~"` return zero rows.
#' @noRd
.reducing_end_text_bounds <- function(
  red_end,
  line_end,
  line_vec,
  orient,
  red_end_size = 6
) {
  if (red_end %in% c("", "~")) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  line_unit <- line_vec / sqrt(sum(line_vec^2))
  text_offset <- 0.1
  sequence <- .parse_reducing_end_aa_sequence(red_end)
  display_width <- if (is.null(sequence)) {
    nchar(red_end, type = "width")
  } else {
    nchar(
      paste0(sequence$prefix, " ", sequence$site, " ", sequence$suffix),
      type = "width"
    )
  }
  size_scale <- red_end_size / 6
  text_width <- max(display_width, 1) * 0.12 * size_scale
  if (!is.null(sequence)) {
    hjust <- .reducing_end_aa_sequence_hjust(sequence)
    angle <- .reducing_end_aa_sequence_angle(orient) * pi / 180
    text_unit <- c(x = cos(angle), y = sin(angle))
    text_coor <- line_end + line_unit * 0.02
    along <- c(-hjust, 1 - hjust) * text_width
    return(data.frame(
      x = as.numeric(text_coor[["x"]] + text_unit[["x"]] * along),
      y = as.numeric(text_coor[["y"]] + text_unit[["y"]] * along)
    ))
  }
  if (.is_horizontal_glycan_orientation(orient)) {
    text_bound <- line_end + line_unit * (text_offset + text_width)
    return(data.frame(
      x = as.numeric(text_bound["x"]),
      y = as.numeric(text_bound["y"])
    ))
  }
  text_coor <- line_end + line_unit * 0.02
  text_height <- 0.36 * size_scale
  data.frame(
    x = as.numeric(text_coor["x"] + c(-1, 1) * text_width / 2),
    y = as.numeric(
      text_coor["y"] + line_unit[["y"]] * c(0, text_height)
    )
  )
}

#' Build reducing-end wave path coordinates
#'
#' @param red_end A string reducing-end annotation.
#' @param line_end A named numeric vector `c(x, y)` for the reducing-end line
#'   endpoint.
#' @param line_vec A named numeric vector `c(x, y)` for the reducing-end line
#'   direction.
#'
#' @returns A data frame with numeric columns `x` and `y` containing wave path
#'   points when `red_end` is `"~"`; otherwise returns zero rows.
#' @noRd
.reducing_end_wave_data <- function(red_end, line_end, line_vec) {
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
