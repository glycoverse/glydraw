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

#' Build linkage annotation rows for every glycosidic edge
#'
#' @param structure An igraph glycan graph whose edges include `linkage`.
#' @param coor A numeric coordinate matrix with columns `x` and `y`, one row
#'   per graph vertex.
#'
#' @returns A data frame with one or two rows per edge and columns `vertice`,
#'   `annot`, `x`, `y`, `segment_start_x`, `segment_start_y`, `segment_end_x`,
#'   and `segment_end_y`. `annot` contains normalized labels such as `alpha`,
#'   `beta`, or linkage position text.
#' @noRd
.linkage_annotation_data <- function(structure, coor) {
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
    gly_annot_coor <- .linkage_label_positions(
      coor[ver, 1],
      coor[ver, 2],
      coor[par_ver, 1],
      coor[par_ver, 2],
      chil_offset = .linkage_label_offset(
        structure,
        ver,
        coor[ver, 1],
        coor[ver, 2],
        coor[par_ver, 1],
        coor[par_ver, 2],
        role = "child"
      ),
      par_offset = .linkage_label_offset(
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
#'
#' @returns A numeric scalar minimum pairwise Euclidean distance among complete
#'   rows. Returns `Inf` when fewer than two complete rows are available.
#' @noRd
.minimum_annotation_distance <- function(coords) {
  finite <- stats::complete.cases(coords)
  if (sum(finite) < 2) {
    return(Inf)
  }

  min(stats::dist(coords[finite, , drop = FALSE]))
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
      .minimum_annotation_distance(candidate_coords)
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

  for (iter in seq_len(max_iter)) {
    shifted <- FALSE
    for (i_pos in seq_len(length(finite_index) - 1)) {
      i <- finite_index[i_pos]
      for (j in finite_index[(i_pos + 1):length(finite_index)]) {
        if (.annotations_are_separated(coords, i, j, min_distance)) {
          next
        }

        best <- .choose_annotation_group_to_reflect(
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
#' @param orient Drawing orientation, either `"H"` or `"V"`.
#'
#' @returns A data frame with columns `vertice`, `annot`, `x`, and `y`.
#'   Unknown linkage prefixes such as `?` are removed from `annot`. Returns an
#'   empty data frame with the same columns when no substituents are present.
#' @noRd
.substituent_annotation_data <- function(structure, coor, orient) {
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
  paste0('"', gsub('"', '\\"', annot, fixed = TRUE), '"')
}

#' Build reducing-end segment, label, wave, and bounds data
#'
#' @param structure An igraph glycan graph. The graph attribute `anomer`
#'   supplies the reducing-end alpha/beta/unknown label.
#' @param coor A numeric coordinate matrix with columns `x` and `y`, one row
#'   per graph vertex.
#' @param orient Drawing orientation, either `"H"` or `"V"`.
#' @param red_end A string. `""` draws only the current reducing-end line,
#'   `"~"` draws a wavy end, and any other string draws custom text.
#'
#' @returns A list with data frames `annotation`, `segment`, `wave`, and
#'   `bounds`. `annotation` contains text rows; `segment` contains one line
#'   segment row; `wave` contains path coordinates for `"~"`; `bounds` contains
#'   invisible points used to reserve space for custom text.
#' @noRd
.reducing_end_annotation_data <- function(
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
        y = numeric(0),
        hjust = numeric(0),
        vjust = numeric(0),
        is_red_end_text = logical(0)
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
  label_offset <- 0.1
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
  red_end_annotation <- .reducing_end_text_data(
    red_end,
    line_end,
    line_vec,
    orient,
    root
  )
  red_end_bounds <- .reducing_end_text_bounds(
    red_end,
    line_end,
    line_vec,
    orient
  )
  list(
    annotation = dplyr::bind_rows(
      data.frame(
        vertice = as.character(root),
        annot = label,
        x = as.numeric(annot_coor["x"]),
        y = as.numeric(annot_coor["y"]),
        hjust = 0.5,
        vjust = 0.5,
        is_red_end_text = FALSE
      ),
      red_end_annotation
    ),
    segment = data.frame(
      start_x = as.numeric(root_coor["x"]),
      start_y = as.numeric(root_coor["y"]),
      end_x = as.numeric(line_end["x"]),
      end_y = as.numeric(line_end["y"])
    ),
    wave = .reducing_end_wave_data(red_end, line_end, line_vec),
    bounds = red_end_bounds
  )
}

#' Build custom reducing-end text rows
#'
#' @param red_end A string reducing-end annotation.
#' @param line_end A named numeric vector `c(x, y)` for the reducing-end line
#'   endpoint.
#' @param line_vec A named numeric vector `c(x, y)` for the reducing-end line
#'   direction.
#' @param orient Drawing orientation, either `"H"` or `"V"`.
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
      is_red_end_text = logical(0)
    ))
  }
  line_unit <- line_vec / sqrt(sum(line_vec^2))
  text_offset <- 0.02
  text_coor <- line_end + line_unit * text_offset
  hjust <- if (orient == "H") 0 else 0.5
  vjust <- if (orient == "H") 0.5 else 1
  data.frame(
    vertice = as.character(root),
    annot = .quote_plotmath_text(red_end),
    x = as.numeric(text_coor["x"]),
    y = as.numeric(text_coor["y"]),
    hjust = hjust,
    vjust = vjust,
    is_red_end_text = TRUE
  )
}

#' Build invisible bounds for custom reducing-end text
#'
#' @param red_end A string reducing-end annotation.
#' @param line_end A named numeric vector `c(x, y)` for the reducing-end line
#'   endpoint.
#' @param line_vec A named numeric vector `c(x, y)` for the reducing-end line
#'   direction.
#' @param orient Drawing orientation, either `"H"` or `"V"`.
#'
#' @returns A data frame with numeric columns `x` and `y`. Horizontal text
#'   returns one bound point; vertical text returns two bound points; `""` and
#'   `"~"` return zero rows.
#' @noRd
.reducing_end_text_bounds <- function(red_end, line_end, line_vec, orient) {
  if (red_end %in% c("", "~")) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  line_unit <- line_vec / sqrt(sum(line_vec^2))
  text_offset <- 0.1
  text_width <- max(nchar(red_end), 1) * 0.12
  if (orient == "H") {
    text_bound <- line_end + line_unit * (text_offset + text_width)
    return(data.frame(
      x = as.numeric(text_bound["x"]),
      y = as.numeric(text_bound["y"])
    ))
  }
  text_coor <- line_end + line_unit * 0.02
  text_height <- 0.36
  data.frame(
    x = as.numeric(text_coor["x"] + c(-1, 1) * text_width / 2),
    y = as.numeric(text_coor["y"] + c(0, -text_height))
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
