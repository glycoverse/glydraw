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
#' @examples .annotation_coordinate(chil_glyx, chil_glyy, par_glyx, par_glyy)
#' @noRd
.annotation_coordinate <- function(
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
.annotation_offset <- function(
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
#' @examples .gly_annotation(structure,coor)
#' @noRd
.gly_annotation <- function(structure, coor) {
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
    gly_annot_coor <- .annotation_coordinate(
      coor[ver, 1],
      coor[ver, 2],
      coor[par_ver, 1],
      coor[par_ver, 2],
      chil_offset = .annotation_offset(
        structure,
        ver,
        coor[ver, 1],
        coor[ver, 2],
        coor[par_ver, 1],
        coor[par_ver, 2],
        role = "child"
      ),
      par_offset = .annotation_offset(
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
.reflect_point_across_segment <- function(point, segment_start, segment_end) {
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
.min_annotation_distance <- function(coords) {
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
.are_annotations_separated <- function(coords, i, j, min_distance) {
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
.reflect_annotation_coords <- function(coords, segment) {
  can_reflect <- stats::complete.cases(segment)
  reflected_coords <- coords
  for (i in which(can_reflect)) {
    reflected_coords[i, ] <- .reflect_point_across_segment(
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
.build_annotation_groups <- function(annotation, segment, segment_lengths) {
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
#' @param groups list returned by .build_annotation_groups
#' @param reflected_groups logical vector tracking reflected groups
#'
#' @returns character group key, or NULL if no candidate exists
#' @noRd
.choose_reflection_group <- function(
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
      .min_annotation_distance(candidate_coords)
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
#' @param groups list returned by .build_annotation_groups
#' @param min_distance minimum distance between annotation centers
#' @param max_iter maximum number of overlap resolution passes
#'
#' @returns numeric matrix of resolved coordinates
#' @noRd
.resolve_overlaps <- function(
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
        if (.are_annotations_separated(coords, i, j, min_distance)) {
          next
        }

        best <- .choose_reflection_group(
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
.resolve_annotation_overlap <- function(
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

  reflected_coords <- .reflect_annotation_coords(coords, segment)
  groups <- .build_annotation_groups(annotation, segment, segment_lengths)
  coords <- .resolve_overlaps(
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
.substituent_annotation <- function(structure, coor, orient) {
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
.is_parseable_annotation <- function(annot) {
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
.quote_annotation <- function(annot) {
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
#' @examples .reducing_end_annotation(structure, coor, orient, red_end)
#' @noRd
.reducing_end_annotation <- function(
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
  red_end_annotation <- .reducing_end_text_annotation(
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
    wave = .reducing_end_wave(red_end, line_end, line_vec),
    bounds = red_end_bounds
  )
}

#' Map reducing-end text coordinates
#'
#' @param red_end reducing-end annotation
#' @param line_end reducing-end line endpoint
#' @param line_vec reducing-end line vector
#' @param orient glycan drawing orientation, "H" or "V"
#' @param root reducing-end vertex index
#'
#' @returns a data frame
#' @noRd
.reducing_end_text_annotation <- function(
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
    annot = .quote_annotation(red_end),
    x = as.numeric(text_coor["x"]),
    y = as.numeric(text_coor["y"]),
    hjust = hjust,
    vjust = vjust,
    is_red_end_text = TRUE
  )
}

#' Map reducing-end text scale bounds
#'
#' @param red_end reducing-end annotation
#' @param line_end reducing-end line endpoint
#' @param line_vec reducing-end line vector
#' @param orient glycan drawing orientation, "H" or "V"
#'
#' @returns a data frame
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

#' Map reducing-end wave coordinates
#'
#' @param red_end reducing-end annotation
#' @param line_end reducing-end line endpoint
#' @param line_vec reducing-end line vector
#'
#' @returns a data frame
#' @noRd
.reducing_end_wave <- function(red_end, line_end, line_vec) {
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
