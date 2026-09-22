# Internal helpers for arranging disconnected floating glycan components around
# the main glycan without exposing WURCS-specific attachment semantics.

.floating_bracket_offset <- 0.65
.floating_bracket_padding <- 0.25
.floating_bracket_tick_length <- 0.25
.floating_bracket_gap <- 0.25
.floating_attachment_length <- 1
.floating_component_gap <- 1
.floating_count_gap <- 0.5

.layout_cartoon_coordinates <- function(
  structure,
  floating_parts,
  layout = "SNFG"
) {
  if (nrow(floating_parts) == 0) {
    return(list(
      coor = .calculate_residue_coordinates(structure, layout),
      floating = NULL
    ))
  }

  parts <- purrr::map(seq_len(nrow(floating_parts)), function(i) {
    part <- list(
      part_id = floating_parts$part_id[[i]],
      root = floating_parts$root_node[[i]],
      nodes = floating_parts$nodes[[i]],
      linkage = floating_parts$linkage[[i]]
    )
    part$topology <- .floating_part_topology(structure, part)
    part
  })
  floating_nodes <- unlist(purrr::map(parts, "nodes"), use.names = FALSE)
  main_nodes <- setdiff(seq_len(igraph::vcount(structure)), floating_nodes)
  coor <- matrix(
    NA_real_,
    nrow = igraph::vcount(structure),
    ncol = 2,
    dimnames = list(NULL, c("x", "y"))
  )
  coor[main_nodes, ] <- .component_residue_coordinates(
    structure,
    main_nodes,
    layout
  )

  signatures <- purrr::map_chr(
    parts,
    \(.part) .part$topology$signature
  )
  signature_levels <- unique(signatures)
  groups <- purrr::map(signature_levels, function(signature) {
    members <- parts[signatures == signature]
    representative <- members[[1]]
    representative_nodes <- representative$topology$nodes
    representative_coor <- .component_residue_coordinates(
      structure,
      representative$nodes,
      layout
    )
    list(
      signature = signature,
      count = length(members),
      members = members,
      representative = representative,
      member_node_maps = purrr::map(
        members,
        \(.member) {
          stats::setNames(
            representative_nodes,
            .member$topology$nodes
          )
        }
      ),
      local_coor = representative_coor
    )
  })
  group_order <- order(
    purrr::map_int(
      groups,
      \(.group) length(.group$representative$nodes)
    ),
    purrr::map_int(
      groups,
      \(.group) .group$representative$part_id
    )
  )
  groups <- groups[group_order]
  group_y_offsets <- .floating_group_y_offsets(groups, coor[main_nodes, "y"])
  bracket_x <- min(coor[main_nodes, "x"]) - .floating_bracket_offset
  attachment_end_x <- bracket_x - .floating_bracket_gap
  attachment_root_x <- attachment_end_x - .floating_attachment_length

  groups <- purrr::map2(
    groups,
    group_y_offsets,
    function(group, y_offset) {
      representative <- group$representative
      root_position <- match(
        representative$root,
        representative$nodes
      )
      x_offset <- attachment_root_x -
        group$local_coor[root_position, "x"]

      member_layouts <- purrr::map(group$members, function(member) {
        member_coor <- .component_residue_coordinates(
          structure,
          member$nodes,
          layout
        )
        member_coor[, "x"] <- member_coor[, "x"] + x_offset
        member_coor[, "y"] <- member_coor[, "y"] + y_offset
        list(nodes = member$nodes, coor = member_coor)
      })

      representative_coor <- member_layouts[[1]]$coor
      root_coor <- representative_coor[root_position, ]
      group$coor <- representative_coor
      group$root <- representative$root
      group$linkage <- representative$linkage
      group$member_layouts <- member_layouts
      group$count_x <- min(representative_coor[, "x"]) -
        .floating_count_gap
      group$count_y <- mean(range(representative_coor[, "y"]))
      group$segment <- data.frame(
        start_x = unname(root_coor[["x"]]),
        start_y = unname(root_coor[["y"]]),
        end_x = attachment_end_x,
        end_y = unname(root_coor[["y"]]),
        root = representative$root,
        segment_type = "floating_attachment"
      )
      group
    }
  )
  for (group in groups) {
    for (member_layout in group$member_layouts) {
      coor[member_layout$nodes, ] <- member_layout$coor
    }
  }

  representative_nodes <- unlist(
    purrr::map(groups, \(.group) .group$representative$nodes),
    use.names = FALSE
  )
  main_y <- coor[main_nodes, "y"]
  bracket_y <- range(main_y) +
    c(
      -.floating_bracket_padding,
      .floating_bracket_padding
    )
  bracket_segments <- data.frame(
    start_x = rep(bracket_x, 3),
    start_y = c(bracket_y[[1]], bracket_y[[1]], bracket_y[[2]]),
    end_x = c(
      bracket_x,
      bracket_x + .floating_bracket_tick_length,
      bracket_x + .floating_bracket_tick_length
    ),
    end_y = c(bracket_y[[2]], bracket_y[[1]], bracket_y[[2]]),
    segment_type = "floating_bracket"
  )

  list(
    coor = coor,
    floating = list(
      main_nodes = main_nodes,
      visible_vertices = sort(c(main_nodes, representative_nodes)),
      groups = groups,
      virtual_segments = dplyr::bind_rows(purrr::map(groups, "segment")),
      bracket_segments = bracket_segments
    )
  )
}

.component_residue_coordinates <- function(structure, nodes, layout = "SNFG") {
  component <- igraph::induced_subgraph(structure, vids = nodes)
  component_coor <- .calculate_residue_coordinates(component, layout)
  component_nodes <- as.integer(igraph::V(component)$name)
  component_coor[match(nodes, component_nodes), , drop = FALSE]
}

.floating_part_topology <- function(structure, part) {
  subtree <- .floating_subtree_topology(structure, part$root)
  list(
    signature = paste(part$linkage, subtree$signature, sep = "|"),
    nodes = subtree$nodes
  )
}

.floating_subtree_topology <- function(structure, vertex) {
  children <- as.integer(igraph::neighbors(structure, vertex, mode = "out"))
  child_topologies <- purrr::map(children, function(child) {
    edge <- igraph::get_edge_ids(structure, c(vertex, child), directed = TRUE)
    subtree <- .floating_subtree_topology(structure, child)
    list(
      signature = paste(
        igraph::E(structure)[edge]$linkage,
        subtree$signature,
        sep = ":"
      ),
      nodes = subtree$nodes,
      root = child
    )
  })
  child_order <- order(
    purrr::map_chr(child_topologies, "signature"),
    purrr::map_int(child_topologies, "root")
  )
  child_topologies <- child_topologies[child_order]

  list(
    signature = paste0(
      igraph::V(structure)[vertex]$mono,
      "{",
      igraph::V(structure)[vertex]$sub,
      "}[",
      paste(purrr::map_chr(child_topologies, "signature"), collapse = ","),
      "]"
    ),
    nodes = c(
      as.integer(vertex),
      unlist(purrr::map(child_topologies, "nodes"), use.names = FALSE)
    )
  )
}

.floating_group_y_offsets <- function(groups, main_y) {
  offsets <- numeric(length(groups))
  placed_min <- Inf
  for (i in seq_along(groups)) {
    local_y <- groups[[i]]$local_coor[, "y"]
    if (i == 1) {
      offsets[[i]] <- 0
    } else {
      offsets[[i]] <- placed_min -
        .floating_component_gap -
        max(local_y)
    }
    placed_min <- min(placed_min, min(local_y + offsets[[i]]))
  }

  floating_y <- unlist(purrr::map2(
    groups,
    offsets,
    \(.group, .offset) .group$local_coor[, "y"] + .offset
  ))
  offsets + mean(range(main_y)) - mean(range(floating_y))
}

.orient_cartoon_layout <- function(
  layout,
  orient = c("left", "right", "up", "down")
) {
  orient <- rlang::arg_match(orient)
  if (orient == "left") {
    return(layout)
  }

  layout$coor <- .rotate_cartoon_coordinates(layout$coor, orient)
  if (is.null(layout$floating)) {
    return(layout)
  }

  layout$floating$virtual_segments <- .rotate_cartoon_segments(
    layout$floating$virtual_segments,
    orient
  )
  layout$floating$bracket_segments <- .rotate_cartoon_segments(
    layout$floating$bracket_segments,
    orient
  )
  layout$floating$groups <- purrr::map(
    layout$floating$groups,
    function(group) {
      count <- .rotate_cartoon_coordinates(
        matrix(
          c(group$count_x, group$count_y),
          nrow = 1,
          dimnames = list(NULL, c("x", "y"))
        ),
        orient
      )
      group$count_x <- count[[1, "x"]]
      group$count_y <- count[[1, "y"]]
      group$segment <- .rotate_cartoon_segments(group$segment, orient)
      group
    }
  )
  layout
}

.rotate_cartoon_segments <- function(segments, orient) {
  if (nrow(segments) == 0) {
    return(segments)
  }
  starts <- .rotate_cartoon_coordinates(
    as.matrix(segments[, c("start_x", "start_y")]) |>
      `colnames<-`(c("x", "y")),
    orient
  )
  ends <- .rotate_cartoon_coordinates(
    as.matrix(segments[, c("end_x", "end_y")]) |>
      `colnames<-`(c("x", "y")),
    orient
  )
  rotated <- segments
  rotated$start_x <- starts[, "x"]
  rotated$start_y <- starts[, "y"]
  rotated$end_x <- ends[, "x"]
  rotated$end_y <- ends[, "y"]
  rotated
}

.merge_floating_highlights <- function(highlight, floating) {
  if (is.null(highlight) || is.null(floating)) {
    return(highlight)
  }

  representative_highlights <- unlist(
    purrr::map(floating$groups, function(group) {
      unlist(
        purrr::map(
          group$member_node_maps,
          function(node_map) {
            highlighted_nodes <- intersect(
              names(node_map),
              as.character(highlight)
            )
            unname(node_map[highlighted_nodes])
          }
        ),
        use.names = FALSE
      )
    }),
    use.names = FALSE
  )
  sort(unique(c(highlight, representative_highlights)))
}
