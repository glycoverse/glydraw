# Build orthogonal subtrees in a local frame pointing left. Longest child paths
# continue straight; other children turn alternately up and down. Bounding boxes
# reserve room for nested branches without changing simple comb bond lengths.
# High-degree subtrees use depth columns and fan their child branches out.
.linear_residue_coordinates <- function(structure) {
  children <- lapply(seq_len(igraph::vcount(structure)), function(vertex) {
    as.integer(igraph::neighbors(structure, vertex, mode = "out"))
  })
  height <- integer(length(children))
  for (vertex in as.integer(igraph::topo_sort(structure, mode = "in"))) {
    if (length(children[[vertex]])) {
      height[vertex] <- 1L + max(height[children[[vertex]]])
    }
  }
  build <- function(vertex, fan = FALSE) {
    result <- matrix(c(0, 0), ncol = 2, dimnames = list(vertex, c("x", "y")))
    child <- children[[vertex]]
    if (!length(child)) {
      return(result)
    }
    edge_ids <- igraph::get_edge_ids(structure, as.vector(rbind(vertex, child)))
    linkages <- igraph::E(structure)$linkage[edge_ids]
    incoming <- igraph::incident(structure, vertex, mode = "in")
    same_linkage <- if (length(incoming)) {
      linkages == igraph::E(structure)$linkage[incoming[1]]
    } else {
      rep(FALSE, length(child))
    }
    same_residue <- igraph::V(structure)$mono[child] ==
      igraph::V(structure)$mono[vertex]
    child <- child[order(-height[child], -same_linkage, -same_residue, child)]
    sides <- child[-1]
    if (fan || length(child) > 3L) {
      # A depth column per generation prevents edges from passing through
      # unrelated residues. Keep the longest continuation at y = 0 and place
      # sibling subtrees in disjoint vertical bands, using diagonal links.
      continuation <- build(child[1], fan = TRUE)
      continuation[, "x"] <- continuation[, "x"] - 1
      result <- rbind(result, continuation)
      for (i in seq_along(sides)) {
        branch <- build(sides[i], fan = TRUE)
        direction <- if (i %% 2L) 1 else -1
        offset <- max(result[, "y"] * direction) +
          1 -
          min(branch[, "y"] * direction)
        branch[, "x"] <- branch[, "x"] - 1
        branch[, "y"] <- branch[, "y"] + direction * offset
        result <- rbind(result, branch)
      }
      return(result)
    }
    for (i in seq_along(sides)) {
      branch <- build(sides[i])
      direction <- if (i %% 2L) 1 else -1
      branch[,] <- cbind(x = branch[, "y"], y = -branch[, "x"] * direction)
      occupied <- result[, "y"] * direction
      offset <- max(occupied) + 1 - min(branch[, "y"] * direction)
      branch[, "y"] <- branch[, "y"] + direction * offset
      result <- rbind(result, branch)
    }
    continuation <- build(child[1])
    # Separate bounding boxes horizontally, keeping the backbone on y = 0.
    continuation[, "x"] <- continuation[, "x"] +
      min(result[, "x"]) -
      1 -
      max(continuation[, "x"])
    rbind(result, continuation)
  }
  coor <- build(igraph::vcount(structure))
  coor <- coor[
    match(seq_len(igraph::vcount(structure)), rownames(coor)),
    ,
    drop = FALSE
  ]
  rownames(coor) <- NULL
  coor
}
