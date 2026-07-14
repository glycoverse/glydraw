# ggplot2 compatibility methods for list-backed glycan structure vectors.

#' Identify glycan structures as discrete ggplot2 values
#'
#' @param x A `glyrepr_structure` vector.
#'
#' @returns The scale-type candidates `c("discrete", "identity")`. Known
#'   discrete aesthetics such as color and fill use their discrete scales,
#'   while custom aesthetics such as `structure` remain unchanged.
#' @noRd
#' @exportS3Method ggplot2::scale_type
scale_type.glyrepr_structure <- function(x) {
  c("discrete", "identity")
}

#' Report glycan structure values as discrete levels
#'
#' `scales` uses `levels()` to recognize non-character discrete vectors. Glycan
#' structure levels are derived from their character representation so ordinary
#' discrete color and fill scales can train and map them.
#'
#' @param x A `glyrepr_structure` vector.
#'
#' @returns A sorted character vector of unique, non-missing structures.
#' @noRd
#' @exportS3Method base::levels
levels.glyrepr_structure <- function(x) {
  structures <- as.character(x)
  sort(unique(structures[!is.na(structures)]))
}

#' Drop unused glycan structure levels
#'
#' Glycan structure levels are derived directly from the values, so there are
#' no separately stored levels to drop.
#'
#' @param x A `glyrepr_structure` vector.
#' @param ... Unused.
#'
#' @returns `x` unchanged.
#' @noRd
#' @exportS3Method base::droplevels
droplevels.glyrepr_structure <- function(x, ...) {
  x
}

#' Normalize glycan structures before position-scale training
#'
#' @param self A ggplot2 discrete position scale.
#' @param x Values supplied to the scale transformation.
#'
#' @returns Glycan structure vectors as character vectors; all other inputs are
#'   returned unchanged.
#' @noRd
.transform_glycan_position <- function(self, x) {
  if (glyrepr::is_glycan_structure(x)) {
    return(as.character(x))
  }
  x
}
