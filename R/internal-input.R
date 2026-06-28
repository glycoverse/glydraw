#' Validate highlight indices
#'
#' @param x Highlight vertex indices.
#' @param gly_vertices Number of vertices in the glycan structure.
#'
#' @return The validated highlight indices.
#' @noRd
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

#' Remove the glydraw cartoon class from a plot
#'
#' @param x A glydraw cartoon plot.
#'
#' @return The plot without the `glydraw_cartoon` class.
#' @noRd
.strip_glydraw_class <- function(x) {
  class(x) <- setdiff(class(x), "glydraw_cartoon")
  x
}

#' Ensure an input can be represented as a glycan structure
#'
#' @param x A glycan structure or structure string.
#'
#' @return A glycan structure object.
#' @noRd
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

#' Ensure an input contains exactly one glycan structure
#'
#' @param x A glycan structure or structure string.
#'
#' @return A scalar glycan structure object.
#' @noRd
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
