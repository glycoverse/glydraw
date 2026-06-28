# Internal input-validation and coercion helpers shared by drawing, saving, and
# export workflows.

#' Validate highlight vertex indices
#'
#' @param x `NULL` or a numeric vector of 1-based vertex indices.
#' @param gly_vertices Integer number of vertices in the glycan graph.
#'
#' @return `x` unchanged when it is `NULL` or all values are within
#'   `seq_len(gly_vertices)`. Throws a cli error for non-numeric or out-of-range
#'   values.
#' @noRd
.validate_highlight_indices <- function(x, gly_vertices) {
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
#' @param x A ggplot object, usually with class `glydraw_cartoon`.
#'
#' @return The same plot object with `glydraw_cartoon` removed from its class
#'   vector.
#' @noRd
.strip_cartoon_class <- function(x) {
  class(x) <- setdiff(class(x), "glydraw_cartoon")
  x
}

#' Convert supported input to glycan-structure data
#'
#' @param x A `glyrepr::glycan_structure()` vector or a character vector of
#'   structure strings accepted by `glyparse::auto_parse()`.
#'
#' @return A `glyrepr::glycan_structure()` vector with the same logical length
#'   as `x`. Throws a cli error for unsupported input classes.
#' @noRd
.as_glycan_structure_input <- function(x) {
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

#' Convert input to exactly one glycan structure
#'
#' @param x A `glyrepr::glycan_structure()` scalar or scalar character
#'   structure string.
#'
#' @return A scalar `glyrepr::glycan_structure()` object. Throws a cli error
#'   when the converted input length is not exactly one.
#' @noRd
.as_single_glycan_structure <- function(x) {
  x <- .as_glycan_structure_input(x)
  if (length(x) > 1) {
    cli::cli_abort(c(
      "Must provide exactly one glycan structure.",
      "x" = "Get length: {.val {length(x)}}."
    ))
  }
  x
}
