# Changelog

## glydraw (development version)

## glydraw 0.3.0

### Breaking changes

- Remove the `border_px` and `path` parameters from
  [`save_cartoon()`](https://glycoverse.github.io/glydraw/reference/save_cartoon.md).

### Minor improvements and bug fixes

- Now we use `ggview` instead of `ggimage` for size fixing, which relies
  on
  [`rstudioapi::viewer()`](https://rstudio.github.io/rstudioapi/reference/viewer.html)
  to display the plot. You might feel some difference in the display
  effect, but the SNFG itself keeps the same.

## glydraw 0.2.0

### Breaking changes

- Signature of
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/reference/draw_cartoon.md)
  is changed into
  `draw_cartoon(structure, mono_size = 0.2, show_linkage = TRUE, orient = "H", ...)`
  for better semantics.

### New features

- [`draw_cartoon()`](https://glycoverse.github.io/glydraw/reference/draw_cartoon.md)
  now prints fixed-size cartoon (sizes are calculated based on glycan
  structures) to the plot panel, to provide a unified behavior with
  [`save_cartoon()`](https://glycoverse.github.io/glydraw/reference/save_cartoon.md).
- Add reducing end annotation to the cartoons (including a short segment
  and the anomer of the reducing end residue).

### Minor improvements and bug fixes

- Update default aesthetic settings:
  - Make residue size larger.
  - Adjust linkage annotation positions.
  - Add a border to the plot panel.
  - Fix the issue that some linkage annotations are truncated.
- Fix the bug that glycans with only one residue cannot be plotted by
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/reference/draw_cartoon.md).
- Fix the bug that `path` parameter is mandatory in
  [`save_cartoon()`](https://glycoverse.github.io/glydraw/reference/save_cartoon.md).
  Now it is optional, to be in line with
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
- Update documentation of
  [`save_cartoon()`](https://glycoverse.github.io/glydraw/reference/save_cartoon.md)
  to differentiate it with
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## glydraw 0.1.0

- First GitHub release.
