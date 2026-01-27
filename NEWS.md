# glydraw 0.3.1

## Minor improvements and bug fixes

* Update dependency to glyrepr 0.10.0.
* Fix various bugs including illegal character processing, fucose arrangement, and display sizing issues.

# glydraw 0.3.0

## Breaking changes

* Remove the `border_px` and `path` parameters from `save_cartoon()`.

## Minor improvements and bug fixes

* Now we use `ggview` instead of `ggimage` for size fixing, which relies on `rstudioapi::viewer()` to display the plot. You might feel some difference in the display effect, but the SNFG itself keeps the same.

# glydraw 0.2.0

## Breaking changes

* Signature of `draw_cartoon()` is changed into `draw_cartoon(structure, mono_size = 0.2, show_linkage = TRUE, orient = "H", ...)` for better semantics.

## New features

* `draw_cartoon()` now prints fixed-size cartoon (sizes are calculated based on glycan structures) to the plot panel, to provide a unified behavior with `save_cartoon()`.
* Add reducing end annotation to the cartoons (including a short segment and the anomer of the reducing end residue).

## Minor improvements and bug fixes

* Update default aesthetic settings:
  * Make residue size larger.
  * Adjust linkage annotation positions.
  * Add a border to the plot panel.
  * Fix the issue that some linkage annotations are truncated.
* Fix the bug that glycans with only one residue cannot be plotted by `draw_cartoon()`.
* Fix the bug that `path` parameter is mandatory in `save_cartoon()`. Now it is optional, to be in line with `ggplot2::ggsave()`.
* Update documentation of `save_cartoon()` to differentiate it with `ggplot2::ggsave()`.

# glydraw 0.1.0

* First GitHub release.
