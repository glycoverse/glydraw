# Changelog

## glydraw (development version)

## glydraw 0.6.3

CRAN release: 2026-07-14

## glydraw 0.6.2

### Minor improvements and bug fixes

- Fix nested Fuc-like side-chain layout so xyloglucan-like branches
  preserve residue order and linkage annotations.
  ([\#51](https://github.com/glycoverse/glydraw/issues/51))

## glydraw 0.6.1

### Minor improvements and bug fixes

- Fix Fuc-like triangle geometry so triangle bases and apexes align with
  rectangle node bounds.
  ([\#49](https://github.com/glycoverse/glydraw/issues/49),
  [\#50](https://github.com/glycoverse/glydraw/issues/50))

## glydraw 0.6.0

### Breaking changes

- [`export_cartoons()`](https://glycoverse.github.io/glydraw/dev/reference/export_cartoons.md)
  no longer supports
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  input, and `glyexp` is no longer a package dependency.
  ([\#33](https://github.com/glycoverse/glydraw/issues/33))
- Specifying optional arguments in a positional manner is no longer
  supported. Please use `arg = value` instead.
  ([\#40](https://github.com/glycoverse/glydraw/issues/40))

### New features

- Add the `colors` parameter to
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md)
  and
  [`export_cartoons()`](https://glycoverse.github.io/glydraw/dev/reference/export_cartoons.md)
  for customizing monosaccharide fill colors.
  ([\#44](https://github.com/glycoverse/glydraw/issues/44))
- Add the `fuc_orient` parameter to
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md)
  and
  [`export_cartoons()`](https://glycoverse.github.io/glydraw/dev/reference/export_cartoons.md)
  for choosing whether Fuc triangles always point upward or flex toward
  their linkage direction.
  ([\#42](https://github.com/glycoverse/glydraw/issues/42))
- Add the `scale` parameter to
  [`save_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/save_cartoon.md)
  and
  [`export_cartoons()`](https://glycoverse.github.io/glydraw/dev/reference/export_cartoons.md)
  for changing output pixel dimensions while preserving cartoon
  appearance. ([\#35](https://github.com/glycoverse/glydraw/issues/35))
- Add the `node_size` parameter to
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md)
  and
  [`export_cartoons()`](https://glycoverse.github.io/glydraw/dev/reference/export_cartoons.md)
  for scaling residue cartoon sizes. Values larger than `2` are rejected
  because residues overlap.
  ([\#36](https://github.com/glycoverse/glydraw/issues/36))
- Add the `edge_linewidth` and `node_linewidth` parameters to
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md)
  and
  [`export_cartoons()`](https://glycoverse.github.io/glydraw/dev/reference/export_cartoons.md)
  for customizing linkage line and node border widths.
  ([\#34](https://github.com/glycoverse/glydraw/issues/34))
- Add the `red_end` parameter to
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md)
  and
  [`export_cartoons()`](https://glycoverse.github.io/glydraw/dev/reference/export_cartoons.md)
  for custom reducing-end text or a wavy reducing-end annotation.
  ([\#31](https://github.com/glycoverse/glydraw/issues/31))

### Minor improvements and bug fixes

- Fix substituent label alignment so horizontal labels are
  bottom-aligned and vertical labels are left-aligned.
  ([\#48](https://github.com/glycoverse/glydraw/issues/48))
- Extend Fuc-style branch layout and flexible orientation to additional
  Fuc-like residues.
  ([\#46](https://github.com/glycoverse/glydraw/issues/46))
- Fix
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md)
  for structures with two core Fuc branches and one b1-4 GlcNAc branch,
  avoiding an igraph vertex-selection error and keeping the b1-4 GlcNAc
  aligned with the core GlcNAc.
  ([\#45](https://github.com/glycoverse/glydraw/issues/45))
- Deprecate `dpi` for
  [`save_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/save_cartoon.md)
  and
  [`export_cartoons()`](https://glycoverse.github.io/glydraw/dev/reference/export_cartoons.md)
  because glydraw uses an internal fixed design scale. Supplying `dpi`
  now warns that the argument is ignored.
  ([\#35](https://github.com/glycoverse/glydraw/issues/35))
- Keep substituent annotations visible when `show_linkage = FALSE`.
  ([\#41](https://github.com/glycoverse/glydraw/issues/41))
- Fix diagonal HexNAc linkage annotation offsets when `orient = "V"`.
  ([\#43](https://github.com/glycoverse/glydraw/issues/43))
- Adjust linkage annotation offsets for diagonal HexNAc links.
  ([\#29](https://github.com/glycoverse/glydraw/issues/29))

## glydraw 0.5.1

### Minor improvements and bug fixes

- Use SNFG standard colors. (8573759)
- Minor aesthetic adjustments to line width and node size. (427e8d0,
  4466f88)

## glydraw 0.5.0

### New features

- Use native ggplot2 theme to manage cartoon size, so `ggview` is no
  longer dependent.
  ([\#27](https://github.com/glycoverse/glydraw/issues/27))

### Minor improvements and bug fixes

- Fix overlapping linkage annotations for some glycans.
  ([\#21](https://github.com/glycoverse/glydraw/issues/21))
- Fix overlapping a1-3 and a1-6 core Fucose residues.
  ([\#23](https://github.com/glycoverse/glydraw/issues/23))
- Orient reducing end annotation line vertically when `orient = "V"`.
  ([\#24](https://github.com/glycoverse/glydraw/issues/24))
- Redesign the node coordination layout algorithm to fix inaccurate
  branch spacing for some glycans.
  ([\#26](https://github.com/glycoverse/glydraw/issues/26),
  [\#28](https://github.com/glycoverse/glydraw/issues/28))

## glydraw 0.4.1

### Minor improvements and bug fixes

- Fix substituent annotations in glycan cartoons.
  ([\#16](https://github.com/glycoverse/glydraw/issues/16))
- Fix reducing-end Fuc orientation in O-Fuc glycans.
  ([\#15](https://github.com/glycoverse/glydraw/issues/15))
- Make
  [`export_cartoons()`](https://glycoverse.github.io/glydraw/dev/reference/export_cartoons.md)
  create the output directory when needed.
  ([\#17](https://github.com/glycoverse/glydraw/issues/17))
- Make
  [`export_cartoons()`](https://glycoverse.github.io/glydraw/dev/reference/export_cartoons.md)
  use vector names as output filenames when available.
  ([\#17](https://github.com/glycoverse/glydraw/issues/17))

## glydraw 0.4.0

### Breaking changes

- Remove the `mono_size` parameter of
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md).
  (9b05e6c)

### New features

- Add a `highlight` parameter to
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md).
  (7e57a54, 3745ec8, c1057ba)
- Add
  [`export_cartoons()`](https://glycoverse.github.io/glydraw/dev/reference/export_cartoons.md)
  to save multiple glycan structure cartoons to files. (e1ac3c3)

### Minor improvements and bug fixes

- Fix the bug that reducing end annotation was not shown with unknown
  anomer information.
  ([\#12](https://github.com/glycoverse/glydraw/issues/12))
- Fix the bug that glycans with only one monosaccharide could not be
  plotted. ([\#11](https://github.com/glycoverse/glydraw/issues/11))
- Fix the bug that “?” in linkages was not correctly handled.
  ([\#9](https://github.com/glycoverse/glydraw/issues/9))
- Fix the incorrect reducing end annotation direction in some
  situations. ([\#8](https://github.com/glycoverse/glydraw/issues/8))
- Fix the bug that glycan structures with multiple branches had
  incorrect layouts. (2ac4e9f)
- Fix the bug that “png” is not supported in
  [`save_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/save_cartoon.md).
  (fed89b9)
- Fix the bug that structures with generic “NeuAc” and “NeuGc” cannot be
  plotted. (3b78303)

## glydraw 0.3.1

### Minor improvements and bug fixes

- Update dependency to glyrepr 0.10.0.
- Fix various bugs including illegal character processing, fucose
  arrangement, and display sizing issues.

## glydraw 0.3.0

### Breaking changes

- Remove the `border_px` and `path` parameters from
  [`save_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/save_cartoon.md).

### Minor improvements and bug fixes

- Now we use `ggview` instead of `ggimage` for size fixing, which relies
  on
  [`rstudioapi::viewer()`](https://rstudio.github.io/rstudioapi/reference/viewer.html)
  to display the plot. You might feel some difference in the display
  effect, but the SNFG itself keeps the same.

## glydraw 0.2.0

### Breaking changes

- Signature of
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md)
  is changed into
  `draw_cartoon(structure, mono_size = 0.2, show_linkage = TRUE, orient = "H", ...)`
  for better semantics.

### New features

- [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md)
  now prints fixed-size cartoon (sizes are calculated based on glycan
  structures) to the plot panel, to provide a unified behavior with
  [`save_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/save_cartoon.md).
- Add reducing end annotation to the cartoons (including a short segment
  and the anomer of the reducing end residue).

### Minor improvements and bug fixes

- Update default aesthetic settings:
  - Make residue size larger.
  - Adjust linkage annotation positions.
  - Add a border to the plot panel.
  - Fix the issue that some linkage annotations are truncated.
- Fix the bug that glycans with only one residue cannot be plotted by
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md).
- Fix the bug that `path` parameter is mandatory in
  [`save_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/save_cartoon.md).
  Now it is optional, to be in line with
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
- Update documentation of
  [`save_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/save_cartoon.md)
  to differentiate it with
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## glydraw 0.1.0

- First GitHub release.
