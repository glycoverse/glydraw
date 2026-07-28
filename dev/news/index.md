# Changelog

## glydraw (development version)

### Breaking changes

- `glydraw_style()` has been renamed to
  [`style_glydraw()`](https://glycoverse.github.io/glydraw/dev/reference/style_glydraw.md)
  and is no longer exported; replace calls to `glydraw_style()` with
  [`style_glydraw()`](https://glycoverse.github.io/glydraw/dev/reference/style_glydraw.md).
  ([\#73](https://github.com/glycoverse/glydraw/issues/73))

- Cartoon appearance is now configured with `style = style_glydraw(...)`
  across
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md),
  [`glycanGrob()`](https://glycoverse.github.io/glydraw/dev/reference/glycanGrob.md),
  [`geom_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/geom_glycan.md),
  [`guide_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/guide_glycan.md),
  [`scale_x_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/scale_x_glycan.md),
  [`scale_y_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/scale_x_glycan.md),
  [`anno_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/anno_glycan.md),
  and
  [`export_cartoons()`](https://glycoverse.github.io/glydraw/dev/reference/export_cartoons.md);
  calls that pass `fuc_orient`, `edge_linewidth`, `node_linewidth`,
  `node_size`, `font_family`, or `colors` directly should move those
  arguments into
  [`style_glydraw()`](https://glycoverse.github.io/glydraw/dev/reference/style_glydraw.md).
  `show_linkage` and `orient` remain explicit arguments and are no
  longer accepted by
  [`style_glydraw()`](https://glycoverse.github.io/glydraw/dev/reference/style_glydraw.md),
  while explicit `red_end` overrides the reusable style value.
  ([\#73](https://github.com/glycoverse/glydraw/issues/73),
  [\#79](https://github.com/glycoverse/glydraw/issues/79))

- The shared `orient` argument no longer accepts `"H"` or `"V"`; calls
  using them now error and should replace `"H"` with `"left"` and `"V"`
  with `"up"`. ([\#68](https://github.com/glycoverse/glydraw/issues/68))

### New features

- New
  [`draw_cartoon_sketch()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon_sketch.md)
  draws hand-sketched glycan cartoons with reproducible rough strokes,
  patterned residue fills, optional drawing media, and the same layout,
  annotation, orientation, highlighting, styling, sizing, and saving
  behavior as
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md).
  ([\#77](https://github.com/glycoverse/glydraw/issues/77))

- [`style_glydraw()`](https://glycoverse.github.io/glydraw/dev/reference/style_glydraw.md)
  replaces `glydraw_style()` as the single reusable interface for tuning
  cartoon appearance; `show_linkage` and `orient` are explicit drawing
  controls instead of style fields, and explicit `red_end = NULL`
  inherits the style value. New
  [`style_glygen()`](https://glycoverse.github.io/glydraw/dev/reference/style_glydraw.md),
  [`style_snfg()`](https://glycoverse.github.io/glydraw/dev/reference/style_glydraw.md),
  and
  [`style_glycoworkbench()`](https://glycoverse.github.io/glydraw/dev/reference/style_glydraw.md)
  provide reusable presets for common glycan-drawing conventions.
  ([\#73](https://github.com/glycoverse/glydraw/issues/73),
  [\#79](https://github.com/glycoverse/glydraw/issues/79))

- New `font_family` style option controls the font used for text
  annotations across glycan cartoons, including alpha and beta anomer
  labels. ([\#69](https://github.com/glycoverse/glydraw/issues/69))

- The shared `orient` argument now accepts `"left"`, `"right"`, `"up"`,
  and `"down"` to draw glycans in any direction.

- New
  [`anno_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/anno_glycan.md)
  uses glycan cartoons as row or column labels in ComplexHeatmap
  heatmaps, with clustering-aware ordering and the sizing, anchoring,
  rotation, nudging, and styling controls of glycan axis scales.
  ([\#67](https://github.com/glycoverse/glydraw/issues/67))

- Glycan grobs used by
  [`geom_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/geom_glycan.md),
  [`geom_node_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/geom_node_glycan.md),
  [`guide_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/guide_glycan.md),
  [`scale_x_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/scale_x_glycan.md),
  and
  [`scale_y_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/scale_x_glycan.md)
  now remain vector graphics at every size when exported to PDF or SVG;
  [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md)
  and repeated panel structures also render more efficiently with native
  grid primitives without changing cartoon layout or appearance.
  ([\#64](https://github.com/glycoverse/glydraw/issues/64),
  [\#65](https://github.com/glycoverse/glydraw/issues/65),
  [\#66](https://github.com/glycoverse/glydraw/issues/66))

### Minor improvements and bug fixes

- `Hex` residues now render as smooth, device-native circles across
  standalone cartoons and embedded glycan grobs.
  ([\#76](https://github.com/glycoverse/glydraw/issues/76))

- Linkage-annotation collision handling now ignores the two labels
  belonging to the same edge, preventing enlarged nodes from
  unnecessarily reflecting an otherwise collision-free linkage.
  ([\#75](https://github.com/glycoverse/glydraw/issues/75))

- `style_glydraw(node_size = ...)` now distributes linkage-annotation
  spacing adjustments across the node-to-label and label-to-label gaps,
  keeping linkage notation readable with enlarged nodes.
  ([\#74](https://github.com/glycoverse/glydraw/issues/74))

- `style_glydraw(red_end = NULL)` now omits both the reducing-end line
  and its anomer annotation across glycan drawing interfaces.
  ([\#71](https://github.com/glycoverse/glydraw/issues/71))

- Move beta linkage annotations slightly away from horizontal and skewed
  edge lines while leaving labels beside vertical edges unchanged,
  including reducing-end annotations.
  ([\#70](https://github.com/glycoverse/glydraw/issues/70))

## glydraw 0.7.0

CRAN release: 2026-07-25

This version of glydraw introduced some `ggplot2` extensions.

### New features

- New `glydraw_style()` stores reusable glycan rendering options for
  standalone cartoons, grobs, export, ggplot2 layers, guides, and glycan
  scales. Explicit rendering arguments override the style.
  ([\#55](https://github.com/glycoverse/glydraw/issues/55))
- New
  [`geom_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/geom_glycan.md)
  draws glycan cartoons for individual observations in ggplot2 plots,
  with support for size, rotation, justification, and cartoon
  appearance.
- New
  [`geom_node_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/geom_node_glycan.md)
  draws glycan cartoons as nodes in ggraph network plots, with automatic
  node positioning and filtering support.
  ([\#61](https://github.com/glycoverse/glydraw/issues/61))
- New
  [`guide_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/guide_glycan.md)
  displays glycan cartoons in ggplot2 legends.
- New
  [`scale_x_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/scale_x_glycan.md)
  and
  [`scale_y_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/scale_x_glycan.md)
  display glycan cartoons as discrete ggplot2 axis labels.
- New
  [`hjust_red_end()`](https://glycoverse.github.io/glydraw/dev/reference/hjust_red_end.md)
  and
  [`vjust_red_end()`](https://glycoverse.github.io/glydraw/dev/reference/hjust_red_end.md)
  anchor vertical and horizontal cartoons at their reducing ends in
  [`geom_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/geom_glycan.md),
  [`scale_x_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/scale_x_glycan.md),
  [`scale_y_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/scale_x_glycan.md),
  and
  [`guide_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/guide_glycan.md).
  [`guide_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/guide_glycan.md)
  also gains `hjust` and `vjust` parameters. Reducing-end justification
  is the default for scale and guide cartoons along the axis
  perpendicular to their drawing orientation;
  [`geom_glycan()`](https://glycoverse.github.io/glydraw/dev/reference/geom_glycan.md)
  remains centered by default.
  ([\#54](https://github.com/glycoverse/glydraw/issues/54))

### Minor improvements and bug fixes

- [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md)
  now places double core Fuc branches on opposite sides of the core
  GlcNAc when linkage positions are unavailable, keeping both
  connections visible.
  ([\#59](https://github.com/glycoverse/glydraw/issues/59),
  [\#60](https://github.com/glycoverse/glydraw/issues/60))
- Recognize bisecting GlcNAc from N-glycan topology when linkage
  information is unavailable, keeping it centered between the two
  mannose arms.
  ([\#58](https://github.com/glycoverse/glydraw/issues/58))
- [`draw_cartoon()`](https://glycoverse.github.io/glydraw/dev/reference/draw_cartoon.md)
  now treats generic `dHex` residues as Fuc-like branches, using the
  same layout and `fuc_orient` behavior as Fuc.
  ([\#56](https://github.com/glycoverse/glydraw/issues/56))

## glydraw 0.6.3

CRAN release: 2026-07-14

- First release on CRAN.

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
