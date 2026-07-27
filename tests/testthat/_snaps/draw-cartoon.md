# cartoon styling is available only through style

    Code
      draw_cartoon("Gal(b1-4)GlcNAc(b1-", edge_linewidth = 1.2)
    Condition
      Error in `.check_no_explicit_style_arguments()`:
      ! Cartoon styling arguments must be supplied through `style`.
      i Move `edge_linewidth` argument into `glydraw_style()`.

---

    Code
      geom_glycan(node_size = 1.2)
    Condition
      Error in `.check_no_explicit_style_arguments()`:
      ! Cartoon styling arguments must be supplied through `style`.
      i Move `node_size` argument into `glydraw_style()`.

---

    Code
      scale_x_glycan(red_end = "~")
    Condition
      Error in `.check_no_explicit_style_arguments()`:
      ! Cartoon styling arguments must be supplied through `style`.
      i Move `red_end` argument into `glydraw_style()`.

# draw_cartoon rejects the previous orientation abbreviations

    Code
      draw_cartoon("Gal(b1-3)GalNAc(a1-", orient = "H")
    Condition
      Error:
      ! `orient` must be one of "left", "right", "up", or "down", not "H".
