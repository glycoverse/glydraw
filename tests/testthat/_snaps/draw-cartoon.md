# style constructors reject a NULL red_end

    Code
      style_glydraw(red_end = NULL)
    Condition
      Error in `.make_glydraw_style()`:
      ! `red_end` in a glycan style cannot be `NULL`.
      i Set `red_end_length` to 0 to omit the reducing-end line and `red_end` decoration while retaining the anomer annotation.

# draw_cartoon rejects the previous orientation abbreviations

    Code
      draw_cartoon("Gal(b1-3)GalNAc(a1-", orient = "H")
    Condition
      Error:
      ! `orient` must be one of "left", "right", "up", or "down", not "H".

