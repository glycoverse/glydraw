# style constructors reject a NULL red_end

    Code
      style_glydraw(red_end = NULL)
    Condition
      Error in `.make_glydraw_style()`:
      ! `red_end` in a glycan style cannot be `NULL`.
      i Set `red_end_length` to 0 to omit the reducing-end line and `red_end` decoration while retaining the anomer annotation.

# tagged amino-acid sequences require exactly one site character

    Code
      style_glydraw(red_end = "ABC<site></site>EFG")
    Condition
      Error in `.parse_reducing_end_aa_sequence()`:
      ! `red_end` has an invalid amino-acid site annotation.
      i Use one `<site></site>` pair containing exactly one character, for example `ABC<site>D</site>EFG`.

---

    Code
      style_glydraw(red_end = "ABC<site>DE</site>FG")
    Condition
      Error in `.parse_reducing_end_aa_sequence()`:
      ! `red_end` has an invalid amino-acid site annotation.
      i Use one `<site></site>` pair containing exactly one character, for example `ABC<site>D</site>EFG`.

---

    Code
      style_glydraw(red_end = "ABC<site>D</site>EFG<site>H</site>I")
    Condition
      Error in `.parse_reducing_end_aa_sequence()`:
      ! `red_end` has an invalid amino-acid site annotation.
      i Use one `<site></site>` pair containing exactly one character, for example `ABC<site>D</site>EFG`.

---

    Code
      style_glydraw(red_end = "ABC<site>D")
    Condition
      Error in `.parse_reducing_end_aa_sequence()`:
      ! `red_end` has an invalid amino-acid site annotation.
      i Use one `<site></site>` pair containing exactly one character, for example `ABC<site>D</site>EFG`.

# draw_cartoon rejects the previous orientation abbreviations

    Code
      draw_cartoon("Gal(b1-3)GalNAc(a1-", orient = "H")
    Condition
      Error:
      ! `orient` must be one of "left", "right", "up", or "down", not "H".

