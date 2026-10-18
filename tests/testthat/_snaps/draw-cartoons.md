# grid inputs and capacity are validated

    Code
      draw_cartoons(character())
    Condition
      Error in `draw_cartoons()`:
      ! `structure` must contain between 1 and 36 glycans; got 0.

---

    Code
      draw_cartoons(rep("Gal", 37))
    Condition
      Error in `draw_cartoons()`:
      ! `structure` must contain between 1 and 36 glycans; got 37.

---

    Code
      draw_cartoons(c("Gal", "Man"), nrow = 1, ncol = 1)
    Condition
      Error in `draw_cartoons()`:
      ! `nrow` * `ncol` must provide space for all 2 glycans.

---

    Code
      draw_cartoons("Gal", nrow = 0)
    Condition
      Error in `draw_cartoons()`:
      ! Assertion on 'nrow' failed: Must be >= 1.

---

    Code
      draw_cartoons("Gal", ncol = 1.5)
    Condition
      Error in `draw_cartoons()`:
      ! Assertion on 'ncol' failed: Must be of type 'count' (or 'NULL'), not 'double'.

---

    Code
      draw_cartoons("Gal", max_n = NA)
    Condition
      Error in `draw_cartoons()`:
      ! Assertion on 'max_n' failed: May not be NA.

---

    Code
      draw_cartoons(1)
    Condition
      Error in `draw_cartoons()`:
      ! `structure` must be a character or glycan_structure vector.

