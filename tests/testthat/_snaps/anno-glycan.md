# anno_glycan validates structures, sides, and anchoring

    Code
      anno_glycan(character(), which = "column")
    Condition
      Error in `.validate_glycan_annotation_structures()`:
      ! `structure` must contain at least one glycan.

---

    Code
      anno_glycan(structure, which = "column", side = "left")
    Condition
      Error in `.resolve_glycan_annotation_side()`:
      ! `side` must be one of "bottom" or "top", not "left".

---

    Code
      anno_glycan(structure, which = "row", hjust = hjust_red_end())
    Condition
      Error in `.validate_red_end_justification_orientation()`:
      ! `hjust_red_end()` can only be used when `orient = "V"`.
