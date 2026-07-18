# guide_glycan reducing-end helpers require matching orientations

    Code
      guide_glycan(orient = "H", hjust = hjust_red_end())
    Condition
      Error in `.validate_red_end_justification_orientation()`:
      ! `hjust_red_end()` can only be used when `orient = "V"`.

---

    Code
      guide_glycan(orient = "V", vjust = vjust_red_end())
    Condition
      Error in `.validate_red_end_justification_orientation()`:
      ! `vjust_red_end()` can only be used when `orient = "H"`.

