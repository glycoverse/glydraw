# glycan axis reducing-end helpers require matching orientations

    Code
      scale_x_glycan(vjust = vjust_red_end())
    Condition
      Error in `.validate_red_end_justification_orientation()`:
      ! `vjust_red_end()` can only be used when `orient = "H"`.

---

    Code
      scale_y_glycan(hjust = hjust_red_end())
    Condition
      Error in `.validate_red_end_justification_orientation()`:
      ! `hjust_red_end()` can only be used when `orient = "V"`.

