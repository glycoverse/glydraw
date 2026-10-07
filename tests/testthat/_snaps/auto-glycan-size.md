# automatic size limits must be positive finite scalars

    Code
      auto_glycan_size(max_size = 0)
    Condition
      Error in `.validate_output_scale()`:
      ! `scale` must be larger than "0".

---

    Code
      auto_glycan_size(max_size = Inf)
    Condition
      Error in `.validate_output_scale()`:
      ! Assertion on 'scale' failed: Must be finite.

