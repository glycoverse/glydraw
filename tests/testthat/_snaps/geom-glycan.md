# geom_glycan rejects alpha on incapable graphics devices

    Code
      .validate_cartoon_alpha_device(0.5, capabilities)
    Condition
      Error in `.validate_cartoon_alpha_device()`:
      ! The active graphics device does not support the alpha aesthetic.
      i Use a device that supports alpha masks and transformations, such as `grDevices::pdf()` or `grDevices::svg()`.

# reducing-end justification helpers require matching orientations

    Code
      geom_glycan(orient = "left", hjust = hjust_red_end())
    Condition
      Error in `.validate_red_end_justification_orientation()`:
      ! `hjust_red_end()` can only be used when `orient` is `"up"` or `"down"`.

---

    Code
      geom_glycan(orient = "up", vjust = vjust_red_end())
    Condition
      Error in `.validate_red_end_justification_orientation()`:
      ! `vjust_red_end()` can only be used when `orient` is `"left"` or `"right"`.

