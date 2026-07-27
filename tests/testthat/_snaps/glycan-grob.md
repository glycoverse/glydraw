# glycanGrob revalidates reusable styles before drawing

    Code
      glycanGrob("Gal(b1-4)GlcNAc(b1-", style = style)
    Condition
      Error in `.validate_colors()`:
      ! `colors` must have exactly the names returned by `glydraw_colors()`.

---

    Code
      glycanGrob("Gal(b1-4)GlcNAc(b1-", style = style)
    Condition
      Error in `.validate_node_size()`:
      ! `node_size` must be no larger than 2 because larger values make residues overlap.

