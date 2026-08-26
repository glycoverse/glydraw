floating_iupac_examples <- function() {
  c(
    multiple_parts = paste0(
      "{Fuc(a1-3)[Gal(b1-4)]GlcNAc(b1-?)|10,11,12,13,14,15}",
      "{Gal(b1-4)GlcNAc(b1-?)|10,11,12,13,14,15}",
      "{Neu5Ac(a2-?)Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-?)|",
      "10,11,12,13,14,15}",
      "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)",
      "[Fuc(a1-6)]GlcNAc(b1-"
    ),
    implicit_part = paste0(
      "{Neu5Ac(a2-6)}Gal(b1-4)GlcNAc(b1-2)Man(a1-3)",
      "[GlcNAc(b1-4)][Gal(b1-4)GlcNAc(b1-2)Man(a1-6)]",
      "Man(b1-4)GlcNAc(b1-4)GlcNAc(?1-"
    ),
    explicit_part = paste0(
      "{Gal(b1-4)GlcNAc(b1-4/6)|5,7}Gal(b1-4)GlcNAc(b1-2)",
      "Man(a1-3)[GlcNAc(b1-2)Man(a1-6)]Man(b1-4)",
      "GlcNAc(b1-4)[Fuc(a1-6)]GlcNAc(?1-"
    ),
    repeated_part = paste0(
      "{Fuc(?1-?)|4,5,6,7,8,9,10,11,12,13,14}",
      "{Fuc(?1-?)|4,5,6,7,8,9,10,11,12,13,14}",
      "{Fuc(?1-?)|4,5,6,7,8,9,10,11,12,13,14}",
      "Gal(?1-?)GlcNAc(?1-?)Man(?1-?)",
      "[Gal(?1-?)GlcNAc(?1-?)[Gal(?1-?)GlcNAc(?1-?)]Man(?1-?)]",
      "Man(?1-?)GlcNAc(?1-?)GlcNAc(?1-"
    )
  )
}
