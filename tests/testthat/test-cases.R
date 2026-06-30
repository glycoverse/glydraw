skip_on_ci()

test_that("Draw G97345NY", {
  glycan <- "Neu5Ac(a2-3)Gal(b1-3)[Neu5Ac(a2-3)Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-6)]GalNAc(a1-"
  vdiffr::expect_doppelganger("G97345NY", draw_cartoon(glycan))
})

test_that("Draw G77550KK", {
  glycan <- "Neu5Ac(a2-3)Gal(b1-3)[Fuc(a1-2)Gal(b1-3)[Fuc(a1-4)]GlcNAc(b1-3)[Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-6)]Gal(b1-4)GlcNAc(b1-6)]GalNAc(a1-"
  vdiffr::expect_doppelganger("G77550KK", draw_cartoon(glycan))
})

test_that("Draw G69233PF", {
  glycan <- "GalNAc(a1-3)[Fuc(a1-2)]Gal(b1-3)GlcNAc(b1-3)[GalNAc(a1-3)[Neu5Ac(a2-6)]Gal(b1-3)GlcNAc(b1-6)]Gal(b1-4)GlcNAc(b1-3)[Neu5Ac(a2-6)]GalNAc(a1-"
  vdiffr::expect_doppelganger("G69233PF", draw_cartoon(glycan))
})

test_that("Draw G59658KK", {
  glycan <- "Neu5Ac(a2-3)Gal(b1-3)[Fuc(a1-2)Gal(b1-3)[Fuc(a1-4)]GlcNAc(b1-3)[Gal(b1-4)GlcNAc(b1-6)]Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-6)]GalNAc(a1-"
  vdiffr::expect_doppelganger("G59658KK", draw_cartoon(glycan))
})

test_that("Draw G13863XN", {
  glycan <- "Neu5Ac(a2-3)Gal(b1-3)[Fuc(a1-2)Gal(b1-3)[Fuc(a1-4)]GlcNAc(b1-3)Gal(b1-4)GlcNAc(b1-3)Gal(b1-4)GlcNAc(b1-6)]GalNAc(a1-"
  vdiffr::expect_doppelganger("G13863XN", draw_cartoon(glycan))
})

test_that("Draw G90542MP", {
  glycan <- "Neu5Ac(a2-6)Gal(b1-4)GlcNAc(b1-2)[Neu5Ac(a2-3)[GalNAc(b1-4)]Gal(b1-4)GlcNAc(b1-4)]Man(a1-3)[Neu5Ac(a2-3)[GalNAc(b1-4)]Gal(b1-4)GlcNAc(b1-2)[Neu5Ac(a2-3)[GalNAc(b1-4)]Gal(b1-4)GlcNAc(b1-6)]Man(a1-6)]Man(b1-4)GlcNAc(b1-4)[Fuc(a1-6)]GlcNAc(b1-"
  vdiffr::expect_doppelganger("G90542MP", draw_cartoon(glycan))
})

test_that("Draw crowded branch linkage annotations", {
  glycan <- "Fuc(a1-2)Gal(b1-3)GlcNAc(b1-3)[Neu5Ac(a2-3)Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-6)]GalNAc(?1-"
  vdiffr::expect_doppelganger(
    "crowded branch linkage annotations",
    draw_cartoon(glycan)
  )
})

test_that("Draw core Fuc branch sides", {
  glycan <- "Fuc(a1-3)[Fuc(a1-6)]GlcNAc(b1-4)GlcNAc(b1-"
  vdiffr::expect_doppelganger("core Fuc branch sides", draw_cartoon(glycan))
})

test_that("Draw three-way branch with two Fuc leaves", {
  glycan <- "Fuc(a1-3)[Fuc(a1-6)][GlcNAc(b1-4)]GlcNAc(b1-"
  vdiffr::expect_doppelganger(
    "three-way branch with two Fuc leaves",
    draw_cartoon(glycan)
  )
})

test_that("Draw Fuc triangle orientation controls", {
  glycan <- "Fuc(a1-3)[Fuc(a1-6)]GlcNAc(b1-4)GlcNAc(b1-"

  vdiffr::expect_doppelganger(
    "flexible Fuc triangle orientation",
    draw_cartoon(glycan, fuc_orient = "flex")
  )
  vdiffr::expect_doppelganger(
    "upward Fuc triangle orientation",
    draw_cartoon(glycan, fuc_orient = "up")
  )
})

test_that("Draw vertical flexible Fuc triangles", {
  glycan <- "Fuc(a1-3)[Fuc(a1-6)]GlcNAc(b1-4)GlcNAc(b1-"
  vdiffr::expect_doppelganger(
    "vertical flexible Fuc triangles",
    draw_cartoon(glycan, orient = "V", fuc_orient = "flex")
  )
})

test_that("Draw vertical reducing-end Fuc triangle", {
  glycan <- "GlcNAc(b1-3)Fuc(a1-"
  vdiffr::expect_doppelganger(
    "vertical reducing-end Fuc triangle",
    draw_cartoon(glycan, orient = "V", fuc_orient = "flex")
  )
})

test_that("Draw elongated Fuc branches together", {
  glycan <- paste0(
    "WURCS=2.0/6,7,6/",
    "[a2122h-1a_1-5_2*NCC/3=O][a1221m-1a_1-5]",
    "[a2122h-1b_1-5_2*NCC/3=O][a2112h-1b_1-5]",
    "[a1122h-1b_1-5][a1122h-1a_1-5]/",
    "1-2-3-2-4-5-6/",
    "a3-b1_a4-c1_c3-d1_c4-f1_d4-e1_f3-g1"
  )
  vdiffr::expect_doppelganger(
    "elongated Fuc branches together",
    draw_cartoon(glycan)
  )
})

test_that("Draw nested branches next to leaf siblings", {
  short_glycan <- "Man(a1-3)[Man(a1-6)]Man(a1-6)[Man(a1-3)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  elongated_glycan <- "Man(a1-3)[Man(a1-6)]Man(a1-6)[Man(a1-2)Man(a1-3)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"

  vdiffr::expect_doppelganger(
    "short nested branches next to leaf siblings",
    draw_cartoon(short_glycan)
  )
  vdiffr::expect_doppelganger(
    "elongated nested branches next to leaf siblings",
    draw_cartoon(elongated_glycan)
  )
})

test_that("Draw elongated and leaf sibling branches", {
  glycan <- "Neu5Ac(a2-3)Gal(b1-3)[Gal(b1-3)GlcNAc(b1-3)[Gal(b1-4)GlcNAc(b1-6)]Gal(b1-4)GlcNAc(b1-6)]GalNAc(a1-"
  vdiffr::expect_doppelganger(
    "elongated and leaf sibling branches",
    draw_cartoon(glycan)
  )
})

test_that("Draw same-depth Man sibling branches", {
  glycan <- "Man(a1-2)Man(a1-3)[Man(a1-3)[Man(a1-2)Man(a1-6)]Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  vdiffr::expect_doppelganger(
    "same-depth Man sibling branches",
    draw_cartoon(glycan)
  )
})
