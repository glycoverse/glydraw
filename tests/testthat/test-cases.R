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