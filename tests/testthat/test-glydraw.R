skip_on_ci()
skip_on_cran()

test_that("draw_cartoon handles a single residue", {
  vdiffr::expect_doppelganger("single_residue", draw_cartoon("Gal(b1-"))
})

test_that("draw_cartoon deals with sizes correctly", {
  vdiffr::expect_doppelganger("large_size", draw_cartoon("Glc(a1-3)Glc(a1-3)Glc(a1-3)Glc(a1-3)Glc(a1-3)Glc(a1-"))
  vdiffr::expect_doppelganger("small_size", draw_cartoon("Glc(a1-3)Glc(a1-"))
})

test_that("draw_cartoon can hide linkages", {
  vdiffr::expect_doppelganger("hide_linkages", draw_cartoon("Glc(a1-3)Glc(a1-", show_linkage = FALSE))
})

test_that("draw_cartoon can draw vertical glycans", {
  vdiffr::expect_doppelganger("vertical", draw_cartoon("Glc(a1-3)Glc(a1-", orient = "V"))
})