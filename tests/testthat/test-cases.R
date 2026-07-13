skip_on_ci()
skip_on_cran()
skip_if_not_installed("vdiffr")

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

test_that("Draw custom node size", {
  glycan <- "Gal(b1-3)GalNAc(a1-"
  vdiffr::expect_doppelganger(
    "custom node size",
    draw_cartoon(glycan, node_size = 1.2)
  )
})

test_that("Draw oversized node size with linkage hidden", {
  glycan <- "Gal(b1-3)GalNAc(a1-"

  expect_warning(
    plot <- draw_cartoon(glycan, node_size = 1.25, red_end = "Ser/Thr"),
    "Linkage annotations are hidden"
  )
  vdiffr::expect_doppelganger("oversized node size hides linkage", plot)
})

test_that("Draw orientation-specific HexNAc linkage labels", {
  glycan <- "GalNAc(b1-3)[GalNAc(b1-6)]GalNAc(a1-"

  vdiffr::expect_doppelganger(
    "horizontal HexNAc linkage labels",
    draw_cartoon(glycan, orient = "H")
  )
  vdiffr::expect_doppelganger(
    "vertical HexNAc linkage labels",
    draw_cartoon(glycan, orient = "V")
  )
})

test_that("Draw reducing-end annotations", {
  glycan <- "Gal(b1-3)GalNAc(a1-"

  vdiffr::expect_doppelganger(
    "custom reducing-end text",
    draw_cartoon(glycan, red_end = "Ser/Thr")
  )
  vdiffr::expect_doppelganger(
    "custom reducing-end text without linkage",
    draw_cartoon(glycan, show_linkage = FALSE, red_end = "Ser/Thr")
  )
  vdiffr::expect_doppelganger(
    "vertical custom reducing-end text",
    draw_cartoon(glycan, orient = "V", red_end = "Ser/Thr")
  )
  vdiffr::expect_doppelganger(
    "wavy reducing end",
    draw_cartoon(glycan, red_end = "~")
  )
})

test_that("Draw linkage-hidden branch", {
  glycan <- "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  vdiffr::expect_doppelganger(
    "linkage-hidden branch",
    draw_cartoon(glycan, show_linkage = FALSE)
  )
})

test_that("Draw substituent annotations", {
  linkage_hidden <- "Gal6S(a1-"
  oriented <- "GalNAc6S(b1-3)GalNAc(a1-"
  unknown_linkage <- "GalNAc?S(b1-3)GalNAc(a1-"

  vdiffr::expect_doppelganger(
    "substituent annotation with linkage hidden",
    draw_cartoon(linkage_hidden, show_linkage = FALSE)
  )
  vdiffr::expect_doppelganger(
    "horizontal substituent annotation",
    draw_cartoon(oriented, orient = "H")
  )
  vdiffr::expect_doppelganger(
    "vertical substituent annotation",
    draw_cartoon(oriented, orient = "V")
  )
  vdiffr::expect_doppelganger(
    "unknown substituent linkage annotation",
    draw_cartoon(unknown_linkage)
  )
})

test_that("Draw reducing-end O-Fuc glycans", {
  vdiffr::expect_doppelganger(
    "reducing-end Fuc",
    draw_cartoon("Fuc(a1-")
  )
  vdiffr::expect_doppelganger(
    "reducing-end Fuc branch",
    draw_cartoon("GlcNAc(b1-3)Fuc(a1-")
  )
})

test_that("Draw representative Fuc-like residues", {
  vdiffr::expect_doppelganger(
    "Fuc-like branch sides Qui",
    draw_cartoon("Qui(a1-3)[Qui(a1-6)]GlcNAc(b1-")
  )
  vdiffr::expect_doppelganger(
    "upward Fuc-like triangle orientation FucNAc",
    draw_cartoon("FucNAc(a1-3)[FucNAc(a1-6)]GlcNAc(b1-", fuc_orient = "up")
  )
})

test_that("Draw nested Xyl-Gal-Fuc side chain", {
  glycan <- "Glc(b1-4)[Fuc(a1-2)Gal(b1-2)Xyl(a1-6)]Glc(b1-4)Glc(b1-"
  vdiffr::expect_doppelganger(
    "nested Xyl-Gal-Fuc side chain",
    draw_cartoon(glycan)
  )
})

test_that("Exported cartoons preserve Fuc orientation", {
  glycan <- "Fuc(a1-3)[Fuc(a1-6)]GlcNAc(b1-4)GlcNAc(b1-"
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(
    result <- export_cartoons(glycan, temp_dir, fuc_orient = "up")
  )

  vdiffr::expect_doppelganger(
    "exported upward Fuc triangle orientation",
    result[[1]]
  )
})

test_that("Exported cartoons preserve custom node size", {
  glycan <- "Gal(b1-3)GalNAc(a1-"
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  fs::dir_create(temp_dir)

  suppressMessages(
    result <- export_cartoons(glycan, temp_dir, node_size = 1.2)
  )

  vdiffr::expect_doppelganger("exported custom node size", result[[1]])
})

test_that("Draw glycans as ggplot2 annotations", {
  data <- data.frame(
    x = c(1, 3),
    y = c(1, 2),
    structure = c(
      "Gal(b1-3)GalNAc(a1-",
      "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-"
    )
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      structure = .data$structure
    )
  ) +
    geom_glycan() +
    ggplot2::coord_cartesian(
      xlim = c(0, 4),
      ylim = c(0, 3),
      expand = FALSE
    ) +
    ggplot2::theme_void()

  vdiffr::expect_doppelganger("ggplot2 glycan annotations", plot)
})
