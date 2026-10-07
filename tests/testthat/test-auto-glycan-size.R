test_that("automatic sizing is the default across glycan interfaces", {
  specification <- auto_glycan_size()
  expect_s3_class(specification, "glydraw_auto_size")
  expect_null(specification$max_size)
  expect_identical(geom_glycan()$geom_params$fit, TRUE)
  expect_equal(geom_glycan()$geom_params$size_limit, 1)
  expect_identical(scale_x_glycan()$guide$params$glycan_fit, TRUE)
  expect_identical(scale_y_glycan()$guide$params$glycan_fit, TRUE)
  expect_identical(guide_glycan()$params$glycan_fit, TRUE)
  expect_equal(
    scale_x_glycan(
      size = auto_glycan_size(max_size = 0.2)
    )$guide$params$glycan_size,
    0.2
  )
  expect_equal(
    geom_glycan(size = auto_glycan_size(max_size = 0.7))$geom_params$size_limit,
    0.7
  )
})

test_that("numeric sizes retain fixed sizing", {
  expect_identical(geom_glycan(size = 0.5)$geom_params$fit, FALSE)
  expect_identical(scale_x_glycan(size = 0.2)$guide$params$glycan_fit, FALSE)
  expect_identical(scale_y_glycan(size = 0.2)$guide$params$glycan_fit, FALSE)
  expect_identical(guide_glycan(size = 0.2)$params$glycan_fit, FALSE)
  skip_if_not_installed("ComplexHeatmap")
  expect_identical(anno_glycan("GlcNAc(b1-")@var_env$fit, TRUE)
  expect_identical(anno_glycan("GlcNAc(b1-", size = 0.2)@var_env$fit, FALSE)
})

test_that("automatic size limits must be positive finite scalars", {
  expect_snapshot(error = TRUE, auto_glycan_size(max_size = 0))
  expect_snapshot(error = TRUE, auto_glycan_size(max_size = Inf))
})
