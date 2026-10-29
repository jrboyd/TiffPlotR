library(testthat)
library(ggplot2)

rect <- TiffRect(1, 1024, 1, 1024)
fetch <- function(...) suppressMessages(fetchTiffData(exampleTiff(), rect, resolution = 4, selected_channels = 1, ...))
has_um_axes <- function(p) identical(p$scales$get_scales("x")$name, "µm")
n_layers <- function(p) length(p$layers)

test_that("fetches have no micron axes or bar unless asked", {
  withr::local_options(TPR_UM_AXES = NULL, TPR_UM_SCALE_BAR = NULL)
  img <- fetch()
  expect_false(has_um_axes(img@plots[[1]]))
  expect_equal(n_layers(img@plots[[1]]), 1)
  expect_false(img@plot_parameters$um_axes)
  expect_false(img@plot_parameters$scale_bar)
})

test_that("TPR_UM_AXES and TPR_UM_SCALE_BAR set the fetch defaults", {
  withr::local_options(TPR_UM_AXES = TRUE, TPR_UM_SCALE_BAR = TRUE)
  img <- fetch()
  p <- img@plots[[1]]
  expect_true(has_um_axes(p))
  expect_equal(n_layers(p), 3)  # raster, bar segment, bar label
  expect_true(img@plot_parameters$um_axes)
  expect_true(img@plot_parameters$scale_bar)
  expect_silent(ggplot_build(p))

  # an explicit argument wins over the option
  img <- fetch(um_axes = FALSE)
  expect_false(has_um_axes(img@plots[[1]]))
  expect_equal(n_layers(img@plots[[1]]), 3)

  rgb <- suppressMessages(fetchTiffData.rgb(exampleTiff(), rect, resolution = 4,
                                            red_channel = 5, green_channel = 6, blue_channel = 1))
  expect_true(has_um_axes(rgb@plots$rgb))
  # renormalize() swaps the raster but keeps the bar
  rgb2 <- renormalize(rgb, channels = 1, max_value = 30)
  expect_equal(n_layers(rgb2@plots$rgb), n_layers(rgb@plots$rgb))
  expect_true(has_um_axes(rgb2@plots$rgb))
})

test_that("invalid options and arguments are errors", {
  withr::local_options(TPR_UM_AXES = "yes")
  expect_error(fetch(), "TPR_UM_AXES must be TRUE or FALSE")
  withr::local_options(TPR_UM_AXES = NULL)
  expect_error(fetch(scale_bar = NA), "scale_bar must be TRUE or FALSE")
})

test_that("plotTiffData*: argument, then the object's choice, then the option", {
  withr::local_options(TPR_UM_AXES = NULL, TPR_UM_SCALE_BAR = NULL)
  img <- fetch(um_axes = TRUE)
  expect_true(has_um_axes(plotTiffData(img)))               # recorded with the object
  expect_false(has_um_axes(plotTiffData(img, um_axes = FALSE)))
  expect_equal(n_layers(plotTiffData(img, scale_bar = TRUE)), 3)

  img@plot_parameters$um_axes <- NULL                       # e.g. an older object
  expect_false(has_um_axes(plotTiffData(img)))
  withr::local_options(TPR_UM_AXES = TRUE)
  expect_true(has_um_axes(plotTiffData(img)))
  expect_true(has_um_axes(plotTiffData.colorized(img, channel_colors = list(DAPI = "blue"))))

  out <- plotTiffData(img, scale_bar = TRUE, return_object = TRUE)
  expect_true(out@plot_parameters$um_axes)
  expect_true(out@plot_parameters$scale_bar)
})

test_that("images without a pixel size stay in pixels, with a warning", {
  withr::local_options(TPR_UM_AXES = NULL, TPR_UM_SCALE_BAR = NULL)
  img <- fetch()
  img@unit_per_pixel <- NA_real_
  expect_warning(p <- plotTiffData(img, um_axes = TRUE, scale_bar = TRUE),
                 "micron axes and a micron scale bar can't be drawn")
  expect_false(has_um_axes(p))
  expect_equal(n_layers(p), 1)
})
