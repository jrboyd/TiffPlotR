library(testthat)
library(ggplot2)

test_that("add_um_scale_bar adds segment and text to ggplot", {
  p <- ggplot(data.frame(i = 1:10, j = 1:10), aes(i, j)) +
    geom_point()

  p2 <- add_um_scale_bar(p, unit_per_pixel = 0.5, bar_um = 2, xlim = c(1, 10), ylim = c(1, 10))

  expect_s3_class(p2, "ggplot")
  expect_equal(length(p2$layers), length(p$layers) + 2)
})

test_that("add_um_scale_bar works on TiffPlotData active plot", {
  df <- data.frame(i = 1:10, j = 1:10, value = runif(10))
  p <- ggplot(df, aes(i, j)) + geom_point()
  obj <- TiffPlotData(
    data = df,
    plots = list(raw = p),
    activePlot = "raw",
    rect = TiffRect(1, 10, 1, 10)
  )

  out <- add_um_scale_bar(obj, unit_per_pixel = 0.5, bar_um = 2)

  expect_s4_class(out, "TiffPlotData")
  expect_equal(out@activePlot, "raw")
  expect_equal(length(out@plots[["raw"]]$layers), length(p$layers) + 2)
})

test_that("add_um_scale_bar infers scale from TiffPlotData slots", {
  df <- data.frame(i = 1:10, j = 1:10, value = runif(10))
  p <- ggplot(df, aes(i, j)) + geom_point()
  obj <- TiffPlotData(
    data = df,
    plots = list(raw = p),
    activePlot = "raw",
    rect = TiffRect(1, 10, 1, 10),
    unit_per_pixel = 0.5,
    unit_name = "um"
  )

  out <- add_um_scale_bar(obj)
  expect_s4_class(out, "TiffPlotData")
  expect_equal(length(out@plots[["raw"]]$layers), length(p$layers) + 2)
})

test_that("add_um_scale_bar validates bad inputs", {
  p <- ggplot(data.frame(i = 1:5, j = 1:5), aes(i, j)) + geom_point()

  expect_error(add_um_scale_bar(p, unit_per_pixel = 0), "positive")
  expect_error(add_um_scale_bar(p, unit_per_pixel = 0.5, bar_fraction = 1), "bar_fraction")
  expect_error(add_um_scale_bar(p, unit_per_pixel = 0.5, pad_fraction = 0.6), "pad_fraction")
})

test_that("set_axes_um relabels breaks in microns and keeps y reversed", {
  p <- ggplot(data.frame(i = c(0, 1000), j = c(0, 1000)), aes(i, j)) +
    geom_point() +
    scale_y_reverse()

  p2 <- set_axes_um(p, unit_per_pixel = 0.5)
  b <- ggplot_build(p2)
  pp <- b$layout$panel_params[[1]]

  expect_equal(p2$scales$get_scales("x")$name, "um")
  expect_true(TiffPlotR:::.scale_bar_axis_reversed(p2, "y"))
  # 0..1000 px at 0.5 um/px is 0..500 um: labels are round micron values
  x_labs <- pp$x$get_labels()
  expect_true(all(as.numeric(x_labs[!is.na(x_labs)]) %% 100 == 0))
  expect_true("500" %in% x_labs)
  # data stays in pixel space
  expect_equal(range(b$data[[1]]$x), c(0, 1000))
})

test_that("set_axes_pixel reverts set_axes_um", {
  p <- ggplot(data.frame(i = c(0, 1000), j = c(0, 1000)), aes(i, j)) +
    geom_point() +
    scale_y_reverse()

  p2 <- set_axes_pixel(set_axes_um(p, unit_per_pixel = 0.5))
  pp <- ggplot_build(p2)$layout$panel_params[[1]]

  expect_equal(p2$scales$get_scales("y")$name, "pixel")
  expect_true(TiffPlotR:::.scale_bar_axis_reversed(p2, "y"))
  expect_true("1000" %in% pp$x$get_labels())
})

test_that("set_axes_um uses TiffPlotData slots on the active plot", {
  df <- data.frame(i = 1:10, j = 1:10, value = runif(10))
  p <- ggplot(df, aes(i, j)) + geom_point()
  obj <- TiffPlotData(
    data = df,
    plots = list(raw = p),
    activePlot = "raw",
    rect = TiffRect(1, 10, 1, 10),
    unit_per_pixel = 0.5,
    unit_name = "µm"
  )

  out <- set_axes_um(obj)
  expect_s4_class(out, "TiffPlotData")
  expect_equal(out@plots[["raw"]]$scales$get_scales("x")$name, "µm")

  out <- set_axes_pixel(out)
  expect_equal(out@plots[["raw"]]$scales$get_scales("x")$name, "pixel")
})

test_that("set_axes_um requires a usable unit_per_pixel", {
  p <- ggplot(data.frame(i = 1:5, j = 1:5), aes(i, j)) + geom_point()
  expect_error(set_axes_um(p), "unit_per_pixel")
  expect_error(set_axes_um(p, unit_per_pixel = NA_real_), "unit_per_pixel")
})
