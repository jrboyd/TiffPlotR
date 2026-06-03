library(testthat)
library(ggplot2)

test_that("add_um_scale_bar adds segment and text to ggplot", {
  p <- ggplot(data.frame(i = 1:10, j = 1:10), aes(i, j)) +
    geom_point()

  p2 <- add_um_scale_bar(p, um_per_pixel = 0.5, bar_um = 2, xlim = c(1, 10), ylim = c(1, 10))

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

  out <- add_um_scale_bar(obj, um_per_pixel = 0.5, bar_um = 2)

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
    pixels_per_unit = 0.5,
    unit_name = "um"
  )

  out <- add_um_scale_bar(obj)
  expect_s4_class(out, "TiffPlotData")
  expect_equal(length(out@plots[["raw"]]$layers), length(p$layers) + 2)
})

test_that("add_um_scale_bar validates bad inputs", {
  p <- ggplot(data.frame(i = 1:5, j = 1:5), aes(i, j)) + geom_point()

  expect_error(add_um_scale_bar(p, um_per_pixel = 0), "positive")
  expect_error(add_um_scale_bar(p, um_per_pixel = 0.5, bar_fraction = 1), "bar_fraction")
  expect_error(add_um_scale_bar(p, um_per_pixel = 0.5, pad_fraction = 0.6), "pad_fraction")
})
