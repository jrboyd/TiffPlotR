library(testthat)
library(ggplot2)

test_that("default scale_colors reproduce scale_fill_viridis_c(option = 'magma')", {
  x <- seq(0, 1, length.out = 51)
  old <- scale_fill_viridis_c(option = "magma")$palette(x)
  new <- TiffPlotR:::.fill_gradient_layers()[[2]]$palette(x)
  expect_equal(new, old)
})

test_that(".check_scale_colors validates colours", {
  expect_equal(TiffPlotR:::.check_scale_colors(c(a = "black", b = "#00FF00")), c("black", "#00FF00"))
  expect_error(TiffPlotR:::.check_scale_colors("black"), "at least 2")
  expect_error(TiffPlotR:::.check_scale_colors(c("black", NA)), "at least 2")
  expect_error(TiffPlotR:::.check_scale_colors(c("black", "notacolour")), "notacolour")
})

test_that("plot_parameters must be a named list", {
  df <- data.frame(i = 1:2, j = 1, channel = 1L)
  p <- ggplot(df, aes(i, j)) + geom_point()
  obj <- TiffPlotData(df, plots = list(raw = p), plot_parameters = list(scale_colors = c("black", "white")))
  expect_equal(obj$plot_parameters$scale_colors, c("black", "white"))
  expect_error(TiffPlotData(df, plots = list(raw = p), plot_parameters = list("black")), "named list")
})

test_that("fetched plots use scale_colors and record it", {
  rect <- TiffRect(900, 2300, 1400, 2800)
  img <- fetchTiffData(exampleTiff(), rect, resolution = 4, selected_channels = 1,
                       scale_colors = c("black", "green"))
  expect_equal(img@plot_parameters$scale_colors, c("black", "green"))
  sc <- img@plots[[1]]$scales$get_scales("fill")
  expect_equal(sc$palette(c(0, 1)), c("#000000", "#00FF00"))

  img_default <- fetchTiffData(exampleTiff(), rect, resolution = 4, selected_channels = 1)
  expect_equal(img_default@plot_parameters$scale_colors, TiffPlotR:::.default_scale_colors())

  expect_error(fetchTiffData(exampleTiff(), rect, resolution = 4, scale_colors = "red"), "at least 2")

  rgb <- fetchTiffData.rgb(exampleTiff(), rect, resolution = 4, red_channel = 5, green_channel = 6, blue_channel = 1)
  expect_s3_class(rgb@plots$rgb$scales$get_scales("fill"), "ScaleDiscreteIdentity")
})
