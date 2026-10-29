library(testthat)
library(ggplot2)

rect <- TiffRect(900, 2300, 1400, 2800)
img <- suppressMessages(fetchTiffData(exampleTiff(), rect, resolution = 4, channel_names = "ome",
                                      scale_colors = c("black", "white")))

test_that("plotTiffData returns a ggplot matching the fetched plot by default", {
  p <- plotTiffData(img)
  expect_s3_class(p, "ggplot")
  expect_equal(ggplot_build(p)$data, ggplot_build(img@plots$normalized)$data)
})

test_that("plotTiffData selects and orders channels, and sets facet layout", {
  p <- plotTiffData(img, selected_channels = c("Opal 690", "DAPI"), ncol = 1)
  expect_equal(levels(p$data$channel_name), c("Opal 690", "DAPI"))
  expect_setequal(unique(p$data$channel), c(5, 1))
  expect_equal(p$facet$params$ncol, 1)
  expect_error(plotTiffData(img, selected_channels = "nope"), "Unknown selected_channels")

  sub <- suppressMessages(fetchTiffData(exampleTiff(), rect, resolution = 4, selected_channels = 1))
  expect_error(plotTiffData(sub, selected_channels = 2), "not in img_data")
})

test_that("plotTiffData colours, value_var and rect", {
  sc <- function(p) p$scales$get_scales("fill")$palette(c(0, 1))
  # defaults to the colours the object was fetched with
  expect_equal(sc(plotTiffData(img)), c("#000000", "#FFFFFF"))
  expect_equal(sc(plotTiffData(img, scale_colors = c("black", "red"))), c("#000000", "#FF0000"))
  expect_error(plotTiffData(img, scale_colors = "red"), "at least 2")

  expect_equal(rlang::as_label(plotTiffData(img, value_var = "value")$mapping$fill), "value")
  expect_error(plotTiffData(img, value_var = "nope"))

  zoom <- TiffRect(1000, 1200, 1500, 1700)
  expect_equal(plotTiffData(img, rect = zoom)$coordinates$limits$x, c(1000, 1200))
})

test_that("return_object stores the plot and makes it active", {
  out <- plotTiffData(img, value_var = "value", scale_colors = c("navy", "yellow"), return_object = TRUE)
  expect_s4_class(out, "TiffPlotData")
  expect_equal(names(out@plots), c("normalized", "raw"))
  expect_equal(out@activePlot, "raw")
  expect_equal(out@plot_parameters$scale_colors, c("navy", "yellow"))

  out <- plotTiffData(out, ncol = 2, return_object = TRUE, plot_name = "two_col")
  expect_equal(out@activePlot, "two_col")
  expect_error(plotTiffData(img, return_object = TRUE, plot_name = ""), "plot_name")
})

test_that("plotTiffData.rgb and .colorized rebuild composites from the data", {
  fetched <- suppressMessages(fetchTiffData.rgb(exampleTiff(), rect, resolution = 4,
                                                red_channel = 5, green_channel = 6, blue_channel = 1))
  p <- plotTiffData.rgb(fetched, red_channel = 5, green_channel = 6, blue_channel = 1)
  expect_equal(ggplot_build(p)$data, ggplot_build(fetched@plots$rgb)$data)
  expect_equal(p$meta$tiffplotr_composite$channels, c(5, 6, 1))

  p <- plotTiffData.rgb(img, red_channel = "Opal 690", green_channel = "Autofluorescence", blue_channel = "DAPI")
  expect_equal(p$meta$tiffplotr_composite$channels, c(5, 6, 1))

  p <- plotTiffData.colorized(img, channel_colors = list(DAPI = "blue", "Opal 690" = "yellow"))
  expect_equal(p$meta$tiffplotr_composite$channel_map$channel, c(1, 5))
  expect_error(plotTiffData.colorized(img, channel_colors = c(DAPI = "blue")), "must be a list")

  out <- plotTiffData.colorized(img, channel_colors = list(DAPI = "blue"), return_object = TRUE)
  expect_equal(out@activePlot, "colorized")
  # composites made here can still be renormalized
  out <- renormalize(out, channels = "DAPI", max_value = 30)
  expect_s3_class(out@plots$colorized, "ggplot")
})

test_that("plotTiffDataMasked needs masked data", {
  expect_error(plotTiffDataMasked(img), "fetchTiffDataMasked")

  mp <- expand.grid(i = 1000:1200, j = 1500:1700)
  mp$Text <- ifelse(mp$i < 1100, "A", "B")
  mp$ROI_ID <- mp$Text
  m <- suppressMessages(fetchTiffDataMasked(exampleTiff(), rect, resolution = 4, mask_points = mp,
                                            selected_channels = 1:2))
  p <- plotTiffDataMasked(m, selected_channels = 2)
  expect_equal(levels(droplevels(p$data$channel_name)), "2")
  expect_equal(names(p$facet$params$cols), "Text")
  expect_equal(ggplot_build(plotTiffDataMasked(m))$data, ggplot_build(m@plots$masked)$data)
})
