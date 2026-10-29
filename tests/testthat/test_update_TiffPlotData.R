library(testthat)
library(ggplot2)

# A TiffPlotData as TiffPlotR 0.1.19 saved it: no @channels or
# @package_version, and data$channel holding numbers or display names.
.old_object <- function(channel, cls = "TiffPlotData", facet = facet_wrap(~channel)) {
  df <- data.frame(i = rep(1:2, length(channel)), j = 1, channel = rep(channel, each = 2),
                   value = 1:4 * 10, norm_value = 1:4 / 4)
  p <- ggplot(df, aes(i, j, fill = norm_value)) + geom_raster() + facet
  pm <- data.frame(channel = 1:3, min_value = 0, max_value = 40)
  obj <- new(cls, data = df, plots = list(raw = p), activePlot = "raw",
             tiff_path = NA_character_, resolution = 1, precalc_max = pm,
             rect = TiffRect(1, 2, 1, 2), img_info = data.frame())
  attr(obj, "channels") <- NULL
  attr(obj, "package_version") <- NULL
  obj
}

test_that("update_TiffPlotData converts numeric channels and stamps the version", {
  old <- .old_object(c(3L, 1L))
  expect_false(methods::.hasSlot(old, "package_version"))

  new_obj <- expect_silent(update_TiffPlotData(old))
  expect_equal(new_obj@package_version, as.character(packageVersion("TiffPlotR")))
  expect_equal(new_obj@channels$channel, c(1, 3))
  expect_equal(new_obj@data$channel, rep(c(3L, 1L), each = 2))
  expect_equal(levels(new_obj@data$channel_name), c("1", "3"))
  expect_equal(new_obj@precalc_max, old@precalc_max)

  p <- new_obj@plots$raw
  expect_true("channel_name" %in% colnames(p$data))
  expect_equal(names(p$facet$params$facets), "channel_name")
  # the original plot's facet is untouched
  expect_equal(names(old@plots$raw$facet$params$facets), "channel")
  expect_silent(ggplot_build(p))
})

test_that("factor of channel numbers keeps its plotting order", {
  old <- .old_object(factor(c("3", "1"), levels = c("3", "1")))
  new_obj <- update_TiffPlotData(old)
  expect_equal(new_obj@data$channel, rep(c(3L, 1L), each = 2))
  expect_equal(levels(new_obj@data$channel_name), c("3", "1"))
})

test_that("named channels need the fetch-time channel_names", {
  old <- .old_object(factor(c("CD45", "DAPI"), levels = c("CD45", "DAPI")))
  expect_error(update_TiffPlotData(old), "channel_names")

  new_obj <- update_TiffPlotData(old, channel_names = c("DAPI", "PanCK", "CD45"))
  expect_equal(new_obj@channels$channel_name, c("DAPI", "PanCK", "CD45"))
  expect_equal(new_obj@data$channel, rep(c(3L, 1L), each = 2))
  expect_equal(levels(new_obj@data$channel_name), c("CD45", "DAPI"))
  expect_error(update_TiffPlotData(old, channel_names = c("DAPI", "PanCK")), "Unknown")
})

test_that("subclasses keep their class and grid facets are renamed", {
  old <- .old_object(c(1L, 2L), cls = "TiffPlotDataMasked", facet = facet_grid(channel ~ .))
  new_obj <- update_TiffPlotData(old)
  expect_s4_class(new_obj, "TiffPlotDataMasked")
  expect_equal(names(new_obj@plots$raw$facet$params$rows), "channel_name")
})

test_that("current objects pass through unchanged and silently", {
  df <- data.frame(i = 1:2, j = 1, channel = 1L, value = 1, norm_value = 1)
  obj <- TiffPlotData(df, plots = list(raw = ggplot(df, aes(i, j)) + geom_point()))
  expect_identical(update_TiffPlotData(obj), obj)
  expect_silent(TiffPlotR:::.check_TiffPlotData_version(obj))
})

test_that("functions taking TiffPlotData update old objects with a warning", {
  old <- .old_object(c(1L, 2L))
  expect_warning(out <- shape_annotate(old, TiffRect(1, 2, 1, 2)), "update_TiffPlotData")
  expect_true(methods::.hasSlot(out, "package_version"))
  expect_warning(out <- apply_coord_fixed(old), "0.1.19")
  expect_s4_class(out, "TiffPlotData")
  expect_warning(out <- add_um_scale_bar(old, unit_per_pixel = 1, bar_um = 0.5), "0.1.19")
  expect_warning(out <- set_axes_um(old, unit_per_pixel = 1), "0.1.19")
  expect_warning(expect_equal(old$channel_name, factor(c("1", "1", "2", "2"))), "0.1.19")
  expect_warning(out <- renormalize(old, channels = 2, max_value = 20), "0.1.19")
  expect_equal(out@data$norm_value, c(0.25, 0.5, 1, 1))
})
