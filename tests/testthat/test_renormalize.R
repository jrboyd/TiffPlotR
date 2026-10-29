library(testthat)

rect <- TiffRect(900, 2300, 1400, 2800)
cn <- c("DAPI", paste("channel", LETTERS[1:5]))
pm0 <- data.frame(channel = 1:6, min_value = 0, max_value = 10)

expected_norm <- function(value, mn, mx) pmin(1, pmax(0, (value - mn) / (mx - mn)))

test_that("small form by number updates data, precalc_max and plot data", {
  img <- fetchTiffData(exampleTiff(), rect = rect, precalc_max = pm0, resolution = 4)
  img2 <- renormalize(img, channels = c(1, 3), max_value = c(5, 0.2))

  expect_s4_class(img2, "TiffPlotData")
  expect_equal(img2@precalc_max$max_value, c(5, 10, 0.2, 10, 10, 10))
  expect_equal(img2@precalc_max$min_value, rep(0, 6))

  d <- img2@data
  ch1 <- d$channel == 1
  expect_equal(d$norm_value[ch1], expected_norm(d$value[ch1], 0, 5))
  # untouched channel is unchanged
  ch2 <- d$channel == 2
  expect_equal(d$norm_value[ch2], img@data$norm_value[ch2])
  expect_equal(img2@plots$normalized$data$norm_value, d$norm_value)
  # original object is not modified
  expect_equal(img@precalc_max$max_value, rep(10, 6))
  expect_false(identical(img@plots$normalized$data$norm_value, d$norm_value))
})

test_that("small form by name works after channel_names and selected_channels", {
  img <- fetchTiffData(exampleTiff(), rect = rect, precalc_max = pm0, resolution = 4,
                       channel_names = cn, selected_channels = c("channel B", "DAPI"))
  expect_equal(img@channels$channel_name, cn)
  img2 <- renormalize(img, channels = "channel B", min_value = 1)

  expect_equal(img2@precalc_max$min_value, c(0, 0, 1, 0, 0, 0))
  d <- img2@data
  b <- d$channel_name == "channel B"
  expect_equal(d$norm_value[b], expected_norm(d$value[b], 1, 10))
  dapi <- d$channel_name == "DAPI"
  expect_equal(d$norm_value[dapi], img@data$norm_value[dapi])
})

test_that("large form applies a subset precalc_max, by name and per resolution", {
  img <- fetchTiffData(exampleTiff(), rect = rect, precalc_max = pm0, resolution = 4,
                       channel_names = cn)
  pm <- data.frame(channel = rep(c("DAPI", "channel E"), 2), resolution = rep(c(4, 3), each = 2),
                   min_value = c(1, 0.5, 2, 1), max_value = c(30, 4, 60, 8))
  img2 <- renormalize(img, precalc_max = dplyr::group_by(pm, resolution))

  expect_equal(img2@precalc_max$min_value, c(1, 0, 0, 0, 0, 0.5))
  expect_equal(img2@precalc_max$max_value, c(30, 10, 10, 10, 10, 4))
  d <- img2@data
  e <- d$channel_name == "channel E"
  expect_equal(d$norm_value[e], expected_norm(d$value[e], 0.5, 4))
})

test_that("rgb and colorized plots are rebuilt, extra layers kept", {
  img <- fetchTiffData.rgb(exampleTiff(), rect = rect, precalc_max = pm0, resolution = 4,
                           red_channel = 1, green_channel = 6, blue_channel = 5)
  img@plots$rgb <- img@plots$rgb + geom_point(data = data.frame(i = 950, j = 1450), aes(i, j))
  n_layers <- length(img@plots$rgb$layers)
  old_raster <- img@plots$rgb$layers[[1]]$data

  img2 <- renormalize(img, channels = 1, min_value = 5, max_value = 20)
  new_raster <- img2@plots$rgb$layers[[1]]$data
  expect_equal(length(img2@plots$rgb$layers), n_layers)
  expect_equal(new_raster, convertTidyToRGB(img2@data, 1, 6, 5))
  expect_false(isTRUE(all.equal(old_raster$red, new_raster$red)))
  # original plot is not modified
  expect_identical(img@plots$rgb$layers[[1]]$data, old_raster)
  expect_silent(ggplot_build(img2@plots$rgb))

  imgc <- fetchTiffData.colorized(exampleTiff(), rect = rect, precalc_max = pm0, resolution = 4,
                                  channel_colors = list(DAPI = "blue", "channel E" = "red"),
                                  channel_names = cn)
  imgc2 <- renormalize(imgc, channels = "channel E", max_value = 0.1)
  expect_equal(imgc2@precalc_max$max_value[6], 0.1)
  expect_false(isTRUE(all.equal(imgc@plots$colorized$layers[[1]]$data$chex,
                                imgc2@plots$colorized$layers[[1]]$data$chex)))
})

test_that("plots without a rebuild recipe are left with a warning", {
  img <- fetchTiffData.rgb(exampleTiff(), rect = rect, precalc_max = pm0, resolution = 4)
  img@plots$rgb$meta$tiffplotr_composite <- NULL
  expect_warning(img2 <- renormalize(img, channels = 1, max_value = 10), "rgb")
  expect_equal(img2@precalc_max$max_value[1], 10)
})

test_that("bad input is rejected", {
  img <- fetchTiffData(exampleTiff(), rect = rect, precalc_max = pm0, resolution = 4)
  expect_error(renormalize(img, channels = 1), "at least one")
  expect_error(renormalize(img, channels = 9, max_value = 1), "Unknown channels")
  expect_error(renormalize(img, channels = "nope", max_value = 1), "Unknown channels")
  expect_error(renormalize(img, channels = 1:2, max_value = 1:3), "length")
  expect_error(renormalize(img, channels = 1, min_value = 20), "greater than")
  expect_error(renormalize(img, channels = 1, max_value = 1, precalc_max = pm0), "not both")
  # OME names resolve even when the object was fetched without channel_names
  img2 <- renormalize(img, channels = "Opal 570", max_value = 1)
  expect_equal(img2@precalc_max$max_value[3], 1)
  # objects with no channel table
  img@channels <- data.frame()
  expect_error(renormalize(img, channels = 1, max_value = 1), "does not record its channels")
})
