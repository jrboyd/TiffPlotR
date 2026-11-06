library(testthat)

# limits picked by fetches without precalc_max (min_q / max_q)
rect <- TiffRect(1, 1024, 1, 1024)
fetch4 <- function(...) suppressMessages(fetchTiffData(exampleTiff(), rect, resolution = 4, ...))
no_q_options <- list(TPR_MIN_Q = NULL, TPR_MAX_Q = NULL)

test_that(".limits_from_quantiles follows the zero-dropping rule", {
  q <- c(0, 0.3, 0.5, 0.99, 0.999, 1)
  dense <- c(0, 10, 20, 90, 99, 100)
  expect_equal(.limits_from_quantiles(q, dense, 0.3, 0.99), c(10, 90))
  expect_equal(.limits_from_quantiles(q, dense, NULL, 0.999), c(0, 99))  # min_q = NULL: floor of 0
  expect_equal(.limits_from_quantiles(q, c(5, dense[-1]), NULL, 0.999), c(0, 99))
  # min_q = 0 is an ordinary quantile: a zero minimum falls back to the smallest non-zero one
  expect_equal(.limits_from_quantiles(q, dense, 0, 0.999), c(10, 99))
  expect_equal(.limits_from_quantiles(q, c(5, dense[-1]), 0, 0.999), c(5, 99))
  sparse <- c(0, 0, 0, 0, 5, 8)
  expect_equal(.limits_from_quantiles(q, sparse, 0.3, 0.99), c(0, 5))   # nearest non-zero to 0.99
  expect_equal(.limits_from_quantiles(q, rep(0, 6), 0.3, 0.99), c(0, 1))
})

test_that("default fetch limits match the old quantile_norm = 0.999 for channels with signal", {
  withr::local_options(no_q_options)
  img <- fetch4()
  pm <- img@precalc_max
  arr <- fetchTiffArray(exampleTiff(), rect, resolution = 4)@.Data
  for(ch in c(1, 3, 4, 5, 6)){
    expect_equal(pm$min_value[pm$channel == ch], 0)
    expect_equal(pm$max_value[pm$channel == ch], unname(quantile(arr[, , ch], 0.999)))
  }
  # Opal 520's 0.999 quantile is 0; it used to give NA norm_value (a grey panel)
  expect_true(all(is.finite(pm$max_value)))
  expect_false(anyNA(img@data$norm_value))
  expect_equal(img@plot_parameters[c("min_q", "max_q")], list(min_q = NULL, max_q = 0.999))
  expect_true("min_q" %in% names(img@plot_parameters))
})

test_that("min_q = 0 is an ordinary quantile, unlike the default NULL", {
  withr::local_options(no_q_options)
  arr <- fetchTiffArray(exampleTiff(), rect, resolution = 4)@.Data
  dapi <- arr[, , 1]
  pm <- fetch4(min_q = 0, selected_channels = 1)@precalc_max
  dq <- quantile(dapi, .scale_probs(0, 0.999))
  expect_equal(pm$min_value[pm$channel == 1], unname(min(dq[dq > 0])))   # smallest non-zero quantile
  expect_gt(pm$min_value[pm$channel == 1], 0)
  expect_error(fetch4(min_q = NA), "min_q")
})

test_that("min_q and max_q set the fetch limits", {
  withr::local_options(no_q_options)
  img <- fetch4(min_q = 0.5, max_q = 0.99, selected_channels = "DAPI")
  arr <- fetchTiffArray(exampleTiff(), rect, resolution = 4)@.Data
  pm <- img@precalc_max
  expect_equal(pm$min_value[pm$channel == 1], unname(quantile(arr[, , 1], 0.5)))
  expect_equal(pm$max_value[pm$channel == 1], unname(quantile(arr[, , 1], 0.99)))
  expect_error(fetch4(min_q = 0.9, max_q = 0.5), "min_q < max_q")
})

test_that("TPR_MIN_Q / TPR_MAX_Q set the defaults; explicit arguments win", {
  withr::local_options(TPR_MIN_Q = 0.5, TPR_MAX_Q = 0.99)
  img <- fetch4(selected_channels = 1)
  expect_equal(img@plot_parameters[c("min_q", "max_q")], list(min_q = 0.5, max_q = 0.99))
  expect_equal(fetch4(selected_channels = 1, max_q = 0.999)@plot_parameters$max_q, 0.999)
  withr::local_options(TPR_MAX_Q = 2)
  expect_error(fetch4(), "Invalid TPR_MAX_Q option")
})

test_that("precalcTiffDataScales gives the limits fetchTiffData computes for the same view", {
  withr::local_options(no_q_options)
  pm <- precalcTiffDataScales(exampleTiff(), rect, resolution = 4)
  expect_equal(colnames(pm), c("resolution", "channel", "min_value", "max_value"))
  expect_equal(pm$resolution, rep(4, 6))
  expect_equal(pm[, -1], fetch4()@precalc_max, ignore_attr = TRUE)
  # automatic resolution, as fetchTiffData chooses it
  expect_message(pm2 <- precalcTiffDataScales(exampleTiff(), rect, max_pixels = 200), "selected resolution 4")
  expect_equal(pm2, pm)
  expect_silent(precalcTiffDataScales(exampleTiff(), rect, max_pixels = 200, verbose = FALSE))
  pm3 <- precalcTiffDataScales(exampleTiff(), rect, resolution = 4, min_q = 0.5, max_q = 0.99,
                               selected_channels = c("Opal 690", "DAPI"))
  expect_equal(pm3$channel, c(5, 1))
  full <- fetch4(min_q = 0.5, max_q = 0.99)@precalc_max
  expect_equal(pm3$min_value, full$min_value[c(5, 1)])
  expect_equal(pm3$max_value, full$max_value[c(5, 1)])
})

test_that("precalcTiffDataScales output carries a view's contrast to other fetches", {
  withr::local_options(no_q_options)
  pm <- precalcTiffDataScales(exampleTiff(), rect, resolution = 4)
  zoom <- suppressMessages(fetchTiffData(exampleTiff(), TiffRect(500, 800, 600, 900), resolution = 2,
                                         precalc_max = pm))
  expect_equal(zoom@precalc_max, pm[, -1], ignore_attr = TRUE)
})

test_that("quantile_norm still works, with a deprecation warning", {
  withr::local_options(no_q_options)
  expect_warning(old <- fetch4(quantile_norm = 0.99, selected_channels = 1), "quantile_norm is deprecated")
  expect_equal(old@precalc_max, fetch4(max_q = 0.99, selected_channels = 1)@precalc_max)
  expect_equal(old@plot_parameters$max_q, 0.99)
  expect_warning(suppressMessages(fetchTiffDataRGB(exampleTiff(), rect, resolution = 4, quantile_norm = 0.99,
                                                   red_channel = 5, green_channel = 6, blue_channel = 1)),
                 "quantile_norm is deprecated")
})
