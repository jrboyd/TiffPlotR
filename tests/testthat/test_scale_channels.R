library(testthat)

# scale_channels_gather_quantiles()-shaped input: per resolution, a quantile x
# channel matrix with rows named like "99.000%".
.fake_gq <- function(n_res = 2) {
  probs <- c(0, 0.30, 0.50, 0.99, 0.999, 1)
  rn <- paste0(formatC(probs * 100, format = "f", digits = 3), "%")
  mk <- function(scale) {
    m <- cbind(
      `1` = c(0, 10, 20, 90, 99, 100) * scale,   # dense: exact min_q present
      `2` = c(0, 0, 0, 0, 5, 8) * scale,         # sparse: only top quantiles non-zero
      `3` = c(0, 0, 0, 0, 0, 0)                  # empty
    )
    rownames(m) <- rn
    m
  }
  list(signal_quantiles = lapply(seq_len(n_res), mk), assessed_ranges = list())
}

test_that("scale_channels_set_thresholds picks min/max per resolution and channel", {
  pm <- scale_channels_set_thresholds(.fake_gq(2), min_q = 0.30, max_q = 0.99)
  expect_equal(colnames(pm), c("resolution", "channel", "min_value", "max_value"))
  expect_equal(nrow(pm), 6)
  ch1 <- pm[pm$channel == 1, ]
  expect_equal(ch1$min_value, c(10, 20))
  expect_equal(ch1$max_value, c(90, 180))
  # sparse: zeros dropped, nearest non-zero to 0.99 is 0.999; min = smallest non-zero
  ch2 <- pm[pm$channel == 2 & pm$resolution == 1, ]
  expect_equal(ch2$max_value, 5)
  expect_equal(ch2$min_value, 0)   # min (5) would equal max -> reset to 0
  ch3 <- pm[pm$channel == 3, ]
  expect_true(all(ch3$min_value == 0 & ch3$max_value == 1))
  expect_true(all(pm$max_value > pm$min_value))
})

test_that("scale_channels_set_thresholds output is accepted by fetchTiffData", {
  # exampleTiff has 6 channels and 4 resolutions
  m <- matrix(rep(c(0, 1, 2, 50, 80, 100), 6), ncol = 6,
              dimnames = list(c("0.000%", "30.000%", "50.000%", "99.000%", "99.900%", "100.000%"), 1:6))
  gq <- list(signal_quantiles = rep(list(m), 4))
  pm <- scale_channels_set_thresholds(gq)
  img <- fetchTiffData(exampleTiff(), rect = TiffRect(1, 100, 1, 100), precalc_max = pm, resolution = 2)
  expect_s4_class(img, "TiffPlotData")
})

test_that("scale_channels_set_thresholds validates input", {
  expect_error(scale_channels_set_thresholds(list()), "scale_channels_gather_quantiles")
  expect_error(scale_channels_set_thresholds(.fake_gq(), min_q = 0.9, max_q = 0.5), "min_q < max_q")
})

test_that(".gather_plan reads the levels a LOW-mode level depends on", {
  # 4 levels: 1024^2, 512^2, 256^2, 128^2 px
  meta <- data.frame(resolutionLevel = 1:4, sizeX = 1024 / 2^(0:3), sizeY = 1024 / 2^(0:3))
  all_full <- .gather_plan(NULL, meta, max_full_fetch = Inf)
  expect_equal(all_full$requested, 1:4)
  expect_equal(all_full$needed, 4:1)
  expect_equal(.gather_plan(3:4, meta, Inf)$needed, 4:3)
  expect_equal(.gather_plan(1, meta, Inf)$needed, 1)
  # levels 1 and 2 are LOW: level 1 needs 2 and 3 (the finest FULL level), not 4
  plan <- .gather_plan(1, meta, max_full_fetch = 300^2)
  expect_equal(plan$requested, 1)
  expect_equal(plan$needed, 3:1)
  expect_equal(.gather_plan(c(4, 2), meta, 300^2)$needed, 4:2)
  expect_error(.gather_plan(5, meta, Inf), "resolution levels: 1, 2, 3, 4")
  expect_error(.gather_plan(NULL, meta, 100), "raise max_full_fetch")
})

test_that("scale_channels_gather_quantiles reports only the requested resolutions", {
  gq <- suppressMessages(scale_channels_gather_quantiles(exampleTiff(), max_full_fetch = 1e6, workers = 1,
                                                         resolutions = 3:4))
  expect_null(gq$signal_quantiles[[1]])
  expect_null(gq$signal_quantiles[[2]])
  expect_equal(dim(gq$signal_quantiles[[4]]), c(length(.query_probs()), 6))
  pm <- scale_channels_set_thresholds(gq)
  expect_equal(sort(unique(pm$resolution)), 3:4)
  expect_equal(nrow(pm), 12)
})

test_that("old quantile function names still work, with a deprecation warning", {
  expect_warning(pm <- precalc_from_quantiles(.fake_gq(2)), "scale_channels_set_thresholds")
  expect_equal(pm, scale_channels_set_thresholds(.fake_gq(2)))
  expect_warning(expect_error(gather_channel_quantiles("no_such_file.tiff")),
                 "scale_channels_gather_quantiles")
})

test_that("old rgb/colorized fetch names still work, with a deprecation warning", {
  rect <- TiffRect(1, 1024, 1, 1024)
  expect_warning(old <- suppressMessages(fetchTiffData.rgb(exampleTiff(), rect, resolution = 4,
                                          red_channel = 5, green_channel = 6, blue_channel = 1)),
                 "fetchTiffDataRGB")
  new <- suppressMessages(fetchTiffDataRGB(exampleTiff(), rect, resolution = 4,
                                           red_channel = 5, green_channel = 6, blue_channel = 1))
  expect_equal(old@data, new@data)
  expect_warning(old <- suppressMessages(fetchTiffData.colorized(exampleTiff(), rect = rect, resolution = 4,
                                          channel_colors = list(DAPI = "blue"))),
                 "fetchTiffDataColorized")
  expect_s4_class(old, "TiffPlotData")
})
