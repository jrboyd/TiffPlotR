library(testthat)

# gather_channel_quantiles()-shaped input: per resolution, a quantile x channel
# matrix with rows named like "99.000%".
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

test_that("precalc_from_quantiles picks min/max per resolution and channel", {
  pm <- precalc_from_quantiles(.fake_gq(2), min_q = 0.30, max_q = 0.99)
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

test_that("precalc_from_quantiles output is accepted by fetchTiffData", {
  # exampleTiff has 6 channels and 5 resolutions
  m <- matrix(rep(c(0, 1, 2, 50, 80, 100), 6), ncol = 6,
              dimnames = list(c("0.000%", "30.000%", "50.000%", "99.000%", "99.900%", "100.000%"), 1:6))
  gq <- list(signal_quantiles = rep(list(m), 5))
  pm <- precalc_from_quantiles(gq)
  img <- fetchTiffData(exampleTiff(), rect = TiffRect(0, 100, 0, 100), precalc_max = pm, resolution = 2)
  expect_s4_class(img, "TiffPlotData")
})

test_that("precalc_from_quantiles validates input", {
  expect_error(precalc_from_quantiles(list()), "gather_channel_quantiles")
  expect_error(precalc_from_quantiles(.fake_gq(), min_q = 0.9, max_q = 0.5), "min_q < max_q")
})
