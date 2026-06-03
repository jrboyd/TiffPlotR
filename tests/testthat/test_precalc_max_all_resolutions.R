library(testthat)

test_that("collect_channel_quantiles_all_resolutions returns inspectable quantile table", {
  tiff_path <- exampleTiff()
  qtab <- collect_channel_quantiles_all_resolutions(tiff_path, probs = c(0, 0.5, 0.9, 0.998))

  expect_true(is.data.frame(qtab))
  expect_true(all(c("channel", "resolution", "prob", "value") %in% colnames(qtab)))
  expect_true(nrow(qtab) > 0)
  expect_true(all(sort(unique(qtab$prob)) == c(0, 0.5, 0.9, 0.998)))
})

test_that("quantiles_to_precalc_max reduces quantile table to fetch-compatible output", {
  quantile_df <- data.frame(
    channel = c(1, 1, 1, 1, 2, 2, 2, 2),
    resolution = c(1, 1, 2, 2, 1, 1, 2, 2),
    prob = c(0.9, 0.998, 0.9, 0.998, 0.9, 0.998, 0.9, 0.998),
    value = c(2, 10, 3, 11, 5, 20, 4, 18)
  )

  pm <- quantiles_to_precalc_max(quantile_df, min_prob = 0.9, max_prob = 0.998, min_agg = "min", max_agg = "max")

  expect_true(is.data.frame(pm))
  expect_true(all(c("channel", "min_value", "max_value") %in% colnames(pm)))
  expect_equal(pm$min_value, c(2, 4))
  expect_equal(pm$max_value, c(11, 20))
})

test_that("precalc_max_all_resolutions wraps the 2-step workflow", {
  tiff_path <- exampleTiff()
  out <- precalc_max_all_resolutions(
    tiff_path,
    probs = c(0.9, 0.998),
    return_by_resolution = TRUE
  )

  expect_true(is.list(out))
  expect_true(all(c("precalc_max", "quantiles") %in% names(out)))
  expect_true(is.data.frame(out$precalc_max))
  expect_true(is.data.frame(out$quantiles))
  expect_true(all(c("channel", "resolution", "prob", "value") %in% colnames(out$quantiles)))
})
