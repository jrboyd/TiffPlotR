library(testthat)

# fetchTiffDataMasked() must reject unusable masks before reading the image.
test_that("fetchTiffDataMasked checks masks before fetching image data", {
  local_mocked_bindings(.fetch_tiff_data = function(...) stop("image was fetched"))
  rect <- TiffRect(200, 900, 200, 900)

  # the example TIFF has no mask annotations
  expect_error(fetchTiffDataMasked(exampleTiff(), rect), "No mask points found")

  expect_error(fetchTiffDataMasked(exampleTiff(), rect, mask_points = data.frame(x = 1, y = 1)),
               "must contain i and j")

  outside <- data.frame(i = 10, j = 10, Text = "a", ROI_ID = "r1")
  expect_error(fetchTiffDataMasked(exampleTiff(), rect, mask_points = outside),
               "overlap the requested rect")
  # retain_unmasked keeps unmasked pixels, so it still fetches, with a warning
  expect_warning(
    expect_error(fetchTiffDataMasked(exampleTiff(), rect, mask_points = outside, retain_unmasked = TRUE),
                 "image was fetched"),
    "overlap the requested rect")

  inside <- data.frame(i = 300, j = 300, Text = "a", ROI_ID = "r1")
  expect_error(fetchTiffDataMasked(exampleTiff(), rect, mask_points = inside), "image was fetched")
})

# a 201 x 201 pixel mask from (x0, y0), split into A and B halves
.mask_points <- function(x0 = 100, y0 = 100) {
  mp <- expand.grid(i = x0 + 0:200, j = y0 + 0:200)
  mp$Text <- ifelse(mp$i < x0 + 100, "A", "B")
  mp$ROI_ID <- mp$Text
  mp
}

.check_masked <- function(m, spacing, x0 = 100, y0 = 100) {
  expect_s4_class(m, "TiffPlotDataMasked")
  expect_setequal(unique(m@data$Text), c("A", "B"))
  expect_true(all(m@data$i > x0 - spacing & m@data$i < x0 + 200 + spacing))
  expect_true(all(m@data$j > y0 - spacing & m@data$j < y0 + 200 + spacing))
  # about 201 x 201 full-resolution pixels, split in two
  n_px <- nrow(unique(m@data[c("i", "j")]))
  expect_gt(n_px, 0.8 * (201 / spacing)^2)
}

test_that("fetchTiffDataMasked matches masks below full resolution", {
  for (res in c(2, 4)) {
    m <- suppressMessages(fetchTiffDataMasked(exampleTiff(), TiffRect(1, 1024, 1, 1024), resolution = res,
                                              mask_points = .mask_points(), selected_channels = 1))
    .check_masked(m, spacing = 2^(res - 1))
  }
})

# Regression: masks were joined on i/j rebuilt from bins, which misses by a
# few ulps when the pixel spacing isn't an integer, e.g. a pyramid level of
# 1082 px for an 8651 px image. The example's levels are exact halves, so this
# feeds the masked path the signal grid .fetch_tiff_data() built for the case
# that failed: level 4 of the old 8651 px example, rect from (900, 1400).
test_that("fetchTiffDataMasked matches masks on a non-integer pixel grid", {
  ratio <- 1082 / 8651
  spacing <- 1 / ratio
  grid <- expand.grid(k = 1:176, l = 1:176)
  df <- data.frame(i = (grid$k + 900 * ratio) / ratio, j = (grid$l + 1400 * ratio) / ratio, channel = 1L,
                   value = 1, norm_value = 0.5)
  df$channel_name <- factor("1")
  rect <- TiffRect(900, 2300, 1400, 2800)
  base <- TiffPlotData(df, plots = list(normalized = ggplot2::ggplot()), resolution = 4,
                       precalc_max = data.frame(channel = 1, min_value = 0, max_value = 2), rect = rect,
                       plot_parameters = list(scale_colors = TiffPlotR:::.magma_scale_colors()))
  local_mocked_bindings(.fetch_tiff_data = function(...) base)

  m <- suppressMessages(fetchTiffDataMasked("unused.tiff", rect, mask_points = .mask_points(1000, 1500)))
  .check_masked(m, spacing = spacing, x0 = 1000, y0 = 1500)
})
