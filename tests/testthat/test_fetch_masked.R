library(testthat)

# fetchTiffDataMasked() must reject unusable masks before reading the image.
test_that("fetchTiffDataMasked checks masks before fetching image data", {
  local_mocked_bindings(.fetch_tiff_data = function(...) stop("image was fetched"))
  rect <- TiffRect(900, 2300, 1400, 2800)

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

  inside <- data.frame(i = 1000, j = 1500, Text = "a", ROI_ID = "r1")
  expect_error(fetchTiffDataMasked(exampleTiff(), rect, mask_points = inside), "image was fetched")
})

# Below full resolution the pixel spacing isn't an integer (8651/1082 px at
# resolution 4), so masks must be joined to the signal by bin, not by i/j.
test_that("fetchTiffDataMasked matches masks below full resolution", {
  rect <- TiffRect(900, 2300, 1400, 2800)
  mp <- expand.grid(i = 1000:1200, j = 1500:1700)
  mp$Text <- ifelse(mp$i < 1100, "A", "B")
  mp$ROI_ID <- mp$Text

  for (res in c(2, 4)) {
    m <- suppressMessages(fetchTiffDataMasked(exampleTiff(), rect, resolution = res,
                                              mask_points = mp, selected_channels = 1))
    expect_setequal(unique(m@data$Text), c("A", "B"))
    spacing <- 2^(res - 1) * 1.01
    expect_true(all(m@data$i > 1000 - spacing & m@data$i < 1200 + spacing))
    expect_true(all(m@data$j > 1500 - spacing & m@data$j < 1700 + spacing))
    # about 201 x 201 full-resolution pixels, split in two
    n_px <- nrow(unique(m@data[c("i", "j")]))
    expect_gt(n_px, 0.8 * (201 / 2^(res - 1))^2)
    expect_s4_class(m, "TiffPlotDataMasked")
  }
})
