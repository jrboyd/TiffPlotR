library(testthat)

# dplyr is imported, not attached: a fetch with precalc_max must work without the
# caller attaching dplyr (unqualified `ungroup` once broke this).
test_that("fetchTiffData accepts precalc_max without dplyr attached", {
  expect_false("package:dplyr" %in% search())
  rect <- TiffRect(0, 100, 0, 100)
  pm <- data.frame(channel = 1:3, min_value = 0, max_value = 1000)
  img <- fetchTiffData(exampleTiff(), rect = rect, precalc_max = pm)
  expect_s4_class(img, "TiffPlotData")
})

test_that("precalc_max with per-resolution rows and a grouped tibble is accepted", {
  rect <- TiffRect(0, 100, 0, 100)
  pm <- data.frame(channel = rep(1:3, 2), resolution = rep(1:2, each = 3),
                   min_value = 0, max_value = rep(c(1000, 2000), each = 3))
  pm <- dplyr::group_by(tibble::as_tibble(pm), resolution)
  img <- fetchTiffData(exampleTiff(), rect = rect, precalc_max = pm, resolution = 1)
  expect_s4_class(img, "TiffPlotData")
})
