library(testthat)

test_that("select_informative_regions_lowres returns TiffRect", {
  tiff_path <- exampleTiff()
  rect <- select_informative_regions_lowres(
    tiff_path,
    rect_width = 200,
    rect_height = 200,
    n_each = 1,
    output = "rect"
  )

  expect_s4_class(rect, "TiffRect")
  expect_true(nrow(rect@coords) >= 1)
})

test_that("select_informative_regions_lowres can return scores table", {
  tiff_path <- exampleTiff()
  out <- select_informative_regions_lowres(
    tiff_path,
    rect_width = 200,
    rect_height = 200,
    n_each = 1,
    output = "list"
  )

  expect_true(is.list(out))
  expect_true(all(c("rect", "scores") %in% names(out)))
  expect_s4_class(out$rect, "TiffRect")
  expect_true(is.data.frame(out$scores))
  expect_true(all(c("score", "group", "name") %in% colnames(out$scores)))
})
