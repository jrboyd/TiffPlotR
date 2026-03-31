library(testthat)
library(ggplot2)

# Tests for TiffRect class and rectangle manipulation functions

test_that("TiffRect constructor creates valid object with required parameters", {
  rect <- TiffRect(10, 20, 30, 50)
  expect_s4_class(rect, "TiffRect")
  expect_equal(rect@coords$xmin[[1]], 10)
  expect_equal(rect@coords$xmax[[1]], 20)
  expect_equal(rect@coords$ymin[[1]], 30)
  expect_equal(rect@coords$ymax[[1]], 50)
  expect_equal(rect@coords$name[[1]], "rect")
})

test_that("TiffRect constructor accepts custom name", {
  rect <- TiffRect(0, 100, 0, 100, name = "roi1")
  expect_equal(rect@coords$name[[1]], "roi1")
})

test_that("TiffRect constructor fails with invalid coordinates", {
  expect_error(TiffRect(20, 10, 30, 50), "xmin must be < xmax")
  expect_error(TiffRect(10, 20, 50, 30), "ymin must be < ymax")
  expect_error(TiffRect(10, 10, 30, 50), "xmin must be < xmax")
})

test_that("TiffRect constructor fails with non-numeric coordinates", {
  expect_error(TiffRect("a", 20, 30, 50), "Coordinates must be numeric")
  expect_error(TiffRect(10, "b", 30, 50), "Coordinates must be numeric")
})

test_that("TiffRect constructor supports vectorized rectangles", {
  rect <- TiffRect(c(10, 15), c(20, 25), c(30, 35), c(50, 55), name = c("a", "b"))
  expect_equal(nrow(rect@coords), 2)
  expect_equal(rect@coords$name, c("a", "b"))
})

test_that("TiffRect constructor makes duplicate names unique", {
  rect <- TiffRect(c(10, 15, 20), c(20, 25, 30), c(30, 35, 40), c(50, 55, 60), name = c("roi", "roi", "roi"))
  expect_equal(rect@coords$name, c("roi_1", "roi_2", "roi_3"))
  expect_equal(length(unique(rect@coords$name)), 3)
})

test_that("TiffRect constructor rejects incompatible vector lengths", {
  expect_error(TiffRect(c(10, 15), c(20, 25, 30), 30, 50), "Coordinate lengths must all be 1 or match the longest input")
  expect_error(TiffRect(c(10, 15), c(20, 25), c(30, 35), c(50, 55), name = c("a", "b", "c")), "name must be length 1 or match coordinate length")
})

test_that("TiffRect show method displays correctly", {
  rect <- TiffRect(10, 20, 30, 50, name = "test")
  expect_output(show(rect), "TiffRect with 1 rectangle")
  expect_output(show(rect), "xmin")
  expect_output(show(rect), "xmax")
  expect_output(show(rect), "ymin")
  expect_output(show(rect), "ymax")
  expect_output(show(rect), "test")
})

test_that("rect_shift shifts rectangle by dx and dy", {
  rect <- TiffRect(10, 20, 30, 50)
  shifted <- rect_shift(rect, dx = 5, dy = -2)
  expect_equal(shifted@coords$xmin[[1]], 15)
  expect_equal(shifted@coords$xmax[[1]], 25)
  expect_equal(shifted@coords$ymin[[1]], 28)
  expect_equal(shifted@coords$ymax[[1]], 48)
  expect_equal(shifted@coords$name[[1]], "rect")
})

test_that("vectorized operations preserve row count", {
  rect <- TiffRect(c(0, 10), c(5, 20), c(0, 10), c(5, 20), name = c("r1", "r2"))
  shifted <- rect_shift(rect, dx = 1, dy = -1)
  resized <- rect_resize_mult(shifted, fx = 2)

  expect_equal(nrow(resized@coords), 2)
  expect_equal(resized@coords$name, c("r1", "r2"))
})

test_that("rect_shift with defaults doesn't modify rectangle", {
  rect <- TiffRect(10, 20, 30, 50)
  shifted <- rect_shift(rect)
  expect_equal(shifted@coords$xmin[[1]], rect@coords$xmin[[1]])
  expect_equal(shifted@coords$xmax[[1]], rect@coords$xmax[[1]])
  expect_equal(shifted@coords$ymin[[1]], rect@coords$ymin[[1]])
  expect_equal(shifted@coords$ymax[[1]], rect@coords$ymax[[1]])
})

test_that("rect_shift with only dx shifts x coordinates", {
  rect <- TiffRect(10, 20, 30, 50)
  shifted <- rect_shift(rect, dx = 5)
  expect_equal(shifted@coords$xmin[[1]], 15)
  expect_equal(shifted@coords$xmax[[1]], 25)
  expect_equal(shifted@coords$ymin[[1]], 30)
  expect_equal(shifted@coords$ymax[[1]], 50)
})

test_that("rect_shift with only dy shifts y coordinates", {
  rect <- TiffRect(10, 20, 30, 50)
  shifted <- rect_shift(rect, dy = 10)
  expect_equal(shifted@coords$xmin[[1]], 10)
  expect_equal(shifted@coords$xmax[[1]], 20)
  expect_equal(shifted@coords$ymin[[1]], 40)
  expect_equal(shifted@coords$ymax[[1]], 60)
})

test_that("rect_shift fails with non-TiffRect input", {
  expect_error(rect_shift(list(), dx = 5), "rect must be a TiffRect")
  expect_error(rect_shift(data.frame(x = 1:10), dx = 5), "rect must be a TiffRect")
})

test_that("rect_shift with negative values works correctly", {
  rect <- TiffRect(10, 20, 30, 50)
  shifted <- rect_shift(rect, dx = -5, dy = -10)
  expect_equal(shifted@coords$xmin[[1]], 5)
  expect_equal(shifted@coords$xmax[[1]], 15)
  expect_equal(shifted@coords$ymin[[1]], 20)
  expect_equal(shifted@coords$ymax[[1]], 40)
})

test_that("rect_resize_abs with default center anchor resizes symmetrically", {
  rect <- TiffRect(10, 30, 20, 40)
  # center is at (20, 30)
  resized <- rect_resize_abs(rect, width = 10, height = 10)
  expect_equal(resized@coords$xmin[[1]], 15)
  expect_equal(resized@coords$xmax[[1]], 25)
  expect_equal(resized@coords$ymin[[1]], 25)
  expect_equal(resized@coords$ymax[[1]], 35)
})

test_that("rect_resize_abs with topleft anchor", {
  rect <- TiffRect(10, 30, 20, 40)
  resized <- rect_resize_abs(rect, width = 40, height = 10, anchor = "topleft")
  expect_equal(resized@coords$xmin[[1]], 10)
  expect_equal(resized@coords$xmax[[1]], 50)
  expect_equal(resized@coords$ymin[[1]], 20)
  expect_equal(resized@coords$ymax[[1]], 30)
})

test_that("rect_resize_abs with topright anchor", {
  rect <- TiffRect(10, 30, 20, 40)
  resized <- rect_resize_abs(rect, width = 20, height = 10, anchor = "topright")
  expect_equal(resized@coords$xmin[[1]], 10)
  expect_equal(resized@coords$xmax[[1]], 30)
  expect_equal(resized@coords$ymin[[1]], 20)
  expect_equal(resized@coords$ymax[[1]], 30)
})

test_that("rect_resize_abs with botleft anchor", {
  rect <- TiffRect(10, 30, 20, 40)
  resized <- rect_resize_abs(rect, width = 10, height = 20, anchor = "botleft")
  expect_equal(resized@coords$xmin[[1]], 10)
  expect_equal(resized@coords$xmax[[1]], 20)
  expect_equal(resized@coords$ymin[[1]], 20)
  expect_equal(resized@coords$ymax[[1]], 40)
})

test_that("rect_resize_abs with botright anchor", {
  rect <- TiffRect(10, 30, 20, 40)
  resized <- rect_resize_abs(rect, width = 10, height = 20, anchor = "botright")
  expect_equal(resized@coords$xmin[[1]], 20)
  expect_equal(resized@coords$xmax[[1]], 30)
  expect_equal(resized@coords$ymin[[1]], 20)
  expect_equal(resized@coords$ymax[[1]], 40)
})

test_that("rect_resize_abs fails with invalid anchor", {
  rect <- TiffRect(10, 30, 20, 40)
  expect_error(
    rect_resize_abs(rect, width = 10, height = 10, anchor = "xmin"),
    "anchor must be one of"
  )
})

test_that("rect_resize_abs fails with non-TiffRect input", {
  expect_error(rect_resize_abs(list(), width = 10, height = 10), "rect must be a TiffRect")
})

test_that("rect_resize_abs coerces numeric inputs", {
  rect <- TiffRect(10, 30, 20, 40)
  resized <- rect_resize_abs(rect, width = "10", height = "10")
  expect_equal(resized@coords$xmax[[1]] - resized@coords$xmin[[1]], 10)
  expect_equal(resized@coords$ymax[[1]] - resized@coords$ymin[[1]], 10)
})

test_that("rect_resize_mult doubles rectangle size with fx=2", {
  rect <- TiffRect(10, 20, 30, 50)
  # width = 10, height = 20
  resized <- rect_resize_mult(rect, fx = 2)
  expect_equal(resized@coords$xmax[[1]] - resized@coords$xmin[[1]], 20)
  expect_equal(resized@coords$ymax[[1]] - resized@coords$ymin[[1]], 40)
})

test_that("rect_resize_mult halves rectangle size with fx=0.5", {
  rect <- TiffRect(10, 20, 30, 50)
  resized <- rect_resize_mult(rect, fx = 0.5)
  expect_equal(resized@coords$xmax[[1]] - resized@coords$xmin[[1]], 5)
  expect_equal(resized@coords$ymax[[1]] - resized@coords$ymin[[1]], 10)
})

test_that("rect_resize_mult with separate fx and fy multipliers", {
  rect <- TiffRect(10, 20, 30, 50)
  # width = 10, height = 20
  resized <- rect_resize_mult(rect, fx = 2, fy = 0.5)
  expect_equal(resized@coords$xmax[[1]] - resized@coords$xmin[[1]], 20)
  expect_equal(resized@coords$ymax[[1]] - resized@coords$ymin[[1]], 10)
})

test_that("rect_resize_mult with only fx uses fx for both dimensions", {
  rect <- TiffRect(10, 20, 30, 50)
  resized <- rect_resize_mult(rect, fx = 2)
  expect_equal(resized@coords$xmax[[1]] - resized@coords$xmin[[1]], 20)
  expect_equal(resized@coords$ymax[[1]] - resized@coords$ymin[[1]], 40)
})

test_that("rect_resize_mult fails with non-TiffRect input", {
  expect_error(rect_resize_mult(list(), fx = 2), "rect must be a TiffRect")
})

test_that("rect_resize_mult maintains center position", {
  rect <- TiffRect(10, 20, 30, 50)
  center_x <- (rect@coords$xmin[[1]] + rect@coords$xmax[[1]]) / 2
  center_y <- (rect@coords$ymin[[1]] + rect@coords$ymax[[1]]) / 2
  
  resized <- rect_resize_mult(rect, fx = 2, fy = 0.5)
  resized_center_x <- (resized@coords$xmin[[1]] + resized@coords$xmax[[1]]) / 2
  resized_center_y <- (resized@coords$ymin[[1]] + resized@coords$ymax[[1]]) / 2
  
  expect_equal(resized_center_x, center_x)
  expect_equal(resized_center_y, center_y)
})

test_that("rect_annotate adds rectangle layer to ggplot", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  p <- ggplot(df, aes(x = x, y = y)) + geom_point()
  rect <- TiffRect(2, 8, -1, 1)
  
  p_annotated <- rect_annotate(p, rect)
  expect_s3_class(p_annotated, "ggplot")
  expect_length(p_annotated$layers, 2)  # original layer + rectangle
})

test_that("rect_annotate uses custom colors", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  p <- ggplot(df, aes(x = x, y = y)) + geom_point()
  rect <- TiffRect(2, 8, -1, 1)
  
  p_annotated <- rect_annotate(p, rect, color = "blue", fill = "yellow")
  expect_s3_class(p_annotated, "ggplot")
  expect_length(p_annotated$layers, 2)
})

test_that("rect_annotate uses default color when not specified", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  p <- ggplot(df, aes(x = x, y = y)) + geom_point()
  rect <- TiffRect(2, 8, -1, 1)
  
  p_annotated <- rect_annotate(p, rect)
  expect_s3_class(p_annotated, "ggplot")
})

test_that("rect_annotate fails with non-TiffRect input", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  p <- ggplot(df, aes(x = x, y = y)) + geom_point()
  
  expect_error(rect_annotate(p, list()), "rect must be a TiffRect")
})

test_that("rect_annotate fails without ggplot object", {
  rect <- TiffRect(2, 8, -1, 1)
  
  expect_error(rect_annotate(rect = rect), "p must be a ggplot object")
})

test_that("chained rectangle operations work correctly", {
  rect <- TiffRect(10, 20, 30, 40)
  
  # Apply multiple operations
  result <- rect %>%
    rect_shift(dx = 5, dy = -5) %>%
    rect_resize_mult(fx = 2) %>%
    rect_shift(dx = -10)
  
  expect_s4_class(result, "TiffRect")
  expect_true(result@coords$xmin[[1]] < result@coords$xmax[[1]])
  expect_true(result@coords$ymin[[1]] < result@coords$ymax[[1]])
})

test_that("rect_intersection_region returns overlap rectangle", {
  rect_a <- TiffRect(0, 10, 0, 10, name = "a")
  rect_b <- TiffRect(5, 15, 2, 8, name = "b")

  out <- rect_intersection_region(rect_a, rect_b)
  expect_s4_class(out, "TiffRect")
  expect_equal(out@coords$xmin[[1]], 5)
  expect_equal(out@coords$xmax[[1]], 10)
  expect_equal(out@coords$ymin[[1]], 2)
  expect_equal(out@coords$ymax[[1]], 8)
  expect_equal(out@coords$name[[1]], "a_intersect_b")
})

test_that("rect_intersection_region returns NULL for non-overlap", {
  rect_a <- TiffRect(0, 2, 0, 2)
  rect_b <- TiffRect(3, 5, 3, 5)

  expect_null(rect_intersection_region(rect_a, rect_b))
})

test_that("rect_intersection_region supports vectorized pairwise intersection", {
  rect_a <- TiffRect(c(0, 0), c(4, 2), c(0, 0), c(4, 2), name = c("a1", "a2"))
  rect_b <- TiffRect(c(1, 3), c(3, 4), c(1, 3), c(3, 4), name = c("b1", "b2"))

  out <- rect_intersection_region(rect_a, rect_b)
  expect_s4_class(out, "TiffRect")
  expect_equal(nrow(out@coords), 1)
  expect_equal(out@coords$name[[1]], "a1_intersect_b1")
})

test_that("rect_test_overlap returns logical and subsets correctly", {
  r1 <- TiffRect(c(0, 10), c(5, 15), c(0, 10), c(5, 15), name = c("a", "b"))
  r2 <- TiffRect(3, 8, 3, 8, name = "x")

  result <- rect_test_overlap(r1, r2)
  expect_equal(result, c(TRUE, FALSE))

  sub <- rect_test_overlap(r1, r2, subset = TRUE)
  expect_s4_class(sub, "TiffRect")
  expect_equal(nrow(sub@coords), 1)
  expect_equal(sub@coords$name[[1]], "a")

  # non-overlapping pair returns NULL when subsetting
  separate <- TiffRect(20, 30, 20, 30, name = "far")
  expect_null(rect_test_overlap(r1, separate, subset = TRUE))
})

test_that("rect_test_contains returns logical and subsets correctly", {
  r1 <- TiffRect(c(1, 0), c(9, 20), c(1, 0), c(9, 20), name = c("a", "b"))
  container <- TiffRect(0, 10, 0, 10, name = "container")

  result <- rect_test_contains(r1, container)
  expect_equal(result, c(TRUE, FALSE))

  sub <- rect_test_contains(r1, container, subset = TRUE)
  expect_s4_class(sub, "TiffRect")
  expect_equal(nrow(sub@coords), 1)
  expect_equal(sub@coords$name[[1]], "a")

  # fully outside returns NULL when subsetting
  outside <- TiffRect(100, 200, 100, 200, name = "out")
  r_out <- TiffRect(50, 150, 50, 150, name = "r")
  expect_null(rect_test_contains(r_out, container, subset = TRUE))
})
