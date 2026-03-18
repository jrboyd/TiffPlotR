library(testthat)
library(ggplot2)

# Tests for TiffRect class and rectangle manipulation functions

test_that("TiffRect constructor creates valid object with required parameters", {
  rect <- TiffRect(10, 20, 30, 50)
  expect_s4_class(rect, "TiffRect")
  expect_equal(rect@xmin, 10)
  expect_equal(rect@xmax, 20)
  expect_equal(rect@ymin, 30)
  expect_equal(rect@ymax, 50)
  expect_equal(rect@name, "rect")
})

test_that("TiffRect constructor accepts custom name", {
  rect <- TiffRect(0, 100, 0, 100, name = "roi1")
  expect_equal(rect@name, "roi1")
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

test_that("TiffRect constructor rejects vectors of length > 1", {
  expect_error(TiffRect(c(10, 15), 20, 30, 50), "All coordinates must be length 1")
  expect_error(TiffRect(10, c(20, 25), 30, 50), "All coordinates must be length 1")
})

test_that("TiffRect show method displays correctly", {
  rect <- TiffRect(10, 20, 30, 50, name = "test")
  expected_output <- "TiffRect: test"
  expect_output(show(rect), "TiffRect: test")
  expect_output(show(rect), "xmin: 10")
  expect_output(show(rect), "xmax: 20")
  expect_output(show(rect), "ymin: 30")
  expect_output(show(rect), "ymax: 50")
})

test_that("rect_shift shifts rectangle by dx and dy", {
  rect <- TiffRect(10, 20, 30, 50)
  shifted <- rect_shift(rect, dx = 5, dy = -2)
  expect_equal(shifted@xmin, 15)
  expect_equal(shifted@xmax, 25)
  expect_equal(shifted@ymin, 28)
  expect_equal(shifted@ymax, 48)
  expect_equal(shifted@name, "rect")
})

test_that("rect_shift with defaults doesn't modify rectangle", {
  rect <- TiffRect(10, 20, 30, 50)
  shifted <- rect_shift(rect)
  expect_equal(shifted@xmin, rect@xmin)
  expect_equal(shifted@xmax, rect@xmax)
  expect_equal(shifted@ymin, rect@ymin)
  expect_equal(shifted@ymax, rect@ymax)
})

test_that("rect_shift with only dx shifts x coordinates", {
  rect <- TiffRect(10, 20, 30, 50)
  shifted <- rect_shift(rect, dx = 5)
  expect_equal(shifted@xmin, 15)
  expect_equal(shifted@xmax, 25)
  expect_equal(shifted@ymin, 30)
  expect_equal(shifted@ymax, 50)
})

test_that("rect_shift with only dy shifts y coordinates", {
  rect <- TiffRect(10, 20, 30, 50)
  shifted <- rect_shift(rect, dy = 10)
  expect_equal(shifted@xmin, 10)
  expect_equal(shifted@xmax, 20)
  expect_equal(shifted@ymin, 40)
  expect_equal(shifted@ymax, 60)
})

test_that("rect_shift fails with non-TiffRect input", {
  expect_error(rect_shift(list(), dx = 5), "rect must be a TiffRect")
  expect_error(rect_shift(data.frame(x = 1:10), dx = 5), "rect must be a TiffRect")
})

test_that("rect_shift with negative values works correctly", {
  rect <- TiffRect(10, 20, 30, 50)
  shifted <- rect_shift(rect, dx = -5, dy = -10)
  expect_equal(shifted@xmin, 5)
  expect_equal(shifted@xmax, 15)
  expect_equal(shifted@ymin, 20)
  expect_equal(shifted@ymax, 40)
})

test_that("rect_resize_abs with default center anchor resizes symmetrically", {
  rect <- TiffRect(10, 30, 20, 40)
  # center is at (20, 30)
  resized <- rect_resize_abs(rect, width = 10, height = 10)
  expect_equal(resized@xmin, 15)
  expect_equal(resized@xmax, 25)
  expect_equal(resized@ymin, 25)
  expect_equal(resized@ymax, 35)
})

test_that("rect_resize_abs with topleft anchor", {
  rect <- TiffRect(10, 30, 20, 40)
  resized <- rect_resize_abs(rect, width = 40, height = 10, anchor = "topleft")
  expect_equal(resized@xmin, 10)
  expect_equal(resized@xmax, 50)
  expect_equal(resized@ymin, 20)
  expect_equal(resized@ymax, 30)
})

test_that("rect_resize_abs with topright anchor", {
  rect <- TiffRect(10, 30, 20, 40)
  resized <- rect_resize_abs(rect, width = 20, height = 10, anchor = "topright")
  expect_equal(resized@xmin, 10)
  expect_equal(resized@xmax, 30)
  expect_equal(resized@ymin, 20)
  expect_equal(resized@ymax, 30)
})

test_that("rect_resize_abs with botleft anchor", {
  rect <- TiffRect(10, 30, 20, 40)
  resized <- rect_resize_abs(rect, width = 10, height = 20, anchor = "botleft")
  expect_equal(resized@xmin, 10)
  expect_equal(resized@xmax, 20)
  expect_equal(resized@ymin, 20)
  expect_equal(resized@ymax, 40)
})

test_that("rect_resize_abs with botright anchor", {
  rect <- TiffRect(10, 30, 20, 40)
  resized <- rect_resize_abs(rect, width = 10, height = 20, anchor = "botright")
  expect_equal(resized@xmin, 20)
  expect_equal(resized@xmax, 30)
  expect_equal(resized@ymin, 20)
  expect_equal(resized@ymax, 40)
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
  expect_equal(resized@xmax - resized@xmin, 10)
  expect_equal(resized@ymax - resized@ymin, 10)
})

test_that("rect_resize_mult doubles rectangle size with fx=2", {
  rect <- TiffRect(10, 20, 30, 50)
  # width = 10, height = 20
  resized <- rect_resize_mult(rect, fx = 2)
  expect_equal(resized@xmax - resized@xmin, 20)
  expect_equal(resized@ymax - resized@ymin, 40)
})

test_that("rect_resize_mult halves rectangle size with fx=0.5", {
  rect <- TiffRect(10, 20, 30, 50)
  resized <- rect_resize_mult(rect, fx = 0.5)
  expect_equal(resized@xmax - resized@xmin, 5)
  expect_equal(resized@ymax - resized@ymin, 10)
})

test_that("rect_resize_mult with separate fx and fy multipliers", {
  rect <- TiffRect(10, 20, 30, 50)
  # width = 10, height = 20
  resized <- rect_resize_mult(rect, fx = 2, fy = 0.5)
  expect_equal(resized@xmax - resized@xmin, 20)
  expect_equal(resized@ymax - resized@ymin, 10)
})

test_that("rect_resize_mult with only fx uses fx for both dimensions", {
  rect <- TiffRect(10, 20, 30, 50)
  resized <- rect_resize_mult(rect, fx = 2)
  expect_equal(resized@xmax - resized@xmin, 20)
  expect_equal(resized@ymax - resized@ymin, 40)
})

test_that("rect_resize_mult fails with non-TiffRect input", {
  expect_error(rect_resize_mult(list(), fx = 2), "rect must be a TiffRect")
})

test_that("rect_resize_mult maintains center position", {
  rect <- TiffRect(10, 20, 30, 50)
  center_x <- (rect@xmin + rect@xmax) / 2
  center_y <- (rect@ymin + rect@ymax) / 2
  
  resized <- rect_resize_mult(rect, fx = 2, fy = 0.5)
  resized_center_x <- (resized@xmin + resized@xmax) / 2
  resized_center_y <- (resized@ymin + resized@ymax) / 2
  
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
  expect_true(result@xmin < result@xmax)
  expect_true(result@ymin < result@ymax)
})
