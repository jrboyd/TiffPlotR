library(testthat)

test_that("TiffRect inherits from TiffShape", {
  rect <- TiffRect(0, 10, 0, 20, name = "r1")
  expect_s4_class(rect, "TiffRect")
  expect_true(is(rect, "TiffShape"))
})

test_that("TiffEllipse constructor and accessors work", {
  ell <- TiffEllipse(x0 = c(10, 20), y0 = c(30, 40), radius_x = 5, radius_y = c(2, 3), name = c("e", "e"))

  expect_s4_class(ell, "TiffEllipse")
  expect_true(is(ell, "TiffShape"))
  expect_equal(ell$width, c(10, 10))
  expect_equal(ell$height, c(4, 6))
  expect_equal(ell@coords$name, c("e_1", "e_2"))

  ell2 <- shape_shift(ell, dx = 1, dy = -2)
  expect_equal(ell2$x0, c(11, 21))
  expect_equal(ell2$y0, c(28, 38))
})

test_that("TiffPolygon constructor and accessors work", {
  poly <- TiffPolygon(
    x = list(c(0, 1, 0), c(10, 12, 11, 10)),
    y = list(c(0, 0, 1), c(10, 10, 12, 13)),
    name = c("p", "p")
  )

  expect_s4_class(poly, "TiffPolygon")
  expect_true(is(poly, "TiffShape"))
  expect_equal(poly$n_points, c(3, 4))
  expect_equal(poly@coords$name, c("p_1", "p_2"))

  poly2 <- shape_shift(poly, dx = 2, dy = 3)
  expect_equal(poly2$x[[1]], c(2, 3, 2))
  expect_equal(poly2$y[[1]], c(3, 3, 4))
})

test_that("Polygon and ellipse combine/subset methods work", {
  e1 <- TiffEllipse(0, 0, 1, 2, name = "e")
  e2 <- TiffEllipse(5, 5, 1, 1, name = "e")
  e12 <- c(e1, e2)
  expect_equal(nrow(e12@coords), 2)
  expect_equal(e12@coords$name, c("e_1", "e_2"))
  expect_equal(nrow(e12[1]@coords), 1)

  p1 <- TiffPolygon(x = c(0, 1, 0), y = c(0, 0, 1), name = "p")
  p2 <- TiffPolygon(x = c(5, 6, 5), y = c(5, 5, 6), name = "p")
  p12 <- c(p1, p2)
  expect_equal(nrow(p12@coords), 2)
  expect_equal(p12@coords$name, c("p_1", "p_2"))
  expect_equal(nrow(p12[2]@coords), 1)
})

test_that("shape_shift rejects non-shape input", {
  expect_error(shape_shift(list(), 1, 1), "shape must be a TiffShape")
})

test_that("shape_center_points works for all shape classes", {
  rect <- TiffRect(0, 10, 0, 20)
  ell <- TiffEllipse(x0 = 5, y0 = 9, radius_x = 2, radius_y = 3)
  poly <- TiffPolygon(x = c(0, 2, 2), y = c(0, 0, 4))

  rc <- shape_center_points(rect)
  ec <- shape_center_points(ell)
  pc <- shape_center_points(poly)

  expect_true(is.data.frame(rc))
  expect_true(is.data.frame(ec))
  expect_true(is.data.frame(pc))

  expect_equal(rc$x, 5)
  expect_equal(rc$y, 10)
  expect_equal(ec$x, 5)
  expect_equal(ec$y, 9)
  expect_equal(pc$x, mean(c(0, 2)))
  expect_equal(pc$y, mean(c(0, 4)))
})

test_that("shape_resize_abs and shape_resize_mult work for each shape", {
  rect <- TiffRect(0, 10, 0, 20)
  rect2 <- shape_resize_abs(rect, width = 20, height = 10)
  expect_equal(rect2$width, 20)
  expect_equal(rect2$height, 10)

  ell <- TiffEllipse(10, 10, radius_x = 5, radius_y = 4)
  ell2 <- shape_resize_mult(ell, fx = 2, fy = 0.5)
  expect_equal(ell2$width, 20)
  expect_equal(ell2$height, 4)

  poly <- TiffPolygon(x = c(0, 2, 1), y = c(0, 0, 2))
  poly2 <- shape_resize_abs(poly, width = 4, height = 8)
  expect_equal(round(max(poly2$x[[1]]) - min(poly2$x[[1]]), 6), 4)
  expect_equal(round(max(poly2$y[[1]]) - min(poly2$y[[1]]), 6), 8)
})

test_that("shape_annotate works for rect, ellipse, and polygon", {
  p <- ggplot2::ggplot()
  rect <- TiffRect(0, 5, 0, 5)
  ell <- TiffEllipse(10, 10, 2, 1)
  poly <- TiffPolygon(x = c(0, 1, 0), y = c(0, 0, 1))

  expect_s3_class(shape_annotate(p, rect), "ggplot")
  expect_s3_class(shape_annotate(p, ell), "ggplot")
  expect_s3_class(shape_annotate(p, poly), "ggplot")
})

test_that("legacy rect wrappers still dispatch to shape generics", {
  rect <- TiffRect(0, 10, 0, 10)
  a <- rect_shift(rect, dx = 2, dy = 3)
  b <- shape_shift(rect, dx = 2, dy = 3)
  expect_equal(a@coords, b@coords)

  a2 <- rect_resize_abs(rect, width = 20, height = 10)
  b2 <- shape_resize_abs(rect, width = 20, height = 10)
  expect_equal(a2@coords, b2@coords)

  p <- ggplot2::ggplot()
  expect_s3_class(rect_annotate(p, rect), "ggplot")
})
