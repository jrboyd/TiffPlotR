library(testthat)

test_that(".label_matrix_polygons traces large label ids with offsets", {
  skip_if_not_installed("EBImage")
  m <- matrix(0, 10, 8)
  m[2:4, 2:4] <- 1e8          # 3x3 block
  m[6:9, 3:6] <- 1e8 + 5      # 4x4 block
  m[10, 8] <- 7               # 1-px sliver -> dropped
  expect_message(poly <- .label_matrix_polygons(m, x0 = 100, y0 = 50), "1 label")
  expect_s4_class(poly, "TiffPolygon")
  expect_setequal(poly$name, c("100000000", "100000005"))
  p1 <- poly[poly$name == "100000000"]
  expect_equal(range(p1$x[[1]]), c(101, 103))  # 0-based col 1..3 + 100
  expect_equal(range(p1$y[[1]]), c(51, 53))
})

test_that(".label_matrix_polygons returns NULL for an empty crop", {
  skip_if_not_installed("EBImage")
  expect_message(res <- .label_matrix_polygons(matrix(0, 4, 4)), "no labels")
  expect_null(res)
})

test_that("label_polygons reads a label TIFF crop, with mask_channel", {
  skip_if_not_installed("EBImage")
  lab <- matrix(0, 20, 20)
  lab[3:8, 3:8] <- 11
  lab[12:18, 10:17] <- 22
  mask <- matrix(0, 20, 20)
  mask[4:6, 4:6] <- 1          # only part of label 11; none of 22
  arr <- array(c(lab, mask), dim = c(20, 20, 2))
  f <- tempfile(fileext = ".ome.tiff")
  ok <- tryCatch({
    RBioFormats::write.image(RBioFormats::AnnotatedImage(arr), f, pixelType = "uint16")
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(ok, "RBioFormats::write.image unavailable")

  poly <- label_polygons(f, TiffRect(0, 20, 0, 20))
  expect_setequal(poly$name, c("11", "22"))
  p11 <- poly[poly$name == "11"]
  expect_equal(range(p11$x[[1]]), c(2, 7))

  # crop offset: rect starting at 10 shifts back to full-image coords
  poly_c <- label_polygons(f, TiffRect(10, 20, 8, 20))
  expect_equal(poly_c$name, "22")
  expect_equal(range(poly_c$x[[1]]), c(11, 17))

  nuc <- label_polygons(f, TiffRect(0, 20, 0, 20), channel = 1, mask_channel = 2)
  expect_equal(nuc$name, "11")
  expect_equal(range(nuc$x[[1]]), c(3, 5))
})
