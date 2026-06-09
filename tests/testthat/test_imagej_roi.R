library(testthat)

.write_uint16_be <- function(buf, offset, value) {
  stopifnot(length(value) == 1)
  value <- as.integer(value)
  buf[offset] <- as.raw(bitwShiftR(value, 8) %% 256L)
  buf[offset + 1] <- as.raw(value %% 256L)
  buf
}

.make_imagej_roi_raw <- function(type_code,
                                 top,
                                 left,
                                 bottom,
                                 right,
                                 x_rel = integer(),
                                 y_rel = integer()) {
  n_coords <- length(x_rel)
  stopifnot(length(y_rel) == n_coords)

  buf <- raw(64 + 4 * n_coords)
  buf[1:4] <- charToRaw("Iout")
  buf <- .write_uint16_be(buf, 5, 227)
  buf[7] <- as.raw(type_code)
  buf <- .write_uint16_be(buf, 9, top)
  buf <- .write_uint16_be(buf, 11, left)
  buf <- .write_uint16_be(buf, 13, bottom)
  buf <- .write_uint16_be(buf, 15, right)
  buf <- .write_uint16_be(buf, 17, n_coords)

  if (n_coords > 0) {
    for (i in seq_len(n_coords)) {
      buf <- .write_uint16_be(buf, 65 + 2 * (i - 1), x_rel[[i]])
      buf <- .write_uint16_be(buf, 65 + 2 * n_coords + 2 * (i - 1), y_rel[[i]])
    }
  }

  buf
}

.write_roi_file <- function(path, raw_roi) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(raw_roi, con)
  path
}

test_that("readImageJRois parses supported single ROI files", {
  tmp <- tempfile(fileext = ".roi")
  .write_roi_file(tmp, .make_imagej_roi_raw(type_code = 1, top = 20, left = 10, bottom = 70, right = 60))

  res <- readImageJRois(tmp)

  expect_true(is.list(res))
  expect_s4_class(res$rects, "TiffRect")
  expect_equal(res$rects@coords$xmin[[1]], 10)
  expect_equal(res$rects@coords$xmax[[1]], 60)
  expect_equal(res$rects@coords$ymin[[1]], 20)
  expect_equal(res$rects@coords$ymax[[1]], 70)
  expect_null(res$ellipses)
  expect_null(res$polygons)
  expect_equal(nrow(res$unsupported), 0)
})

test_that("readImageJRois maps ovals and polygons to Tiff shapes", {
  oval_path <- tempfile(fileext = ".roi")
  poly_path <- tempfile(fileext = ".roi")

  .write_roi_file(oval_path, .make_imagej_roi_raw(type_code = 2, top = 40, left = 10, bottom = 80, right = 50))
  .write_roi_file(
    poly_path,
    .make_imagej_roi_raw(
      type_code = 0,
      top = 100,
      left = 200,
      bottom = 130,
      right = 240,
      x_rel = c(0, 30, 10),
      y_rel = c(0, 0, 20)
    )
  )

  oval_res <- readImageJRois(oval_path)
  poly_res <- readImageJRois(poly_path)

  expect_s4_class(oval_res$ellipses, "TiffEllipse")
  expect_equal(oval_res$ellipses@coords$x0[[1]], 30)
  expect_equal(oval_res$ellipses@coords$y0[[1]], 60)
  expect_equal(oval_res$ellipses@coords$radius_x[[1]], 20)
  expect_equal(oval_res$ellipses@coords$radius_y[[1]], 20)

  expect_s4_class(poly_res$polygons, "TiffPolygon")
  expect_equal(poly_res$polygons@coords$x[[1]], c(200, 230, 210))
  expect_equal(poly_res$polygons@coords$y[[1]], c(100, 100, 120))
})

test_that("readImageJRois reads zip exports with multiple ROI files", {
  zip_cmd <- Sys.which("zip")
  if (!nzchar(zip_cmd) && !nzchar(Sys.getenv("R_ZIPCMD"))) {
    skip("zip executable not available")
  }

  dir_path <- tempfile()
  dir.create(dir_path)
  rect_path <- file.path(dir_path, "rect.roi")
  poly_path <- file.path(dir_path, "poly.roi")
  zip_path <- tempfile(fileext = ".zip")

  .write_roi_file(rect_path, .make_imagej_roi_raw(type_code = 1, top = 1, left = 2, bottom = 5, right = 7))
  .write_roi_file(
    poly_path,
    .make_imagej_roi_raw(type_code = 0, top = 10, left = 10, bottom = 20, right = 20, x_rel = c(0, 5, 0), y_rel = c(0, 0, 5))
  )

  old_wd <- getwd()
  setwd(dir_path)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(zipfile = zip_path, files = c("rect.roi", "poly.roi"), flags = "-j")

  res <- readImageJRois(zip_path)

  expect_s4_class(res$rects, "TiffRect")
  expect_s4_class(res$polygons, "TiffPolygon")
  expect_equal(nrow(res$roi_info), 2)
})

test_that("readImageJRois reports unsupported ROI types", {
  tmp <- tempfile(fileext = ".roi")
  .write_roi_file(tmp, .make_imagej_roi_raw(type_code = 3, top = 0, left = 0, bottom = 1, right = 1))

  res <- readImageJRois(tmp)

  expect_null(res$rects)
  expect_equal(nrow(res$unsupported), 1)
  expect_false(res$roi_info$supported[[1]])
  expect_match(res$unsupported$reason[[1]], "not currently supported")
})

test_that("readImageJRois accepts a vector of ROI file paths", {
  rect_path <- tempfile(fileext = ".roi")
  oval_path <- tempfile(fileext = ".roi")

  .write_roi_file(rect_path, .make_imagej_roi_raw(type_code = 1, top = 5, left = 7, bottom = 25, right = 30))
  .write_roi_file(oval_path, .make_imagej_roi_raw(type_code = 2, top = 40, left = 10, bottom = 80, right = 50))

  res <- readImageJRois(c(rect_path, oval_path))

  expect_equal(nrow(res$roi_info), 2)
  expect_s4_class(res$rects, "TiffRect")
  expect_s4_class(res$ellipses, "TiffEllipse")
  expect_equal(res$rects@coords$xmin[[1]], 7)
  expect_equal(res$ellipses@coords$x0[[1]], 30)
})