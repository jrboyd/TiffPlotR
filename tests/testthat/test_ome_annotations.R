library(testthat)

sample_ome_xml <- function() {
  xml2::read_xml(
    paste0(
      "<OME>",
      "  <ROI ID='ROI:0'><Union>",
      "    <Label ID='Label:0' Text='A' X='10' Y='20'/>",
      "    <Polygon ID='Poly:0' Points='1,1 2,1 2,2'/>",
      "    <Ellipse ID='Ellipse:0' X='5' Y='6' RadiusX='2' RadiusY='3'/>",
      "    <Mask ID='Mask:0' X='10' Y='20' Width='2' Height='2'>gA==</Mask>",
      "  </Union></ROI>",
      "  <ROI ID='ROI:1'><Union>",
      "    <Label ID='Label:1' Text='B' X='100' Y='200'/>",
      "  </Union></ROI>",
      "</OME>"
    )
  )
}

test_that("fetchTiffAnnotations extracts ROI-linked shapes", {
  ann <- fetchTiffAnnotations(ome_doc = sample_ome_xml(), include_summary = TRUE)

  expect_true(is.list(ann))
  expect_true(all(c("summary", "roi", "labels", "polygons", "ellipses", "masks", "mask_points") %in% names(ann)))

  expect_equal(nrow(ann$roi), 2)
  expect_equal(nrow(ann$labels), 2)
  expect_equal(nrow(ann$polygons), 1)
  expect_equal(nrow(ann$ellipses), 1)
  expect_equal(nrow(ann$masks), 1)
  expect_true("ROI_ID" %in% colnames(ann$labels))
})

test_that("fetchTiffAnnotations optionally decodes masks", {
  ann <- fetchTiffAnnotations(ome_doc = sample_ome_xml(), decode_masks = TRUE)
  expect_true(is.data.frame(ann$mask_points))
  expect_equal(nrow(ann$mask_points), 1)
  expect_equal(ann$mask_points$x[[1]], 10)
  expect_equal(ann$mask_points$y[[1]], 20)
})

test_that("decode_ome_masks returns matrix and points outputs", {
  mask_df <- data.frame(
    node_text = "gA==",
    Width = 2,
    Height = 2,
    X = 10,
    Y = 20,
    ROI_ID = "ROI:0",
    stringsAsFactors = FALSE
  )

  res_matrix <- decode_ome_masks(mask_df, output = "matrix")
  expect_true("mask_matrix" %in% colnames(res_matrix))
  expect_true(is.matrix(res_matrix$mask_matrix[[1]]))

  res_points <- decode_ome_masks(mask_df, output = "points")
  expect_true(is.data.frame(res_points))
  expect_equal(nrow(res_points), 1)
  expect_true("ROI_ID" %in% colnames(res_points))
})

test_that("decode_ome_masks validates required columns", {
  bad_df <- data.frame(node_text = "gA==", Width = 2, Height = 2)
  expect_error(decode_ome_masks(bad_df), "missing required columns")
})
