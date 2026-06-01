#' Shift a shape in x/y dimensions
#'
#' @param shape A `TiffShape` object (`TiffRect`, `TiffEllipse`, or `TiffPolygon`)
#' @param dx shift in x direction (numeric)
#' @param dy shift in y direction (numeric)
#' @return shifted object of the same class as input
#' @export
methods::setGeneric("shape_shift", function(shape, dx = 0, dy = 0) {
  standardGeneric("shape_shift")
})

#' Resize shape geometry to absolute dimensions
#'
#' @param shape A `TiffShape` object.
#' @param width Target shape width in x units.
#' @param height Target shape height in y units.
#' @param anchor Anchor point used to keep shape location fixed while resizing.
#' @return Resized shape object.
#' @export
methods::setGeneric("shape_resize_abs", function(shape, width, height, anchor = "center") {
  standardGeneric("shape_resize_abs")
})

#' Resize shape geometry by multiplicative scale factors
#'
#' @param shape A `TiffShape` object.
#' @param fx Width scale factor.
#' @param fy Height scale factor (defaults to `fx`).
#' @param anchor Anchor point used to keep shape location fixed while resizing.
#' @return Resized shape object.
#' @export
methods::setGeneric("shape_resize_mult", function(shape, fx = 1, fy = NULL, anchor = "center") {
  standardGeneric("shape_resize_mult")
})

#' Return shape center points
#'
#' Computes one representative center point per shape row.
#'
#' @param shape A `TiffShape` object.
#' @return data.frame with columns `x` and `y`.
#' @export
methods::setGeneric("shape_center_points", function(shape) {
  standardGeneric("shape_center_points")
})

#' Annotate ggplot objects with shape geometry
#'
#' @param p ggplot object.
#' @param shape A `TiffShape` object.
#' @param color Border/line color.
#' @param fill Fill color.
#' @param alpha Alpha transparency.
#' @param annotate_center If TRUE, annotate center points instead of boundaries.
#' @param n Number of vertices used to draw ellipse boundaries.
#' @param ... Additional arguments passed to ggplot layer calls.
#' @return A ggplot object with added shape annotation layer.
#' @export
methods::setGeneric("shape_annotate", function(p, shape, color = "green", fill = NA, alpha = 0.2, annotate_center = FALSE, n = 120L, ...) {
  standardGeneric("shape_annotate")
})
