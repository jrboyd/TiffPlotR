#' Test which shapes overlap a rectangle (bounding-box test)
#'
#' Uses each shape row's bounding box (via the shape's own bbox) and tests
#' overlap against a single query `TiffRect`. This is a fast filter suitable for
#' restricting a large set of shapes (e.g. cell polygons) to a plotting window;
#' it does not clip geometry.
#'
#' @param shape A `TiffShape` (`TiffPolygon`, `TiffEllipse`, `TiffRect`).
#' @param rect A single-row `TiffRect` query window.
#' @param subset If `FALSE` (default) return a logical vector, one per shape row.
#'   If `TRUE` return the subset of `shape` whose bounding boxes overlap `rect`.
#' @return logical vector, or a subset `TiffShape` when `subset = TRUE`
#'   (`NULL` if nothing overlaps).
#' @export
shape_overlaps_rect <- function(shape, rect, subset = FALSE) {
  if (!is(shape, "TiffShape")) stop("shape must be a TiffShape", call. = FALSE)
  if (!is(rect, "TiffRect")) stop("rect must be a TiffRect", call. = FALSE)
  if (nrow(rect@coords) != 1L) stop("rect must be a single rectangle", call. = FALSE)

  bb <- .shape_bbox(shape)
  q  <- rect@coords[1L, ]
  has <- (pmin(bb$xmax, q$xmax) > pmax(bb$xmin, q$xmin)) &
         (pmin(bb$ymax, q$ymax) > pmax(bb$ymin, q$ymin))

  if (!isTRUE(subset)) return(has)
  if (!any(has)) return(NULL)
  shape[which(has)]
}
