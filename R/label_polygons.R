#' Outline labelled objects in a label-mask TIFF as polygons
#'
#' Reads a crop of a label image (each object's pixels hold its integer id, 0 is
#' background) at full resolution and traces each object's outline with
#' `EBImage::ocontour()`. Outlines therefore follow exactly the pixels the label
#' image assigns, and objects cut by the rect edge are traced only up to the edge.
#'
#' @param label_tiff Path to a label (OME-)TIFF.
#' @param rect A one-row `TiffRect` in full-resolution pixel coordinates.
#' @param channel Channel of `label_tiff` holding the object ids.
#' @param mask_channel Optional channel of a binary sub-compartment mask (e.g. a
#'   nuclear mask where all nuclei = 1). Label pixels where this mask is 0 are
#'   dropped, so each sub-compartment inherits the id of its parent object.
#' @return A `TiffPolygon` with one polygon per label, named by label value, or
#'   `NULL` (with a message) when the rect contains no labels. Objects whose
#'   outline has fewer than 3 vertices (1-2 px slivers) are dropped with a message.
#' @details Requires the EBImage package (Bioconductor).
#' @export
label_polygons <- function(label_tiff, rect, channel = 1, mask_channel = NULL) {
  if (!requireNamespace("EBImage", quietly = TRUE))
    stop("label_polygons() requires the EBImage package", call. = FALSE)
  m <- .read_label_crop(label_tiff, rect, channel)
  if (!is.null(mask_channel)) {
    mask <- .read_label_crop(label_tiff, rect, mask_channel)
    m[mask == 0] <- 0
  }
  .label_matrix_polygons(m, attr(m, "x0"), attr(m, "y0"))
}

#' Raw full-resolution crop of one channel as a numeric matrix [x, y]
#'
#' Attributes `x0`/`y0` give the 0-based pixel offset of `m[1, 1]`.
#' @noRd
.read_label_crop <- function(path, rect, channel) {
  if (!is(rect, "TiffRect") || nrow(rect@coords) != 1L)
    stop("rect must be a single-row TiffRect", call. = FALSE)
  r <- rect@coords[1, ]
  # RBioFormats subsets are 1-based
  xs <- seq(floor(r$xmin) + 1, ceiling(r$xmax))
  ys <- seq(floor(r$ymin) + 1, ceiling(r$ymax))
  img <- RBioFormats::read.image(path, resolution = 1, normalize = FALSE,
                                 subset = list(X = xs, Y = ys, C = channel))
  m <- as.array(img)
  dim(m) <- dim(m)[1:2]
  attr(m, "x0") <- min(xs) - 1
  attr(m, "y0") <- min(ys) - 1
  m
}

#' Trace each nonzero label of a matrix into a TiffPolygon
#'
#' @param m numeric matrix [x, y] of label values, 0 = background
#' @param x0,y0 offsets added to the 0-based `ocontour()` coordinates
#' @noRd
.label_matrix_polygons <- function(m, x0 = 0, y0 = 0) {
  vals <- sort(unique(as.vector(m)))
  vals <- vals[vals > 0]
  if (!length(vals)) { message("no labels in rect"); return(NULL) }
  # relabel to 1..n: ocontour indexes by label value, and raw ids can be ~1e8
  idx <- matrix(match(m, vals, nomatch = 0L), nrow = nrow(m))
  cont <- EBImage::ocontour(idx)
  lab <- vals[as.integer(names(cont))]

  keep <- vapply(cont, nrow, integer(1)) >= 3L
  if (any(!keep)) message(sum(!keep), " label(s) with < 3 outline vertices dropped")
  if (!any(keep)) return(NULL)
  cont <- cont[keep]
  # format(): as.character(1e8) would give "1e+08"
  nm <- format(lab[keep], scientific = FALSE, trim = TRUE)

  n_v <- vapply(cont, nrow, integer(1))
  xy <- do.call(rbind, cont)
  TiffPolygon(x = xy[, 1] + x0, y = xy[, 2] + y0, name = rep(nm, n_v))
}
