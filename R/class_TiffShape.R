#' Base class for TIFF geometry shapes
#'
#' `TiffShape` is a virtual S4 class that stores shared structure for shape
#' objects used in TIFF coordinate space.
#'
#' @slot coords data.frame containing shape coordinates and a `name` column
#' @export
setClass(
  "TiffShape",
  contains = "VIRTUAL",
  slots = list(
    coords = "data.frame",
    meta   = "data.frame"
  ),
  prototype = methods::prototype(meta = data.frame()),
  validity = function(object) {
    if (!is.data.frame(object@coords)) return("coords must be a data.frame")
    if (nrow(object@coords) < 1) return("coords must have at least one row")
    if (!"name" %in% names(object@coords)) return("coords must include a name column")
    if (!is.character(object@coords$name)) return("name must be character")
    if (length(object@coords$name) != nrow(object@coords)) return("name must have one value per row")
    if (!is.data.frame(object@meta)) return("meta must be a data.frame")
    if (nrow(object@meta) > 0L) {
      if (!"name" %in% names(object@meta)) return("meta must include a name column when non-empty")
      if (!all(object@meta$name %in% object@coords$name)) return("meta names must be a subset of coords names")
    }
    TRUE
  }
)


.unique_shape_names <- function(x) {
  x <- as.character(x)
  out <- x
  dup_keys <- unique(x[duplicated(x) | duplicated(x, fromLast = TRUE)])
  for (key in dup_keys) {
    idx <- which(x == key)
    out[idx] <- paste0(key, "_", seq_along(idx))
  }
  out
}


#' Get or set optional per-shape metadata
#'
#' Metadata is a data.frame with a `name` column matching shape names, plus any
#' additional columns (e.g. `color`, `fill`). When a `color` or `fill` column
#' is present, `shape_annotate` uses those values unless an explicit override is
#' passed via its `color`/`fill` parameters.
#'
#' @param shape A `TiffShape` object.
#' @param value A data.frame with a `name` column. Names must match those in the
#'   shape's `coords`. Set to `data.frame()` to clear metadata.
#' @return `shape_meta` returns the current metadata data.frame.
#'   `shape_meta<-` returns the updated shape object.
#' @export
#' @examples
#' r <- TiffRect(xmin = c(0, 5), xmax = c(4, 9), ymin = c(0, 5), ymax = c(4, 9),
#'               name = c("a", "b"))
#' shape_meta(r) <- data.frame(name = c("a", "b"), color = c("red", "blue"),
#'                              stringsAsFactors = FALSE)
#' shape_meta(r)
shape_meta <- function(shape) {
  if (!is(shape, "TiffShape")) stop("shape must be a TiffShape", call. = FALSE)
  shape@meta
}

#' @rdname shape_meta
#' @export
`shape_meta<-` <- function(shape, value) {
  if (!is(shape, "TiffShape")) stop("shape must be a TiffShape", call. = FALSE)
  if (!is.data.frame(value)) stop("value must be a data.frame", call. = FALSE)
  shape@meta <- value
  validObject(shape)
  shape
}
