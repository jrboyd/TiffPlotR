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
    coords = "data.frame"
  ),
  validity = function(object) {
    if (!is.data.frame(object@coords)) return("coords must be a data.frame")
    if (nrow(object@coords) < 1) return("coords must have at least one row")
    if (!"name" %in% names(object@coords)) return("coords must include a name column")
    if (!is.character(object@coords$name)) return("name must be character")
    if (length(object@coords$name) != nrow(object@coords)) return("name must have one value per row")
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
