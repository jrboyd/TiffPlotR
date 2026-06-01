#' Ellipse definition S4 class
#'
#' Stores ellipses using center coordinates and x/y radii.
#'
#' @slot coords data.frame with x0/y0/radius_x/radius_y/name columns
#' @export
setClass(
  "TiffEllipse",
  contains = "TiffShape",
  validity = function(object) {
    required_cols <- c("x0", "y0", "radius_x", "radius_y", "name")
    if (!all(required_cols %in% names(object@coords))) {
      return("coords must include x0, y0, radius_x, radius_y, name columns")
    }
    num_cols <- object@coords[, c("x0", "y0", "radius_x", "radius_y"), drop = FALSE]
    if (any(!vapply(num_cols, is.numeric, logical(1)))) return("Ellipse coordinates must be numeric")
    if (any(object@coords$radius_x <= 0) || any(object@coords$radius_y <= 0)) {
      return("Ellipse radii must be > 0")
    }
    TRUE
  }
)


#' Construct a TiffEllipse
#'
#' @param x0 x center coordinate
#' @param y0 y center coordinate
#' @param radius_x ellipse x radius
#' @param radius_y ellipse y radius (defaults to radius_x)
#' @param name optional name(s). Recycled when length 1.
#' @return a `TiffEllipse` object
#' @export
TiffEllipse <- function(x0, y0, radius_x, radius_y = radius_x, name = "ellipse") {
  vals <- list(x0 = x0, y0 = y0, radius_x = radius_x, radius_y = radius_y)
  if (any(vapply(vals, function(x) !is.numeric(x), logical(1)))) stop("Ellipse coordinates must be numeric")

  lengths <- vapply(vals, length, integer(1))
  n <- max(lengths)
  if (any(!(lengths %in% c(1L, n)))) stop("Coordinate lengths must all be 1 or match the longest input")

  name <- as.character(name)
  if (!(length(name) %in% c(1L, n))) stop("name must be length 1 or match coordinate length")

  recycle <- function(x) if (length(x) == 1L) rep(x, n) else x
  coords_df <- data.frame(
    x0 = as.numeric(recycle(x0)),
    y0 = as.numeric(recycle(y0)),
    radius_x = as.numeric(recycle(radius_x)),
    radius_y = as.numeric(recycle(radius_y)),
    name = .unique_shape_names(recycle(name)),
    stringsAsFactors = FALSE
  )
  new("TiffEllipse", coords = coords_df)
}

#' @rdname TiffEllipse
#' @param object A TiffEllipse object
setMethod("show", "TiffEllipse", function(object) {
  n <- nrow(object@coords)
  cat("TiffEllipse with", n, if (n == 1) "ellipse\n" else "ellipses\n")
  print(object@coords, row.names = FALSE)
})

#' @rdname TiffEllipse
#' @param x A TiffEllipse object
setMethod("names", "TiffEllipse", function(x) {
  c("x0", "y0", "radius_x", "radius_y", "name", "width", "height")
})

#' @rdname TiffEllipse
#' @param name Name of slot to be replaced/retrieved
setMethod("$", "TiffEllipse", function(x, name) {
  switch(
    name,
    x0 = x@coords$x0,
    y0 = x@coords$y0,
    radius_x = x@coords$radius_x,
    radius_y = x@coords$radius_y,
    name = x@coords$name,
    width = 2 * x@coords$radius_x,
    height = 2 * x@coords$radius_y
  )
})

#' @rdname TiffEllipse
#' @param x TiffEllipse object
#' @param i integer, logical, or character index
#' @param j unused
#' @param drop unused
#' @return a `TiffEllipse` containing only the selected rows
#' @export
setMethod("[", signature(x = "TiffEllipse"),
          function(x, i, j, ..., drop = FALSE) {
            new("TiffEllipse", coords = x@coords[i, , drop = FALSE])
          })

#' Combine `TiffEllipse` objects
#'
#' @param ... one or more `TiffEllipse` objects
#' @return a single `TiffEllipse` with all rows combined
#' @export
setMethod("c", signature(x = "TiffEllipse"),
          function(x, ...) {
            parts <- c(list(x), list(...))
            if (!all(vapply(parts, is, logical(1), "TiffEllipse"))) {
              stop("all arguments to c() must be TiffEllipse objects")
            }
            combined <- do.call(rbind, lapply(parts, function(r) r@coords))
            rownames(combined) <- NULL
            combined$name <- .unique_shape_names(combined$name)
            new("TiffEllipse", coords = combined)
          })
