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


#' Polygon definition S4 class
#'
#' Stores polygons as list-columns of x/y vertices.
#'
#' @slot coords data.frame with x/y/name columns where x and y are list-columns
#' @export
setClass(
  "TiffPolygon",
  contains = "TiffShape",
  validity = function(object) {
    required_cols <- c("x", "y", "name")
    if (!all(required_cols %in% names(object@coords))) return("coords must include x, y, and name columns")
    if (!is.list(object@coords$x) || !is.list(object@coords$y)) {
      return("x and y must be list-columns of numeric vertex vectors")
    }

    for (i in seq_len(nrow(object@coords))) {
      xi <- object@coords$x[[i]]
      yi <- object@coords$y[[i]]
      if (!is.numeric(xi) || !is.numeric(yi)) return("Each polygon x and y entry must be numeric")
      if (length(xi) != length(yi)) return("Each polygon must have equal x and y vertex counts")
      if (length(xi) < 3) return("Each polygon must have at least 3 vertices")
    }
    TRUE
  }
)


#' Construct a TiffPolygon
#'
#' @param x numeric vector of x vertices or list of numeric vectors
#' @param y numeric vector of y vertices or list of numeric vectors
#' @param name optional name(s). Recycled when length 1.
#' @return a `TiffPolygon` object
#' @export
TiffPolygon <- function(x, y, name = "polygon") {
  x_list <- if (is.list(x)) x else list(x)
  y_list <- if (is.list(y)) y else list(y)

  n <- max(length(x_list), length(y_list))
  if (!(length(x_list) %in% c(1L, n)) || !(length(y_list) %in% c(1L, n))) {
    stop("x and y must each have length 1 or match the longest input")
  }

  recycle_list <- function(v) if (length(v) == 1L && n > 1L) rep(v, n) else v
  x_list <- recycle_list(x_list)
  y_list <- recycle_list(y_list)

  name <- as.character(name)
  if (!(length(name) %in% c(1L, n))) stop("name must be length 1 or match polygon count")
  if (length(name) == 1L && n > 1L) name <- rep(name, n)

  coords_df <- data.frame(name = .unique_shape_names(name), stringsAsFactors = FALSE)
  coords_df$x <- x_list
  coords_df$y <- y_list
  new("TiffPolygon", coords = coords_df)
}

#' @rdname TiffEllipse
#' @param object A TiffEllipse object
setMethod("show", "TiffEllipse", function(object) {
  n <- nrow(object@coords)
  cat("TiffEllipse with", n, if (n == 1) "ellipse\n" else "ellipses\n")
  print(object@coords, row.names = FALSE)
})

#' @rdname TiffPolygon
#' @param object A TiffPolygon object
setMethod("show", "TiffPolygon", function(object) {
  n <- nrow(object@coords)
  cat("TiffPolygon with", n, if (n == 1) "polygon\n" else "polygons\n")
  out <- object@coords
  out$n_points <- vapply(out$x, length, integer(1))
  out$x <- NULL
  out$y <- NULL
  print(out, row.names = FALSE)
})

#' @rdname TiffEllipse
#' @param x A TiffEllipse object
setMethod("names", "TiffEllipse", function(x) {
  c("x0", "y0", "radius_x", "radius_y", "name", "width", "height")
})

#' @rdname TiffPolygon
#' @param x A TiffPolygon object
setMethod("names", "TiffPolygon", function(x) {
  c("x", "y", "name", "n_points")
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


#' @rdname TiffPolygon
#' @param name Name of slot to be replaced/retrieved
setMethod("$", "TiffPolygon", function(x, name) {
  switch(
    name,
    x = x@coords$x,
    y = x@coords$y,
    name = x@coords$name,
    n_points = vapply(x@coords$x, length, integer(1))
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


#' @rdname TiffPolygon
#' @param x TiffPolygon object
#' @param i integer, logical, or character index
#' @param j unused
#' @param drop unused
#' @return a `TiffPolygon` containing only the selected rows
#' @export
setMethod("[", signature(x = "TiffPolygon"),
          function(x, i, j, ..., drop = FALSE) {
            new("TiffPolygon", coords = x@coords[i, , drop = FALSE])
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


#' Combine `TiffPolygon` objects
#'
#' @param ... one or more `TiffPolygon` objects
#' @return a single `TiffPolygon` with all rows combined
#' @export
setMethod("c", signature(x = "TiffPolygon"),
          function(x, ...) {
            parts <- c(list(x), list(...))
            if (!all(vapply(parts, is, logical(1), "TiffPolygon"))) {
              stop("all arguments to c() must be TiffPolygon objects")
            }
            combined <- do.call(rbind, lapply(parts, function(r) r@coords))
            rownames(combined) <- NULL
            combined$name <- .unique_shape_names(combined$name)
            new("TiffPolygon", coords = combined)
          })


.shape_bbox <- function(shape) {
  if (is(shape, "TiffEllipse")) {
    return(data.frame(
      xmin = shape@coords$x0 - shape@coords$radius_x,
      xmax = shape@coords$x0 + shape@coords$radius_x,
      ymin = shape@coords$y0 - shape@coords$radius_y,
      ymax = shape@coords$y0 + shape@coords$radius_y,
      stringsAsFactors = FALSE
    ))
  }
  if (is(shape, "TiffPolygon")) {
    return(data.frame(
      xmin = vapply(shape@coords$x, min, numeric(1)),
      xmax = vapply(shape@coords$x, max, numeric(1)),
      ymin = vapply(shape@coords$y, min, numeric(1)),
      ymax = vapply(shape@coords$y, max, numeric(1)),
      stringsAsFactors = FALSE
    ))
  }
  stop("shape must be a TiffShape")
}


.shape_recycle_numeric <- function(x, n, label) {
  x <- as.numeric(x)
  if (!(length(x) %in% c(1L, n))) {
    stop(label, " must be length 1 or match number of shapes")
  }
  if (length(x) == 1L) {
    return(rep(x, n))
  }
  x
}


.shape_anchor_coords <- function(bbox, anchor) {
  anchors <- c("center", "topleft", "topright", "botleft", "botright")
  if (length(anchor) != 1L || !anchor %in% anchors) {
    stop("anchor must be one of: center, topleft, topright, botleft, botright")
  }

  if (anchor == "center") {
    ax <- (bbox$xmin + bbox$xmax) / 2
    ay <- (bbox$ymin + bbox$ymax) / 2
  } else if (anchor == "topleft") {
    ax <- bbox$xmin
    ay <- bbox$ymin
  } else if (anchor == "topright") {
    ax <- bbox$xmax
    ay <- bbox$ymin
  } else if (anchor == "botleft") {
    ax <- bbox$xmin
    ay <- bbox$ymax
  } else {
    ax <- bbox$xmax
    ay <- bbox$ymax
  }

  list(ax = ax, ay = ay)
}


.shape_resized_bbox <- function(bbox, width, height, anchor) {
  anc <- .shape_anchor_coords(bbox, anchor)
  ax <- anc$ax
  ay <- anc$ay

  if (anchor == "center") {
    xmin <- ax - width / 2
    xmax <- ax + width / 2
    ymin <- ay - height / 2
    ymax <- ay + height / 2
  } else if (anchor == "topleft") {
    xmin <- ax
    xmax <- ax + width
    ymin <- ay
    ymax <- ay + height
  } else if (anchor == "topright") {
    xmin <- ax - width
    xmax <- ax
    ymin <- ay
    ymax <- ay + height
  } else if (anchor == "botleft") {
    xmin <- ax
    xmax <- ax + width
    ymin <- ay - height
    ymax <- ay
  } else {
    xmin <- ax - width
    xmax <- ax
    ymin <- ay - height
    ymax <- ay
  }

  data.frame(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, stringsAsFactors = FALSE)
}


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

#' @rdname shape_shift
#' @export
setMethod("shape_shift", signature(shape = "TiffEllipse"),
          function(shape, dx = 0, dy = 0) {
            shape@coords$x0 <- shape@coords$x0 + dx
            shape@coords$y0 <- shape@coords$y0 + dy
            validObject(shape)
            shape
          })

#' @rdname shape_shift
#' @export
setMethod("shape_shift", signature(shape = "TiffPolygon"),
          function(shape, dx = 0, dy = 0) {
            shape@coords$x <- lapply(shape@coords$x, function(v) v + dx)
            shape@coords$y <- lapply(shape@coords$y, function(v) v + dy)
            validObject(shape)
            shape
          })

setMethod("shape_shift", signature(shape = "ANY"),
          function(shape, dx = 0, dy = 0) {
            stop("shape must be a TiffShape")
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

#' @rdname shape_resize_abs
#' @export
setMethod("shape_resize_abs", signature(shape = "ANY"),
          function(shape, width, height, anchor = "center") {
            stop("shape must be a TiffShape")
          })

#' @rdname shape_resize_abs
#' @export
setMethod("shape_resize_abs", signature(shape = "TiffEllipse"),
          function(shape, width, height, anchor = "center") {
            bbox <- .shape_bbox(shape)
            n <- nrow(bbox)
            w <- .shape_recycle_numeric(width, n, "width")
            h <- .shape_recycle_numeric(height, n, "height")
            rb <- .shape_resized_bbox(bbox, w, h, anchor)

            shape@coords$x0 <- (rb$xmin + rb$xmax) / 2
            shape@coords$y0 <- (rb$ymin + rb$ymax) / 2
            shape@coords$radius_x <- (rb$xmax - rb$xmin) / 2
            shape@coords$radius_y <- (rb$ymax - rb$ymin) / 2
            validObject(shape)
            shape
          })

#' @rdname shape_resize_abs
#' @export
setMethod("shape_resize_abs", signature(shape = "TiffPolygon"),
          function(shape, width, height, anchor = "center") {
            bbox <- .shape_bbox(shape)
            n <- nrow(bbox)
            w <- .shape_recycle_numeric(width, n, "width")
            h <- .shape_recycle_numeric(height, n, "height")

            anc <- .shape_anchor_coords(bbox, anchor)
            old_w <- bbox$xmax - bbox$xmin
            old_h <- bbox$ymax - bbox$ymin
            if (any(old_w <= 0) || any(old_h <= 0)) {
              stop("Cannot resize polygon with zero width or height")
            }
            sx <- w / old_w
            sy <- h / old_h

            for (i in seq_len(n)) {
              shape@coords$x[[i]] <- anc$ax[[i]] + (shape@coords$x[[i]] - anc$ax[[i]]) * sx[[i]]
              shape@coords$y[[i]] <- anc$ay[[i]] + (shape@coords$y[[i]] - anc$ay[[i]]) * sy[[i]]
            }
            validObject(shape)
            shape
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

#' @rdname shape_resize_mult
#' @export
setMethod("shape_resize_mult", signature(shape = "TiffShape"),
          function(shape, fx = 1, fy = NULL, anchor = "center") {
            if (is.null(fy)) {
              fy <- fx
            }
            bbox <- .shape_bbox(shape)
            n <- nrow(bbox)
            fx <- .shape_recycle_numeric(fx, n, "fx")
            fy <- .shape_recycle_numeric(fy, n, "fy")

            w <- (bbox$xmax - bbox$xmin) * fx
            h <- (bbox$ymax - bbox$ymin) * fy
            shape_resize_abs(shape, width = w, height = h, anchor = anchor)
          })


.ellipse_path_df <- function(shape, n = 120L) {
  n <- as.integer(n)
  theta <- seq(0, 2 * pi, length.out = n)
  parts <- lapply(seq_len(nrow(shape@coords)), function(i) {
    data.frame(
      x = shape@coords$x0[[i]] + shape@coords$radius_x[[i]] * cos(theta),
      y = shape@coords$y0[[i]] + shape@coords$radius_y[[i]] * sin(theta),
      shape_name = shape@coords$name[[i]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, parts)
}


.polygon_path_df <- function(shape) {
  parts <- lapply(seq_len(nrow(shape@coords)), function(i) {
    data.frame(
      x = shape@coords$x[[i]],
      y = shape@coords$y[[i]],
      shape_name = shape@coords$name[[i]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, parts)
}


.shape_center_points <- function(shape) {
  if (is(shape, "TiffEllipse")) {
    data.frame(
      x = shape@coords$x0,
      y = shape@coords$y0,
      stringsAsFactors = FALSE
    )
  } else if (is(shape, "TiffPolygon")) {
    data.frame(
      x = vapply(shape@coords$x, mean, numeric(1)),
      y = vapply(shape@coords$y, mean, numeric(1)),
      stringsAsFactors = FALSE
    )
  } else {
    stop("shape must be a TiffShape")
  }
}


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

#' @rdname shape_annotate
#' @export
setMethod("shape_annotate", signature(p = "ANY", shape = "ANY"),
          function(p, shape, color = "green", fill = NA, alpha = 0.2, annotate_center = FALSE, n = 120L, ...) {
            if (!inherits(p, "ggplot")) stop("p must be a ggplot or TiffPlotData object")
            if (!is(shape, "TiffShape")) stop("shape must be a TiffShape")
            stop("unexpected error")
          })

#' @rdname shape_annotate
#' @export
setMethod("shape_annotate", signature(p = "ANY", shape = "TiffEllipse"),
          function(p, shape, color = "green", fill = NA, alpha = 0.2, annotate_center = FALSE, n = 120L, ...) {
            if (!inherits(p, "ggplot")) stop("p must be a ggplot or TiffPlotData object")

            if (isTRUE(annotate_center)) {
              center_df <- .shape_center_points(shape)
              return(
                p + ggplot2::annotate("point", x = center_df$x, y = center_df$y,
                                      color = color, alpha = alpha, ...)
              )
            }

            ell_df <- .ellipse_path_df(shape, n = n)
            p + ggplot2::geom_polygon(
              data = ell_df,
              mapping = ggplot2::aes(x = x, y = y, group = shape_name),
              inherit.aes = FALSE,
              color = color,
              fill = fill,
              alpha = alpha,
              ...
            )
          })

#' @rdname shape_annotate
#' @export
setMethod("shape_annotate", signature(p = "ANY", shape = "TiffPolygon"),
          function(p, shape, color = "green", fill = NA, alpha = 0.2, annotate_center = FALSE, n = 120L, ...) {
            if (!inherits(p, "ggplot")) stop("p must be a ggplot or TiffPlotData object")

            if (isTRUE(annotate_center)) {
              center_df <- .shape_center_points(shape)
              return(
                p + ggplot2::annotate("point", x = center_df$x, y = center_df$y,
                                      color = color, alpha = alpha, ...)
              )
            }

            poly_df <- .polygon_path_df(shape)
            p + ggplot2::geom_polygon(
              data = poly_df,
              mapping = ggplot2::aes(x = x, y = y, group = shape_name),
              inherit.aes = FALSE,
              color = color,
              fill = fill,
              alpha = alpha,
              ...
            )
          })