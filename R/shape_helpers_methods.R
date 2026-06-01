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


#' @rdname shape_resize_mult
#' @export
setMethod("shape_resize_mult", signature(shape = "ANY"),
          function(shape, fx = 1, fy = NULL, anchor = "center") {
              stop("shape must be a TiffShape")
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


#' @rdname shape_center_points
#' @export
setMethod("shape_center_points", signature(shape = "TiffEllipse"),
          function(shape) {
            .shape_center_points(shape)
          })

#' @rdname shape_center_points
#' @export
setMethod("shape_center_points", signature(shape = "TiffPolygon"),
          function(shape) {
            .shape_center_points(shape)
          })

#' @rdname shape_center_points
#' @export
setMethod("shape_center_points", signature(shape = "ANY"),
          function(shape) {
            stop("shape must be a TiffShape")
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
              center_df <- shape_center_points(shape)
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
              center_df <- shape_center_points(shape)
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


#' Add a `TiffEllipse` to a ggplot using `+`
#'
#' Enables syntax like `ggplot(...) + my_ellipse`.
#'
#' @param object A `TiffEllipse` object.
#' @param plot A ggplot object.
#' @param object_name Unused; required by `ggplot_add`.
#' @return ggplot object with ellipse annotation added.
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.TiffEllipse <- function(object, plot, object_name) {
  shape_annotate(plot, object)
}


#' Add a `TiffPolygon` to a ggplot using `+`
#'
#' Enables syntax like `ggplot(...) + my_polygon`.
#'
#' @param object A `TiffPolygon` object.
#' @param plot A ggplot object.
#' @param object_name Unused; required by `ggplot_add`.
#' @return ggplot object with polygon annotation added.
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.TiffPolygon <- function(object, plot, object_name) {
  shape_annotate(plot, object)
}


#' Create a style-aware shape layer for ggplot2 `+`
#'
#' Wraps a `TiffShape` object together with annotation arguments so you can
#' use styled `+` chaining in ggplot2.
#'
#' @param shape A `TiffShape` object (`TiffRect`, `TiffEllipse`, or `TiffPolygon`).
#' @param color Border/line color.
#' @param fill Fill color.
#' @param alpha Alpha transparency.
#' @param annotate_center If `TRUE`, annotate center points instead of boundaries.
#' @param n Number of vertices used to draw ellipse boundaries.
#' @param ... Additional arguments passed to ggplot layer calls.
#' @return A `TiffShapeLayer` object usable with `ggplot +`.
#' @export
#' @examples
#' library(ggplot2)
#' r <- TiffRect(10, 30, 10, 25)
#' ggplot() +
#'   coord_fixed(xlim = c(0, 40), ylim = c(0, 40)) +
#'   shape_layer(r, color = "red", fill = "tomato", alpha = 0.3)
shape_layer <- function(shape,
                        color = "green",
                        fill = NA,
                        alpha = 0.2,
                        annotate_center = FALSE,
                        n = 120L,
                        ...) {
  if (!is(shape, "TiffShape")) {
    stop("shape must be a TiffShape")
  }

  structure(
    list(
      shape = shape,
      params = c(
        list(
          color = color,
          fill = fill,
          alpha = alpha,
          annotate_center = annotate_center,
          n = n
        ),
        list(...)
      )
    ),
    class = "TiffShapeLayer"
  )
}


#' Add a `TiffShapeLayer` to a ggplot using `+`
#'
#' @param object A `TiffShapeLayer` object created by `shape_layer()`.
#' @param plot A ggplot object.
#' @param object_name Unused; required by `ggplot_add`.
#' @return ggplot object with shape annotation added.
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.TiffShapeLayer <- function(object, plot, object_name) {
  do.call(
    shape_annotate,
    c(list(p = plot, shape = object$shape), object$params)
  )
}
