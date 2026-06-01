#' Rectangle definition S4 class
#'
#' Stores a \code{\link{TiffRect}} region definition with numeric boundaries.
#'
#' @slot coords data.frame with xmin/xmax/ymin/ymax/name columns
#' @seealso
#' Construction and creation: \code{\link{TiffRect}} to build new rectangle objects.
#'
#' Geometric transforms: \code{\link{rect_shift}} to translate rectangles,
#' \code{\link{rect_resize_abs}} for fixed-size resizing, and
#' \code{\link{rect_resize_mult}} for multiplier-based resizing.
#'
#' Spatial relationships: \code{\link{rect_intersection_region}} to compute overlap
#' regions, \code{\link{rect_test_overlap}} to flag overlapping pairs, and
#' \code{\link{rect_test_contains}} to test containment.
#'
#' Visualization: \code{\link{rect_annotate}} to draw rectangles on ggplot objects.
#'
#'
#' @export
setClass("TiffRect",
         contains = "TiffShape",
         validity = function(object){
           required_cols <- c("xmin", "xmax", "ymin", "ymax", "name")
           if(!is.data.frame(object@coords)) return("coords must be a data.frame")
           if(nrow(object@coords) < 1) return("coords must have at least one row")
           if(!all(required_cols %in% names(object@coords))) return("coords must include xmin, xmax, ymin, ymax, name columns")

           num_cols <- object@coords[, c("xmin", "xmax", "ymin", "ymax"), drop = FALSE]
           if(any(!vapply(num_cols, is.numeric, logical(1)))) return("Coordinates must be numeric")
           if(any(num_cols$xmin >= num_cols$xmax)) return("xmin must be < xmax")
           if(any(num_cols$ymin >= num_cols$ymax)) return("ymin must be < ymax")

           if(!is.character(object@coords$name)) return("name must be character")
           if(length(object@coords$name) != nrow(object@coords)) return("name must have one value per row")
           TRUE
         }
)

utils::globalVariables(c("xmin", "xmax", "ymin", "ymax", "name", "x", "y"))

#' Construct a TiffRect
#'
#' @param xmin left x coordinate
#' @param xmax right x coordinate
#' @param ymin bottom y coordinate
#' @param ymax top y coordinate
#' @param name optional name(s). Recycled when length 1
#' @return a `TiffRect` object
#' @seealso
#' Class details: \code{\link{TiffRect-class}} for slot structure and validity rules.
#'
#' Geometric transforms: \code{\link{rect_shift}}, \code{\link{rect_resize_abs}},
#' and \code{\link{rect_resize_mult}}.
#'
#' Spatial relationships: \code{\link{rect_intersection_region}},
#' \code{\link{rect_test_overlap}}, and \code{\link{rect_test_contains}}.
#'
#' Visualization: \code{\link{rect_annotate}}.
#' @export
TiffRect <- function(xmin, xmax, ymin, ymax, name = "rect"){
  coords <- list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  if(any(vapply(coords, function(x) !is.numeric(x), logical(1)))) stop("Coordinates must be numeric")

  lengths <- vapply(coords, length, integer(1))
  n <- max(lengths)
  if(any(!(lengths %in% c(1L, n)))) stop("Coordinate lengths must all be 1 or match the longest input")

  name <- as.character(name)
  if(!(length(name) %in% c(1L, n))) stop("name must be length 1 or match coordinate length")

  recycle <- function(x) if(length(x) == 1L) rep(x, n) else x

  coords_df <- data.frame(
    xmin = as.numeric(recycle(xmin)),
    xmax = as.numeric(recycle(xmax)),
    ymin = as.numeric(recycle(ymin)),
    ymax = as.numeric(recycle(ymax)),
    name = .unique_shape_names(recycle(name)),
    stringsAsFactors = FALSE
  )
  new("TiffRect", coords = coords_df)
}

#' Show method for \code{\link{TiffRect}}
#' @param object TiffRect
setMethod("show", "TiffRect", function(object){
  n <- nrow(object@coords)
  cat("TiffRect with", n, if(n == 1) "rectangle\n" else "rectangles\n")
  print(object@coords, row.names = FALSE)
})


#### Names ####

#' @rdname TiffRect
#' @param x A TiffRect object
setMethod("names", "TiffRect",
          function(x)
          {
              c(
                  "xmin",
                  "xmax",
                  "ymin",
                  "ymax",
                  "name",
                  "width",
                  "height"
              )

          })

#' @rdname TiffRect
#' @param name Name of slot to be replaced/retrieved
setMethod("$", "TiffRect",
          function(x, name)
          {
              switch (name,
                      xmin = x@coords$xmin,
                      xmax = x@coords$xmax,
                      ymin = x@coords$ymin,
                      ymax = x@coords$ymax,
                      name = x@coords$name,
                      width = x@coords$xmax - x@coords$xmin,
                      height = x@coords$ymax - x@coords$ymin
              )
          })

#' #' @rdname TiffRect
#' #' @param value New value for slot.
#' setReplaceMethod("$", "TiffRect",
#'                  function(x, name, value)
#'                  {
#'                      warn_msg = "This assignment is not supported.  No effect."
#'                      switch (name,
#'                              view_size = {
#'                                  if(is.null(value)){
#'                                      value = -1
#'                                  }
#'                                  if(is.na(value)){
#'                                      value = -1
#'                                  }
#'                                  x@view_size = value
#'                              },
#'                              window_size = {
#'                                  x@window_size = value
#'                              },
#'                              read_mode = {
#'                                  stopifnot(value %in% c("bam_SE", "bam_PE", "bigwig", "null"))
#'                                  x@read_mode = value
#'                              },
#'                              fetch_options = {
#'                                  x@fetch_options = value
#'                              },
#'                              meta_data = {
#'                                  x@meta_data = value
#'                              },
#'                              warning(warn_msg)
#'                      )
#'                      validObject(x)
#'                      x
#'                  })



#### Shift and resize ####

#' @rdname shape_center_points
#' @export
setMethod("shape_center_points", signature(shape = "TiffRect"),
          function(shape, anchor = "center") {
            anchors <- c("center", "topleft", "topright", "botleft", "botright")
            if (length(anchor) != 1L || !anchor %in% anchors) {
              stop("anchor must be one of: center, topleft, topright, botleft, botright")
            }

            coords <- shape@coords
            if (anchor == "center") {
              x <- (coords$xmin + coords$xmax) / 2
              y <- (coords$ymin + coords$ymax) / 2
            } else if (anchor == "topleft") {
              x <- coords$xmin
              y <- coords$ymin
            } else if (anchor == "topright") {
              x <- coords$xmax
              y <- coords$ymin
            } else if (anchor == "botleft") {
              x <- coords$xmin
              y <- coords$ymax
            } else {
              x <- coords$xmax
              y <- coords$ymax
            }

            data.frame(
              x = x,
              y = y,
              name = coords$name,
              anchor = rep(anchor, nrow(coords)),
              stringsAsFactors = FALSE
            )
          })

#' @rdname shape_shift
#' @export
setMethod("shape_shift", signature(shape = "TiffRect"),
          function(shape, dx = 0, dy = 0) {
            shape@coords$xmin <- shape@coords$xmin + dx
            shape@coords$xmax <- shape@coords$xmax + dx
            shape@coords$ymin <- shape@coords$ymin + dy
            shape@coords$ymax <- shape@coords$ymax + dy
            validObject(shape)
            shape
          })

#' @rdname shape_resize_abs
#' @export
setMethod("shape_resize_abs", signature(shape = "TiffRect"),
          function(shape, width, height, anchor = "center") {
            if(!anchor %in% c("center", "topleft", "topright", "botleft", "botright")){
              stop("anchor must be one of: center, topleft, topright, botleft, botright")
            }
            cx <- (shape@coords$xmin + shape@coords$xmax) / 2
            cy <- (shape@coords$ymin + shape@coords$ymax) / 2
            w <- as.numeric(width)
            h <- as.numeric(height)
            if(length(w) == 1L) w <- rep(w, nrow(shape@coords))
            if(length(h) == 1L) h <- rep(h, nrow(shape@coords))
            if(length(w) != nrow(shape@coords) || length(h) != nrow(shape@coords)){
              stop("width and height must be length 1 or match row count")
            }

            if(anchor == "center"){
              shape@coords$xmin <- cx - w/2
              shape@coords$xmax <- cx + w/2
              shape@coords$ymin <- cy - h/2
              shape@coords$ymax <- cy + h/2
            } else if(anchor == "topleft"){
              shape@coords$xmax <- shape@coords$xmin + w
              shape@coords$ymax <- shape@coords$ymin + h
            } else if(anchor == "topright"){
              shape@coords$xmin <- shape@coords$xmax - w
              shape@coords$ymax <- shape@coords$ymin + h
            } else if(anchor == "botleft"){
              shape@coords$xmax <- shape@coords$xmin + w
              shape@coords$ymin <- shape@coords$ymax - h
            } else if(anchor == "botright"){
              shape@coords$xmin <- shape@coords$xmax - w
              shape@coords$ymin <- shape@coords$ymax - h
            }
            validObject(shape)
            shape
          })

#' @rdname shape_resize_mult
#' @export
setMethod("shape_resize_mult", signature(shape = "TiffRect"),
          function(shape, fx = 1, fy = NULL, anchor = "center") {
            if(is.null(fy)) fy <- fx
            w <- (shape@coords$xmax - shape@coords$xmin) * as.numeric(fx)
            h <- (shape@coords$ymax - shape@coords$ymin) * as.numeric(fy)
            shape_resize_abs(shape, width = w, height = h, anchor = anchor)
          })

#' Shift and resize \code{\link{TiffRect}} objects
#'
#' Functions to transform a \code{\link{TiffRect}} object's position or dimensions.
#'
#' @param rect TiffRect object
#' @param dx shift in x direction (numeric)
#' @param dy shift in y direction (numeric)
#' @param width desired absolute width (numeric)
#' @param height desired absolute height (numeric)
#' @param fx multiplier for width (numeric)
#' @param fy multiplier for height (numeric, defaults to `fx`)
#' @param anchor point to keep fixed during resize: `"center"` (default),
#'   `"topleft"`, `"topright"`, `"botleft"`, `"botright"`
#' @return a `TiffRect` with updated coordinates
#' @export
#' @examples
#' library(ggplot2)
#' library(magrittr)
#' r <- TiffRect(10, 20, 10, 20, name = "original")
#'
#' # rect_shift: show original vs shifted
#' r_shifted <- rect_shift(r, dx = 8, dy = 5)
#' ggplot() %>%
#'   rect_annotate(r, fill = "steelblue", alpha = 0.3) %>%
#'   rect_annotate(r_shifted, fill = "tomato", alpha = 0.3)
#'
#' # rect_resize_abs: show original vs absolute-resized from different anchors
#' r_abs_center  <- rect_resize_abs(r, width = 20, height = 20, anchor = "center")
#' r_abs_topleft <- rect_resize_abs(r, width = 20, height = 20, anchor = "topleft")
#' ggplot() %>%
#'   rect_annotate(r, fill = "steelblue", alpha = 0.3) %>%
#'   rect_annotate(r_abs_center,  fill = "tomato",    alpha = 0.3) %>%
#'   rect_annotate(r_abs_topleft, fill = "goldenrod", alpha = 0.3)
#'
#' # rect_resize_mult: show original vs scaled up and down
#' r_big   <- rect_resize_mult(r, fx = 2)
#' r_small <- rect_resize_mult(r, fx = 0.5)
#' ggplot() %>%
#'   rect_annotate(r,       fill = "steelblue", alpha = 0.3) %>%
#'   rect_annotate(r_big,   fill = "tomato",    alpha = 0.3) %>%
#'   rect_annotate(r_small, fill = "seagreen",  alpha = 0.3)
rect_shift <- function(rect, dx = 0, dy = 0){
  if (interactive()) .Deprecated("shape_shift")
  shape_shift(rect, dx = dx, dy = dy)
}

#' @describeIn rect_shift Resize to absolute width and height anchored at a fixed point
#' @export
#' @examples
#' # Demonstrate how each anchor keeps a different corner fixed
#' library(ggplot2)
#' library(magrittr)
#' r <- TiffRect(10, 20, 10, 20, name = "original")
#' r_tl <- rect_resize_abs(r, 15, 15, anchor = "topleft")
#' r_br <- rect_resize_abs(r, 15, 15, anchor = "botright")
#' ggplot() %>%
#'   rect_annotate(r_tl, fill = "tomato",    alpha = 0.3) %>%
#'   rect_annotate(r_br, fill = "seagreen",  alpha = 0.3) %>%
#'   rect_annotate(r,    fill = "steelblue", alpha = 0.3, color = "red")
rect_resize_abs <- function(rect, width, height, anchor = "center"){
  if (interactive()) .Deprecated("shape_resize_abs")
  shape_resize_abs(rect, width = width, height = height, anchor = anchor)
}

#' @describeIn rect_shift Resize by multiplying current width and height
#' @export
#' @examples
#' # Stretch width only vs uniform scaling
#' library(ggplot2)
#' library(magrittr)
#' r <- TiffRect(10, 20, 10, 20, name = "original")
#' r_wide   <- rect_resize_mult(r, fx = 2,   fy = 1)
#' r_tall   <- rect_resize_mult(r, fx = 1,   fy = 2)
#' r_scaled <- rect_resize_mult(r, fx = 1.5)
#' ggplot() %>%
#'   rect_annotate(r_wide,   fill = "tomato",    alpha = 0.3) %>%
#'   rect_annotate(r_tall,   fill = "goldenrod", alpha = 0.3) %>%
#'   rect_annotate(r_scaled, fill = "seagreen",  alpha = 0.3) %>%
#'   rect_annotate(r,        fill = "steelblue", alpha = 0.3, color = "red")
rect_resize_mult <- function(rect, fx = 1, fy = NULL, anchor = "center"){
  if (interactive()) .Deprecated("shape_resize_mult")
  shape_resize_mult(rect, fx = fx, fy = fy, anchor = anchor)
}

#' Return anchor points for \code{\link{TiffRect}} rows
#'
#' Computes one point per rectangle and returns the result as a data.frame.
#' By default this returns rectangle centers, but corner anchors are also supported.
#'
#' @param rect TiffRect
#' @param anchor one of `"center"`, `"topleft"`, `"topright"`, `"botleft"`, `"botright"`
#' @return data.frame with columns `x`, `y`, `name`, and `anchor`
#' @export
#' @examples
#' r <- TiffRect(xmin = c(0, 10), xmax = c(4, 14), ymin = c(0, 10), ymax = c(6, 16), name = c("a", "b"))
#' rect_centers(r)
#' rect_centers(r, anchor = "topleft")
rect_centers <- function(rect, anchor = "center"){
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect")
  anchors <- c("center", "topleft", "topright", "botleft", "botright")
  if(length(anchor) != 1L || !anchor %in% anchors){
    stop("anchor must be one of: center, topleft, topright, botleft, botright")
  }

  coords <- rect@coords
  if(anchor == "center"){
    x <- (coords$xmin + coords$xmax) / 2
    y <- (coords$ymin + coords$ymax) / 2
  } else if(anchor == "topleft"){
    x <- coords$xmin
    y <- coords$ymin
  } else if(anchor == "topright"){
    x <- coords$xmax
    y <- coords$ymin
  } else if(anchor == "botleft"){
    x <- coords$xmin
    y <- coords$ymax
  } else {
    x <- coords$xmax
    y <- coords$ymax
  }

  data.frame(x = x,
             y = y,
             name = coords$name,
             anchor = rep(anchor, nrow(coords)),
             stringsAsFactors = FALSE)
}

#' Spatial relationship functions for \code{\link{TiffRect}} objects
#'
#' Functions to query spatial relationships between \code{\link{TiffRect}}
#' objects: compute the
#' intersection region, test for pairwise overlap, or test for containment.
#'
#' @param rect TiffRect
#' @param other TiffRect to compare against
#' @param name optional name(s) for returned intersection rectangle(s)
#' @param subset logical; when `FALSE` (default) returns a logical vector.
#'   When `TRUE`, returns `rect` subsetted to matching rows, or `NULL` if none match.
#' @return `rect_intersection_region`: a `TiffRect` of the overlapping region(s), or
#'   `NULL` if there is no overlap; `rect_test_overlap` and `rect_test_contains`: a
#'   logical vector, or a `TiffRect` when `subset = TRUE`
#' @export
#' @examples
#' library(ggplot2)
#' library(magrittr)
#' r1 <- TiffRect(0, 10, 0, 10, name = "a")
#' r2 <- TiffRect(5, 15, 5, 15, name = "b")
#' isect <- rect_intersection_region(r1, r2)
#' ggplot() %>%
#'   rect_annotate(r1,    fill = "steelblue", alpha = 0.3) %>%
#'   rect_annotate(r2,    fill = "tomato",    alpha = 0.3) %>%
#'   rect_annotate(isect, fill = "seagreen",  alpha = 0.5, color = "red")
rect_intersection_region <- function(rect, other, name = NULL){
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect")
  if(!is(other, "TiffRect")) stop("other must be a TiffRect")

  n1 <- nrow(rect@coords)
  n2 <- nrow(other@coords)
  if(n1 != n2 && n1 != 1 && n2 != 1){
    stop("rect and other must have same number of rows or one must have one row")
  }

  n <- max(n1, n2)
  idx1 <- rep(seq_len(n1), length.out = n)
  idx2 <- rep(seq_len(n2), length.out = n)

  rect_coords <- rect@coords[idx1, , drop = FALSE]
  other_coords <- other@coords[idx2, , drop = FALSE]

  xmin <- pmax(rect_coords$xmin, other_coords$xmin)
  xmax <- pmin(rect_coords$xmax, other_coords$xmax)
  ymin <- pmax(rect_coords$ymin, other_coords$ymin)
  ymax <- pmin(rect_coords$ymax, other_coords$ymax)

  has_overlap <- (xmin < xmax) & (ymin < ymax)
  if(!any(has_overlap)) return(NULL)

  default_names <- paste0(rect_coords$name, "_intersect_", other_coords$name)
  if(is.null(name)){
    out_names <- default_names[has_overlap]
  } else {
    name <- as.character(name)
    if(length(name) == 1L){
      out_names <- rep(name, sum(has_overlap))
    } else if(length(name) == n){
      out_names <- name[has_overlap]
    } else if(length(name) == sum(has_overlap)){
      out_names <- name
    } else {
      stop("name must be length 1, number of pairs, or number of overlaps")
    }
  }

  TiffRect(xmin = xmin[has_overlap], xmax = xmax[has_overlap], ymin = ymin[has_overlap], ymax = ymax[has_overlap], name = out_names)
}

#' @describeIn rect_intersection_region Test whether each paired row of `rect` overlaps with `other`
#' @export
#' @examples
#' # Three candidate rects, one query — highlight those that overlap
#' library(ggplot2)
#' library(magrittr)
#' candidates <- TiffRect(
#'   xmin = c(0, 12, 6), xmax = c(5, 17, 11),
#'   ymin = c(0, 0,  0), ymax = c(5, 5,  5),
#'   name = c("left", "right", "center")
#' )
#' query <- TiffRect(4, 9, 0, 5, name = "query")
#' hits <- rect_test_overlap(candidates, query, subset = TRUE)
#' p <- ggplot() %>%
#'   rect_annotate(candidates, fill = "steelblue", alpha = 0.3) %>%
#'   rect_annotate(query,      fill = "tomato",    alpha = 0.2)
#' if (!is.null(hits)) p <- rect_annotate(p, hits, fill = NA, alpha = 0.5, color = "red")
#' p + labs(title = "hits are outlined in red")
rect_test_overlap <- function(rect, other, subset = FALSE){
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect")
  if(!is(other, "TiffRect")) stop("other must be a TiffRect")

  n1 <- nrow(rect@coords)
  n2 <- nrow(other@coords)
  if(n1 != n2 && n1 != 1 && n2 != 1){
    stop("rect and other must have same number of rows or one must have one row")
  }

  n <- max(n1, n2)
  idx1 <- rep(seq_len(n1), length.out = n)
  idx2 <- rep(seq_len(n2), length.out = n)

  rect_coords <- rect@coords[idx1, , drop = FALSE]
  other_coords <- other@coords[idx2, , drop = FALSE]

  has_overlap <- (pmin(rect_coords$xmax, other_coords$xmax) > pmax(rect_coords$xmin, other_coords$xmin)) &
                 (pmin(rect_coords$ymax, other_coords$ymax) > pmax(rect_coords$ymin, other_coords$ymin))

  if(!isTRUE(subset)) return(has_overlap)

  rect_row_has_overlap <- as.logical(tapply(has_overlap, idx1, any))
  if(!any(rect_row_has_overlap)) return(NULL)
  new("TiffRect", coords = rect@coords[rect_row_has_overlap, , drop = FALSE])
}

#' @describeIn rect_intersection_region Test whether each row of `rect` is fully within `other`
#' @export
#' @examples
#' # Two candidates, one fully inside the container, one partially outside
#' library(ggplot2)
#' library(magrittr)
#' candidates <- TiffRect(
#'   xmin = c(2, 8, 0), xmax = c(8, 14, 10),
#'   ymin = c(2, 2, 0), ymax = c(8, 8, 10),
#'   name = c("inside", "outside", "identical")
#' )
#' container <- TiffRect(0, 10, 0, 10, name = "container")
#' contained <- rect_test_contains(candidates, container, subset = TRUE)
#' p <- ggplot() %>%
#'   rect_annotate(container,  fill = NA,          color = "black",     alpha = 1) %>%
#'   rect_annotate(candidates, fill = "steelblue", alpha = 0.3)
#' if (!is.null(contained)) p <- rect_annotate(p, contained, fill = "seagreen", alpha = 0.5, color = "red")
#' p
rect_test_contains <- function(rect, other, subset = FALSE){
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect")
  if(!is(other, "TiffRect")) stop("other must be a TiffRect")

  n1 <- nrow(rect@coords)
  n2 <- nrow(other@coords)
  if(n1 != n2 && n1 != 1 && n2 != 1){
    stop("rect and other must have same number of rows or one must have one row")
  }

  n <- max(n1, n2)
  idx1 <- rep(seq_len(n1), length.out = n)
  idx2 <- rep(seq_len(n2), length.out = n)

  rect_coords <- rect@coords[idx1, , drop = FALSE]
  other_coords <- other@coords[idx2, , drop = FALSE]

  is_contained <- (rect_coords$xmin >= other_coords$xmin) &
                  (rect_coords$xmax <= other_coords$xmax) &
                  (rect_coords$ymin >= other_coords$ymin) &
                  (rect_coords$ymax <= other_coords$ymax)

  if(!isTRUE(subset)) return(is_contained)

  rect_row_contained <- as.logical(tapply(is_contained, idx1, all))
  if(!any(rect_row_contained)) return(NULL)
  new("TiffRect", coords = rect@coords[rect_row_contained, , drop = FALSE])
}

#' Annotate a ggplot with a \code{\link{TiffRect}}
#'
#' Adds one or more \code{\link{TiffRect}} regions as \code{geom_rect} layers.
#'
#' @param rect TiffRect
#' @param p ggplot object
#' @param color border color (default "red")
#' @param fill fill color (default NA)
#' @param alpha fill alpha (default 0.2)
#' @param annotate_center logical; when `TRUE`, plot rectangle center points instead of rectangle outlines
#' @param ... additional args passed to geom_rect
#' @return ggplot object with rectangle layer added
#' @export
#' @importFrom ggplot2 geom_rect aes ggplot
methods::setGeneric("rect_annotate", function(p, rect, color = "green", fill = NA, alpha = 0.2, annotate_center = FALSE, ...){
  standardGeneric("rect_annotate")
})

.annotate_ggplot = function(p, rect, color = "green", fill = NA, alpha = 0.2, annotate_center = FALSE, ...){
    if(length(annotate_center) != 1L || !is.logical(annotate_center)){
      stop("annotate_center must be TRUE or FALSE")
    }

    if(isTRUE(annotate_center)){
      point_df <- rect_centers(rect, anchor = "center")
      p + ggplot2::annotate("point",
                            x = point_df$x,
                            y = point_df$y,
                            color = color,
                            alpha = alpha,
                            ...)
    } else {
      rect_df <- rect@coords
      p + ggplot2::geom_rect(data = rect_df,
                             mapping = ggplot2::aes(xmin = xmin, xmax = xmax,
                                                    ymin = ymin, ymax = ymax),
                             inherit.aes = FALSE,
                             color = color, fill = fill, alpha = alpha, ...)
    }
}

#' @rdname shape_annotate
#' @export
setMethod("shape_annotate", signature(p = "ANY", shape = "TiffRect"),
          function(p, shape, color = "green", fill = NA, alpha = 0.2, annotate_center = FALSE, n = 120L, ...){
            if (!inherits(p, "ggplot")) stop("p must be a ggplot or TiffPlotData object")
            .annotate_ggplot(p, shape, color = color, fill = fill, alpha = alpha, annotate_center = annotate_center, ...)
          })

#' @rdname shape_annotate
#' @return for `p` as `TiffPlotData`, returns a `TiffPlotData` with the active plot annotated
#' @export
setMethod("shape_annotate", signature(p = "TiffPlotData", shape = "TiffShape"),
          function(p, shape, color = "green", fill = NA, alpha = 0.2, annotate_center = FALSE, n = 120L, ...){
            active_name <- p@activePlot
            if(!grepl("annotated", active_name)){
                anno_name = paste0(active_name, ".annotated")
            }else{
                anno_name = active_name
            }
            p@activePlot = anno_name

            if(!active_name %in% names(p@plots)){
              stop("activePlot is not present in plots")
            }
            active_plot <- p@plots[[active_name]]
            if(!is(active_plot, "ggplot")){
              stop("active plot must be a ggplot object")
            }

            p@plots[[anno_name]] <- shape_annotate(
              active_plot,
              shape,
              color = color,
              fill = fill,
              alpha = alpha,
              annotate_center = annotate_center,
              n = n,
              ...
            )
            validObject(p)
            p
          })

#' @rdname rect_annotate
#' @export
setMethod("rect_annotate", signature(p = "ANY", rect = "ANY"),
      function(p, rect, color = "green", fill = NA, alpha = 0.2, annotate_center = FALSE, ...){
        if (interactive()) .Deprecated("shape_annotate")
              if (!inherits(p, "ggplot")) stop("p must be a ggplot or TiffPlotData object")
              if (!inherits(rect, "TiffRect")) stop("rect must be a TiffRect")
              stop("unexpected error")
          })

#' @rdname rect_annotate
#' @export
setMethod("rect_annotate", signature(p = "ANY", rect = "TiffRect"),
          function(p, rect, color = "green", fill = NA, alpha = 0.2, annotate_center = FALSE, ...){
            if (interactive()) .Deprecated("shape_annotate")
            shape_annotate(p, rect, color = color, fill = fill, alpha = alpha, annotate_center = annotate_center, ...)
          })

#' @rdname rect_annotate
#' @return for `p` as `TiffPlotData`, returns a `TiffPlotData` with the active plot annotated
#' @export
setMethod("rect_annotate", signature(p = "TiffPlotData", rect = "TiffRect"),
          function(p, rect, color = "green", fill = NA, alpha = 0.2, annotate_center = FALSE, ...){
            if (interactive()) .Deprecated("shape_annotate")
            shape_annotate(p, rect, color = color, fill = fill, alpha = alpha, annotate_center = annotate_center, ...)
          })

#' Add a `TiffRect` to a ggplot using `+`
#'
#' Enables syntax like `ggplot(...) + my_rect`.
#'
#' @param object A `TiffRect` object.
#' @param plot A ggplot object.
#' @param object_name Unused; required by `ggplot_add`.
#' @return ggplot object with rectangle annotation added.
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.TiffRect <- function(object, plot, object_name){
  shape_annotate(plot, object)
}

#' Subset a \code{\link{TiffRect}} by row index
#'
#' @param x TiffRect object
#' @param i integer, logical, or character index
#' @param j unused
#' @param drop unused
#' @return a `TiffRect` containing only the selected rows
#' @export
#' @examples
#' r <- TiffRect(xmin = 1:4, xmax = 2:5, ymin = 1:4, ymax = 2:5,
#'               name = c("a", "b", "c", "d"))
#' r[2:3]          # rows 2 and 3
#' r[c(TRUE, FALSE, TRUE, FALSE)]  # logical index
setMethod("[", signature(x = "TiffRect"),
          function(x, i, j, ..., drop = FALSE){
            new("TiffRect", coords = x@coords[i, , drop = FALSE])
          })

#' Combine \code{\link{TiffRect}} objects
#'
#' Concatenates the coordinate rows of two or more `TiffRect` objects.
#' Duplicate names are made unique automatically (same rule as in `TiffRect()`).
#'
#' @param ... one or more `TiffRect` objects
#' @return a single `TiffRect` with all rows combined
#' @export
#' @examples
#' r1 <- TiffRect(0, 5, 0, 5, name = "a")
#' r2 <- TiffRect(10, 15, 10, 15, name = "b")
#' r3 <- c(r1, r2)
setMethod("c", signature(x = "TiffRect"),
          function(x, ...){
            parts <- c(list(x), list(...))
            if(!all(vapply(parts, is, logical(1), "TiffRect"))){
              stop("all arguments to c() must be TiffRect objects")
            }
            combined <- do.call(rbind, lapply(parts, function(r) r@coords))
            rownames(combined) <- NULL
            combined$name <- .unique_shape_names(combined$name)
            new("TiffRect", coords = combined)
          })
