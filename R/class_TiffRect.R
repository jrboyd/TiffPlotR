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
         slots = list(
           coords = "data.frame"
         ),
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

utils::globalVariables(c("xmin", "xmax", "ymin", "ymax", "name"))

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
  unique_rect_names <- function(x){
    x <- as.character(x)
    out <- x
    dup_keys <- unique(x[duplicated(x) | duplicated(x, fromLast = TRUE)])
    for(key in dup_keys){
      idx <- which(x == key)
      out[idx] <- paste0(key, "_", seq_along(idx))
    }
    out
  }

  coords_df <- data.frame(
    xmin = as.numeric(recycle(xmin)),
    xmax = as.numeric(recycle(xmax)),
    ymin = as.numeric(recycle(ymin)),
    ymax = as.numeric(recycle(ymax)),
    name = unique_rect_names(recycle(name)),
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
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect")
  rect@coords$xmin <- rect@coords$xmin + dx
  rect@coords$xmax <- rect@coords$xmax + dx
  rect@coords$ymin <- rect@coords$ymin + dy
  rect@coords$ymax <- rect@coords$ymax + dy
  validObject(rect)
  rect
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
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect")
  if(!anchor %in% c("center", "topleft", "topright", "botleft", "botright")){
    stop("anchor must be one of: center, topleft, topright, botleft, botright")
  }
  cx <- (rect@coords$xmin + rect@coords$xmax) / 2
  cy <- (rect@coords$ymin + rect@coords$ymax) / 2
  w <- as.numeric(width)
  h <- as.numeric(height)
  if(anchor == "center"){
    rect@coords$xmin <- cx - w/2
    rect@coords$xmax <- cx + w/2
    rect@coords$ymin <- cy - h/2
    rect@coords$ymax <- cy + h/2
  } else if(anchor == "topleft"){
    rect@coords$xmax <- rect@coords$xmin + w
    rect@coords$ymax <- rect@coords$ymin + h
  } else if(anchor == "topright"){
    rect@coords$xmin <- rect@coords$xmax - w
    rect@coords$ymax <- rect@coords$ymin + h
  } else if(anchor == "botleft"){
    rect@coords$xmax <- rect@coords$xmin + w
    rect@coords$ymin <- rect@coords$ymax - h
  } else if(anchor == "botright"){
    rect@coords$xmin <- rect@coords$xmax - w
    rect@coords$ymin <- rect@coords$ymax - h
  }
  validObject(rect)
  rect
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
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect")
  if(is.null(fy)) fy <- fx
  w <- (rect@coords$xmax - rect@coords$xmin) * as.numeric(fx)
  h <- (rect@coords$ymax - rect@coords$ymin) * as.numeric(fy)
  rect_resize_abs(rect, width = w, height = h, anchor = anchor)
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
#' @param ... additional args passed to geom_rect
#' @return ggplot object with rectangle layer added
#' @export
#' @importFrom ggplot2 geom_rect aes ggplot
methods::setGeneric("rect_annotate", function(p, rect, color = "green", fill = NA, alpha = 0.2, ...){
  standardGeneric("rect_annotate")
})

.annotate_ggplot = function(p, rect, color = "green", fill = NA, alpha = 0.2, ...){
    rect_df <- rect@coords
    p + ggplot2::geom_rect(data = rect_df,
                           mapping = ggplot2::aes(xmin = xmin, xmax = xmax,
                                                  ymin = ymin, ymax = ymax),
                           inherit.aes = FALSE,
                           color = color, fill = fill, alpha = alpha, ...)
}

#' @rdname rect_annotate
#' @export
setMethod("rect_annotate", signature(p = "ANY", rect = "ANY"),
          function(p, rect, color = "green", fill = NA, alpha = 0.2, ...){
              if (!inherits(p, "ggplot")) stop("p must be a ggplot or TiffPlotData object")
              if (!inherits(rect, "TiffRect")) stop("rect must be a TiffRect")
              stop("unexpected error")
          })

#' @rdname rect_annotate
#' @export
setMethod("rect_annotate", signature(p = "ANY", rect = "TiffRect"),
          function(p, rect, color = "green", fill = NA, alpha = 0.2, ...){
            if (!inherits(p, "ggplot")) stop("p must be a ggplot or TiffPlotData object")
            .annotate_ggplot(p, rect, color, fill, alpha, ...)
          })

#' @rdname rect_annotate
#' @return for `p` as `TiffPlotData`, returns a `TiffPlotData` with the active plot annotated
#' @export
setMethod("rect_annotate", signature(p = "TiffPlotData", rect = "TiffRect"),
          function(p, rect, color = "green", fill = NA, alpha = 0.2, ...){
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
            p@plots[[anno_name]] <- .annotate_ggplot(active_plot,
                                                    rect,
                                                    color = color,
                                                    fill = fill,
                                                    alpha = alpha,
                                                    ...)
            validObject(p)
            p
          })

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
            unique_rect_names <- function(nms){
              out <- nms
              dup_keys <- unique(nms[duplicated(nms) | duplicated(nms, fromLast = TRUE)])
              for(key in dup_keys){
                idx <- which(nms == key)
                out[idx] <- paste0(key, "_", seq_along(idx))
              }
              out
            }
            combined$name <- unique_rect_names(combined$name)
            new("TiffRect", coords = combined)
          })
