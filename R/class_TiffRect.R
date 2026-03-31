#' Rectangle definition S4 class
#'
#' Stores a rectangular region definition with numeric boundaries.
#'
#' @slot coords data.frame with xmin/xmax/ymin/ymax/name columns
#'
#' @examples
#' r <- TiffRect(c(10, 15), 20, 40, 50, name = "roi1")
#' r2 <- rect_shift(r, dx = 5, dy = -2)
#' r3 <- rect_resize_mult(r2, fx = 2, )
#' r4 <- rect_resize_mult(r2, fx = .5)
#'
#' library(ggplot2)
#' library(magrittr)
#' ggplot() %>%
#'   rect_annotate(r, fill = "red", alpha = 0.2) %>%
#'   rect_annotate(r2, fill = "green", alpha = 0.2)
#'
#' ggplot() %>%
#'   rect_annotate(r, fill = "red", alpha = 0.2) %>%
#'   rect_annotate(r3, fill = "blue", alpha = 0.2) %>%
#'   rect_annotate(r4, fill = "yellow", alpha = 0.2)
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

#' Show method for TiffRect
#' @param object TiffRect
setMethod("show", "TiffRect", function(object){
  n <- nrow(object@coords)
  cat("TiffRect with", n, if(n == 1) "rectangle\n" else "rectangles\n")
  print(object@coords, row.names = FALSE)
})

#' Shift a TiffRect by dx and dy
#'
#' @param rect TiffRect object
#' @param dx shift in x direction (numeric)
#' @param dy shift in y direction (numeric)
#' @return shifted TiffRect
#' @export
rect_shift <- function(rect, dx = 0, dy = 0){
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect")
  rect@coords$xmin <- rect@coords$xmin + dx
  rect@coords$xmax <- rect@coords$xmax + dx
  rect@coords$ymin <- rect@coords$ymin + dy
  rect@coords$ymax <- rect@coords$ymax + dy
  validObject(rect)
  rect
}

#' Resize a TiffRect to absolute width/height (anchor=center)
#'
#' @param rect TiffRect
#' @param width desired width (numeric)
#' @param height desired height (numeric)
#' @param anchor where to keep fixed: "center" (default), "topleft", "topright", "botleft", "botright"
#' @return resized TiffRect
#' @export
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

#' Resize a TiffRect by multipliers
#'
#' @param rect TiffRect
#' @param fx multiplier for width (numeric)
#' @param fy multiplier for height (numeric, defaults to fx)
#' @param anchor where to keep fixed: "center" (default), "topleft", "topright", "botleft", "botright"
#' @return resized TiffRect
#' @export
rect_resize_mult <- function(rect, fx = 1, fy = NULL, anchor = "center"){
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect")
  if(is.null(fy)) fy <- fx
  w <- (rect@coords$xmax - rect@coords$xmin) * as.numeric(fx)
  h <- (rect@coords$ymax - rect@coords$ymin) * as.numeric(fy)
  rect_resize_abs(rect, width = w, height = h, anchor = anchor)
}

#' Intersect two TiffRect objects
#'
#' @param rect TiffRect
#' @param other TiffRect
#' @param invert logical; when FALSE (default), returns the intersection rectangle
#'   or NULL when there is no overlap. When TRUE, returns whether rectangles do
#'   not overlap.
#' @param name optional name(s) for returned intersection rectangle(s)
#' @return A `TiffRect`, `NULL`, or logical depending on `invert`
#' @export
#' @examples
#' r1 <- TiffRect(0, 10, 0, 10, name = "a")
#' r2 <- TiffRect(5, 15, 5, 15, name = "b")
#' rect_intersect(r1, r2)
#' rect_intersect(r1, r2, invert = TRUE)
rect_intersect <- function(rect, other, invert = FALSE, name = NULL){
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
  if(isTRUE(invert)) return(!has_overlap)
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

#' Annotate a ggplot with a TiffRect
#'
#' @param rect TiffRect
#' @param p ggplot object
#' @param color border color (default "red")
#' @param fill fill color (default NA)
#' @param alpha fill alpha (default 0.2)
#' @param ... additional args passed to geom_rect
#' @return ggplot object with rectangle layer added
#' @export
#' @importFrom ggplot2 geom_rect aes
rect_annotate <- function(p, rect, color = "green", fill = NA, alpha = 0.2, ...){
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect")
  if(missing(p)) stop("p must be a ggplot object")
  rect_df <- rect@coords
  p + ggplot2::geom_rect(data = rect_df, mapping = ggplot2::aes_string(xmin = "xmin", xmax = "xmax", ymin = "ymin", ymax = "ymax"), inherit.aes = FALSE, color = color, fill = fill, alpha = alpha, ...)
}
