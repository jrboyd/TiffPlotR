#' Rectangle definition S4 class
#'
#' Stores a rectangular region definition with numeric boundaries.
#'
#' @slot xmin Numeric left x coordinate
#' @slot xmax Numeric right x coordinate
#' @slot ymin Numeric bottom y coordinate
#' @slot ymax Numeric top y coordinate
#' @slot name Optional single character name for the rectangle
#'
#' @examples
#' \dontrun{
#' r <- TiffRect(10, 20, 30, 50, name = "roi1")
#' r2 <- rect_shift(r, dx = 5, dy = -2)
#' r3 <- rect_resize_mult(r, fx = 2)
#' p <- ggplot() + geom_point(data = data.frame(x = 1:100, y = rnorm(100)), aes(x,y))
#' p <- rect_annotate(r, p, fill = "red", alpha = 0.2)
#' }
#'
#' @export
setClass("TiffRect",
         slots = list(
           xmin = "numeric",
           xmax = "numeric",
           ymin = "numeric",
           ymax = "numeric",
           name = "character"
         ),
         validity = function(object){
           if(length(object@xmin) != 1 || length(object@xmax) != 1 || length(object@ymin) != 1 || length(object@ymax) != 1) return("All coordinates must be length 1 numeric values")
           if(!is.numeric(object@xmin) || !is.numeric(object@xmax) || !is.numeric(object@ymin) || !is.numeric(object@ymax)) return("Coordinates must be numeric")
           if(object@xmin >= object@xmax) return("xmin must be < xmax")
           if(object@ymin >= object@ymax) return("ymin must be < ymax")
           if(length(object@name) != 1) return("name must be single character")
           TRUE
         }
)

#' Construct a TiffRect
#'
#' @param xmin left x coordinate
#' @param xmax right x coordinate
#' @param ymin bottom y coordinate
#' @param ymax top y coordinate
#' @param name optional name (default "rect")
#' @return a `TiffRect` object
#' @export
TiffRect <- function(xmin, xmax, ymin, ymax, name = "rect"){
  if(any(!is.numeric(c(xmin, xmax, ymin, ymax)))) stop("Coordinates must be numeric")
  if(any(!c(length(xmin), length(xmax), length(ymin), length(ymax))) == 1) stop("All coordinates must be length 1")
  new("TiffRect", xmin = as.numeric(xmin), xmax = as.numeric(xmax), ymin = as.numeric(ymin), ymax = as.numeric(ymax), name = as.character(name))
}

#' Show method for TiffRect
#' @param object TiffRect
setMethod("show", "TiffRect", function(object){
  cat("TiffRect:", object@name, "\n")
  cat("  xmin:", object@xmin, " xmax:", object@xmax, "\n")
  cat("  ymin:", object@ymin, " ymax:", object@ymax, "\n")
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
  rect@xmin <- rect@xmin + dx
  rect@xmax <- rect@xmax + dx
  rect@ymin <- rect@ymin + dy
  rect@ymax <- rect@ymax + dy
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
  cx <- (rect@xmin + rect@xmax) / 2
  cy <- (rect@ymin + rect@ymax) / 2
  w <- as.numeric(width)
  h <- as.numeric(height)
  if(anchor == "center"){
    rect@xmin <- cx - w/2
    rect@xmax <- cx + w/2
    rect@ymin <- cy - h/2
    rect@ymax <- cy + h/2
  } else if(anchor == "topleft"){
    rect@xmax <- rect@xmin + w
    rect@ymax <- rect@ymin + h
  } else if(anchor == "topright"){
    rect@xmin <- rect@xmax - w
    rect@ymax <- rect@ymin + h
  } else if(anchor == "botleft"){
    rect@xmax <- rect@xmin + w
    rect@ymin <- rect@ymax - h
  } else if(anchor == "botright"){
    rect@xmin <- rect@xmax - w
    rect@ymin <- rect@ymax - h
  }
  validObject(rect)
  rect
}

#' Resize a TiffRect by multipliers
#'
#' @param rect TiffRect
#' @param fx multiplier for width (numeric)
#' @param fy multiplier for height (numeric, defaults to fx)
#' @return resized TiffRect
#' @export
rect_resize_mult <- function(rect, fx = 1, fy = NULL){
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect")
  if(is.null(fy)) fy <- fx
  w <- (rect@xmax - rect@xmin) * as.numeric(fx)
  h <- (rect@ymax - rect@ymin) * as.numeric(fy)
  rect_resize_abs(rect, width = w, height = h, anchor = "center")
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
  rect_df <- data.frame(xmin = rect@xmin, xmax = rect@xmax, ymin = rect@ymin, ymax = rect@ymax)
  p + ggplot2::geom_rect(data = rect_df, mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), inherit.aes = FALSE, color = color, fill = fill, alpha = alpha, ...)
}
