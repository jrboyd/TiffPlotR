#' An S4 Class to Store a Data Frame and List of ggplot Objects
#'
#' This class combines a data frame with a list of associated ggplot objects
#'
#' @slot data A data.frame object
#' @slot plots A list of ggplot objects (should be named)
#' @slot activePlot A character string specifying which plot to display (must be a name in the plots list)
#' @slot tiff_path A character scalar giving the path of the TIFF file used to produce the object (may be NA if not applicable)
#' @slot resolution A numeric scalar recording the resolution level read from the TIFF (NA when unknown)
#' @slot precalc_max A data.frame of precalculated min/max values used for normalization
#' @slot rect A \linkS4class{TiffRect} object describing the rectangular region that was plotted
#' @slot img_info A data.frame containing TIFF metadata returned by \code{read_tiff_meta_data} (may be empty)
#'
#' @importFrom methods setClass setMethod
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' df <- data.frame(x = 1:10, y = rnorm(10))
#' p1 <- ggplot(df, aes(x = x, y = y)) + geom_point()
#' r <- TiffRect(0, 10, 0, 10)
#' # manual construction using new():
#' obj <- new("TiffPlotData", data = df, plots = list(scatter=p1), activePlot = "scatter",
#'            tiff_path = NA_character_, resolution = NA_real_, precalc_max = data.frame(), rect = r,
#'            img_info = data.frame())
#' # Preferred convenience constructor:
#' obj2 <- TiffPlotData(df, plots = list(scatter=p1), activePlot = "scatter")
#' }
#'
#' @export
setClass("TiffPlotData",
         slots = list(
           data = "data.frame",
           plots = "list",
           activePlot = "character",
           tiff_path = "character",
           resolution = "numeric",
           precalc_max = "data.frame",
           rect = "ANY",
           img_info = "data.frame"
         ),
         validity = function(object) {
           if (length(object@activePlot) != 1) {
             return("Slot 'activePlot' must be a single character string")
           }
           if (!object@activePlot %in% names(object@plots)) {
             return("'activePlot' must be a name in the plots list")
           }
           if (length(object@tiff_path) != 1) {
             return("Slot 'tiff_path' must be a single character string")
           }
           if (length(object@resolution) != 1) {
             return("Slot 'resolution' must be a single numeric value")
           }
           TRUE
         }
)

#' Show method for TiffPlotData
#' @param object A TiffPlotData object
#' @export
setMethod("show", "TiffPlotData",
          function(object) {
            cat("TiffPlotData object\n")
            cat("Data frame:\n")
            print(head(object@data))
            cat("\nNumber of plots:", length(object@plots), "\n")
            cat("Available plots:", paste(names(object@plots), collapse = ", "), "\n")
            cat("Active plot:", object@activePlot, "\n")
            cat("TIFF path:", object@tiff_path, "\n")
            cat("Resolution:", object@resolution, "\n")
            cat("Precalc max rows:", nrow(object@precalc_max), "\n")
            cat("Rect:")
            show(object@rect)
            cat("img_info rows:", nrow(object@img_info), "\n")
            plot(object)
          }
)

#' Plot method for TiffPlotData
#' @param x A TiffPlotData object
#' @param y Unused
#' @export
setMethod("plot", "TiffPlotData",
          function(x, y = 1, ...) {
            print(x@plots[[x@activePlot]])
          }
)

#' #' Plot method for TiffPlotData
#' #' @param x A TiffPlotData object
#' setMethod("plot", "TiffPlotData",
#'           function(x) {
#'             print(x@plots[[x@activePlot]])
#'           }
#' )


#' Constructor for TiffPlotData objects
#'
#' Create a \\linkS4class{TiffPlotData} with sensible defaults for the
#' metadata slots.  This is simply a thin wrapper around \code{new()}, but
#' is more convenient in client code and examples.
#'
#' @param data A data.frame
#' @param plots A named list of ggplot objects
#' @param activePlot Character name of the initial active plot.  If missing,
#'   the first name in \code{plots} is used.
#' @param tiff_path Character path to TIFF (default \code{NA_character_})
#' @param resolution Numeric resolution level (default \code{NA_real_})
#' @param precalc_max Data frame of precomputed min/max values (default
#'   empty data.frame)
#' @param rect A \linkS4class{TiffRect} describing the region (default a
#'   zero-sized rectangle)
#' @param img_info Data frame returned by \code{read_tiff_meta_data}
#'   (default empty)
#' @return A \linkS4class{TiffPlotData} object
#' @export
TiffPlotData <- function(data,
                         plots,
                         activePlot = NULL,
                         tiff_path = NA_character_,
                         resolution = NA_real_,
                         precalc_max = data.frame(),
                         rect = TiffRect(0,1,0,1),
                         img_info = data.frame()) {
  if (is.null(activePlot)) {
    activePlot <- names(plots)[1]
  }
  new("TiffPlotData",
      data = data,
      plots = plots,
      activePlot = activePlot,
      tiff_path = tiff_path,
      resolution = resolution,
      precalc_max = precalc_max,
      rect = rect,
      img_info = img_info)
}

#' @export
setMethod("names", "TiffPlotData",
          function(x) {
            name_vals = c("data", "plots", "activePlot", "tiff_path", "resolution", "precalc_max", "rect", "img_info")
            return(name_vals)
          }
)

#' $ accessor for TiffPlotData
#' Access slots or data.frame columns
#' @param x A TiffPlotData object
#' @param name Name of slot or data.frame column
#' @export
setMethod("$", "TiffPlotData",
          function(x, name) {
            if (name == "data") {
              return(x@data)
            } else if (name == "plots") {
              return(x@plots)
            } else if (name == "activePlot") {
              return(x@activePlot)
            } else if (name == "tiff_path") {
              return(x@tiff_path)
            } else if (name == "resolution") {
              return(x@resolution)
            } else if (name == "precalc_max") {
              return(x@precalc_max)
            } else if (name == "rect") {
              return(x@rect)
            } else if (name == "img_info") {
              return(x@img_info)
            } else if (name %in% names(x@data)) {
              # Allow direct access to data.frame columns
              return(x@data[[name]])
            } else {
              stop("'", name, "' not found in slots or data.frame columns")
            }
          }
)

#' $<- assignment accessor for TiffPlotData
#' Set slots or data.frame columns
#' @param x A TiffPlotData object
#' @param name Name of slot or data.frame column
#' @param value Value to assign
#' @export
setMethod("$<-", "TiffPlotData",
          function(x, name, value) {
            if (name == "data") {
              x@data <- value
            } else if (name == "plots") {
              x@plots <- value
            } else if (name == "activePlot") {
              x@activePlot <- value
            } else if (name == "tiff_path") {
              x@tiff_path <- value
            } else if (name == "resolution") {
              x@resolution <- value
            } else if (name == "precalc_max") {
              x@precalc_max <- value
            } else if (name == "rect") {
              x@rect <- value
            } else if (name == "img_info") {
              x@img_info <- value
            } else {
              # Set as a column in the data.frame
              x@data[[name]] <- value
            }
            validObject(x)
            return(x)
          }
)
