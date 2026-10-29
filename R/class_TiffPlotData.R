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
#' @slot unit_per_pixel Numeric scalar from OME metadata (for example microns per pixel in X/Y),
#'   or NA when unavailable.
#' @slot unit_name Character scalar naming the physical unit for \code{unit_per_pixel}
#'   (for example \code{"um"}), or NA when unavailable.
#' @slot channels A data.frame describing every channel in the image, one row
#'   per channel: \code{channel} (number), \code{channel_name} (the label used
#'   in \code{@data} and plots), \code{ome_name} and \code{color} (from the
#'   OME-XML, may be NA). Empty for objects not made from a TIFF.
#' @slot package_version Character scalar: the version of TiffPlotR that
#'   created the object, set automatically. Objects saved before 0.1.20 do not
#'   have this slot (check with \code{methods::.hasSlot()}).
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
           img_info = "data.frame",
           unit_per_pixel = "numeric",
           unit_name = "character",
           channels = "data.frame",
           package_version = "character"
         ),
         prototype = list(
           unit_per_pixel = NA_real_,
           unit_name = NA_character_,
           channels = data.frame(),
           package_version = NA_character_
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
           if (length(object@unit_per_pixel) != 1) {
             return("Slot 'unit_per_pixel' must be a single numeric value")
           }
           if (length(object@unit_name) != 1) {
             return("Slot 'unit_name' must be a single character string")
           }
           if (length(object@package_version) != 1) {
             return("Slot 'package_version' must be a single character string")
           }
           if (nrow(object@channels) > 0) {
             if (!all(c("channel", "channel_name") %in% colnames(object@channels))) {
               return("Slot 'channels' must have columns 'channel' and 'channel_name'")
             }
             if (anyDuplicated(object@channels$channel) || anyDuplicated(object@channels$channel_name)) {
               return("Slot 'channels' must have unique channel numbers and names")
             }
           }
           TRUE
         }
)

.tiffplotr_version <- function() {
  as.character(utils::packageVersion("TiffPlotR"))
}

# Stamp every new object with the TiffPlotR version that made it. readRDS()
# does not call initialize, so saved objects keep the version they were made with.
setMethod("initialize", "TiffPlotData",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            if (is.na(.Object@package_version)) {
              .Object@package_version <- .tiffplotr_version()
            }
            .Object
          }
)

#' Show method for TiffPlotData
#' @param object A TiffPlotData object
#' @export
setMethod("show", "TiffPlotData",
          function(object) {
            object <- .check_TiffPlotData_version(object)
            cat("TiffPlotData object\n")
            cat("Data frame:\n")
            print(head(object@data))
            cat("\nNumber of plots:", length(object@plots), "\n")
            cat("Available plots:", paste(names(object@plots), collapse = ", "), "\n")
            cat("Active plot:", object@activePlot, "\n")
            cat("TIFF path:", object@tiff_path, "\n")
            cat("Resolution:", object@resolution, "\n")
            cat("Pixels per unit:", object@unit_per_pixel, "\n")
            cat("Unit name:", object@unit_name, "\n")
            cat("TiffPlotR version:", if (methods::.hasSlot(object, "package_version")) object@package_version else "before 0.1.20", "\n")
            cat("Precalc max rows:", nrow(object@precalc_max), "\n")
            if (nrow(object@channels) > 0) {
              cat("Channels:", paste0(object@channels$channel, "=", object@channels$channel_name, collapse = ", "), "\n")
            }
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
            x <- .check_TiffPlotData_version(x)
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
#' @param unit_per_pixel Numeric scalar physical scale from OME metadata
#'   (default \code{NA_real_}).
#' @param unit_name Character scalar physical unit name from OME metadata
#'   (default \code{NA_character_}).
#' @param channels Data frame describing the channels; see the
#'   \code{channels} slot. If NULL (the default), it is built from the channel
#'   numbers in \code{data$channel}, each named by its number (empty if
#'   \code{data} has no numeric \code{channel} column).
#' @return A \linkS4class{TiffPlotData} object
#' @export
TiffPlotData <- function(data,
                         plots,
                         activePlot = NULL,
                         tiff_path = NA_character_,
                         resolution = NA_real_,
                         precalc_max = data.frame(),
                         rect = TiffRect(0,1,0,1),
                         img_info = data.frame(),
                         unit_per_pixel = NA_real_,
                         unit_name = NA_character_,
                         channels = NULL) {
  if (is.null(activePlot)) {
    activePlot <- names(plots)[1]
  }
  if (is.null(channels)) {
    channels <- .default_channel_table(data)
  }
  new("TiffPlotData",
      data = data,
      plots = plots,
      activePlot = activePlot,
      tiff_path = tiff_path,
      resolution = resolution,
      precalc_max = precalc_max,
      rect = rect,
      img_info = img_info,
      unit_per_pixel = unit_per_pixel,
      unit_name = unit_name,
      channels = channels)
}

#' @export
setMethod("names", "TiffPlotData",
          function(x) {
            name_vals = c("data", "plots", "activePlot", "tiff_path", "resolution", "precalc_max", "rect", "img_info", "unit_per_pixel", "unit_name", "channels", "package_version")
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
            x <- .check_TiffPlotData_version(x)
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
            } else if (name == "unit_per_pixel") {
              return(x@unit_per_pixel)
            } else if (name == "unit_name") {
              return(x@unit_name)
            } else if (name == "channels") {
              return(x@channels)
            } else if (name == "package_version") {
              return(x@package_version)
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
            x <- .check_TiffPlotData_version(x)
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
            } else if (name == "unit_per_pixel") {
              x@unit_per_pixel <- value
            } else if (name == "unit_name") {
              x@unit_name <- value
            } else if (name == "channels") {
              x@channels <- value
            } else if (name == "package_version") {
              x@package_version <- value
            } else {
              # Set as a column in the data.frame
              x@data[[name]] <- value
            }
            validObject(x)
            return(x)
          }
)
