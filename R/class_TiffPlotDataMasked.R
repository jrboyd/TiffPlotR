#' An S4 class for mask-filtered TIFF signal data
#'
#' Extends \linkS4class{TiffPlotData} by storing decoded mask points and
#' mask color mappings.
#'
#' @slot mask_points A data.frame of decoded mask coordinates and metadata.
#' @slot mask_color_mappings A data.frame mapping mask identifiers to colors.
#' @export
setClass(
  "TiffPlotDataMasked",
  contains = "TiffPlotData",
  slots = list(
    mask_points = "data.frame",
    mask_color_mappings = "data.frame"
  ),
  validity = function(object) {
    if (!is.data.frame(object@mask_points)) {
      return("Slot 'mask_points' must be a data.frame")
    }
    if (!is.data.frame(object@mask_color_mappings)) {
      return("Slot 'mask_color_mappings' must be a data.frame")
    }
    TRUE
  }
)


#' Constructor for TiffPlotDataMasked objects
#'
#' @param data A data.frame of image signal data.
#' @param plots A named list of ggplot objects.
#' @param activePlot Active plot name. If NULL, first plot name is used.
#' @param tiff_path Character path to TIFF file.
#' @param resolution Numeric resolution level.
#' @param precalc_max Data frame of min/max normalization values per channel.
#' @param rect A \linkS4class{TiffRect} region.
#' @param img_info TIFF metadata data.frame.
#' @param unit_per_pixel Numeric scalar physical scale from OME metadata.
#' @param unit_name Character scalar physical unit name from OME metadata.
#' @param mask_points Decoded mask points and metadata.
#' @param mask_color_mappings Mask ID/text to color mapping table.
#' @return A \linkS4class{TiffPlotDataMasked} object.
#' @export
TiffPlotDataMasked <- function(data,
                               plots,
                               activePlot = NULL,
                               tiff_path = NA_character_,
                               resolution = NA_real_,
                               precalc_max = data.frame(),
                               rect = TiffRect(0, 1, 0, 1),
                               img_info = data.frame(),
                               unit_per_pixel = NA_real_,
                               unit_name = NA_character_,
                               mask_points = data.frame(),
                               mask_color_mappings = data.frame()) {
  if (is.null(activePlot)) {
    activePlot <- names(plots)[1]
  }

  new(
    "TiffPlotDataMasked",
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
    mask_points = mask_points,
    mask_color_mappings = mask_color_mappings
  )
}


#' @export
setMethod("names", "TiffPlotDataMasked",
          function(x) {
            c(
              callNextMethod(),
              "mask_points",
              "mask_color_mappings"
            )
          })


#' @export
setMethod("$", "TiffPlotDataMasked",
          function(x, name) {
            if (name == "mask_points") {
              return(x@mask_points)
            }
            if (name == "mask_color_mappings") {
              return(x@mask_color_mappings)
            }
            callNextMethod()
          })


#' @export
setMethod("$<-", "TiffPlotDataMasked",
          function(x, name, value) {
            if (name == "mask_points") {
              x@mask_points <- value
            } else if (name == "mask_color_mappings") {
              x@mask_color_mappings <- value
            } else {
              x <- callNextMethod()
            }
            validObject(x)
            x
          })
