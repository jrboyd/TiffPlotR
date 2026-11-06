#' Deprecated functions in TiffPlotR
#'
#' These still work, with a warning, and will be removed in a later version.
#' \itemize{
#'   \item \code{gather_channel_quantiles()}: use
#'     \code{scale_channels_gather_quantiles()}.
#'   \item \code{precalc_from_quantiles()}: use
#'     \code{scale_channels_set_thresholds()}.
#'   \item \code{fetchTiffData.rgb()}: use \code{fetchTiffDataRGB()}.
#'   \item \code{fetchTiffData.colorized()}: use
#'     \code{fetchTiffDataColorized()}.
#'   \item \code{plotTiffData.rgb()}: use \code{plotTiffDataRGB()}.
#'   \item \code{plotTiffData.colorized()}: use
#'     \code{plotTiffDataColorized()}.
#' }
#'
#' @param ... Passed to the replacement function.
#'
#' @returns The replacement function's result.
#' @name TiffPlotR-deprecated
#' @keywords internal
NULL

#' @rdname TiffPlotR-deprecated
#' @export
gather_channel_quantiles = function(...){
  .Deprecated("scale_channels_gather_quantiles", package = "TiffPlotR")
  scale_channels_gather_quantiles(...)
}

#' @rdname TiffPlotR-deprecated
#' @export
precalc_from_quantiles = function(...){
  .Deprecated("scale_channels_set_thresholds", package = "TiffPlotR")
  scale_channels_set_thresholds(...)
}

#' @rdname TiffPlotR-deprecated
#' @export
fetchTiffData.rgb = function(...){
  .Deprecated("fetchTiffDataRGB", package = "TiffPlotR")
  fetchTiffDataRGB(...)
}

#' @rdname TiffPlotR-deprecated
#' @export
fetchTiffData.colorized = function(...){
  .Deprecated("fetchTiffDataColorized", package = "TiffPlotR")
  fetchTiffDataColorized(...)
}

#' @rdname TiffPlotR-deprecated
#' @export
plotTiffData.rgb = function(...){
  .Deprecated("plotTiffDataRGB", package = "TiffPlotR")
  plotTiffDataRGB(...)
}

#' @rdname TiffPlotR-deprecated
#' @export
plotTiffData.colorized = function(...){
  .Deprecated("plotTiffDataColorized", package = "TiffPlotR")
  plotTiffDataColorized(...)
}
