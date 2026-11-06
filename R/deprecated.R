#' Deprecated functions in TiffPlotR
#'
#' These still work, with a warning, and will be removed in a later version.
#' \itemize{
#'   \item \code{gather_channel_quantiles()}: use
#'     \code{scale_channels_gather_quantiles()}.
#'   \item \code{precalc_from_quantiles()}: use
#'     \code{scale_channels_set_thresholds()}.
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
