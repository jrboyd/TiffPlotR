#' TiffPlotR: ggplot2 plots from (OME-)TIFF images
#'
#' Efficient ggplot2 plots of regions of multi-resolution (OME-)TIFF images,
#' plus shape classes for annotating them. Start with \code{fetchTiffData()}.
#'
#' @section Options:
#' TiffPlotR reads these \code{\link[base]{options}}:
#' \describe{
#'   \item{\code{TPR_SCALE_COLORS}}{Default fill gradient colours for
#'     single-channel image plots: a character vector of at least 2 colours,
#'     low to high. It is used whenever \code{scale_colors} isn't given to
#'     \code{fetchTiffData()} or \code{fetchTiffDataMasked()}. If unset, the
#'     magma palette is used. The colours used are recorded in each object's
#'     \code{@plot_parameters$scale_colors}, so changing the option later does
#'     not change objects already made. An invalid value is an error at the
#'     next fetch.}
#' }
#'
#' @examples
#' \dontrun{
#' options(TPR_SCALE_COLORS = c("black", "darkgreen", "yellow"))
#' img <- fetchTiffData(exampleTiff(), TiffRect(900, 2300, 1400, 2800))
#' img@plot_parameters$scale_colors
#' options(TPR_SCALE_COLORS = NULL)  # back to magma
#' }
#'
#' @keywords internal
"_PACKAGE"
