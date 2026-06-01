% Generated manually to match current package docs workflow
\name{fetchTiffAnnotations}
\alias{fetchTiffAnnotations}
\title{Fetch OME ROI annotations from a TIFF}
\usage{
fetchTiffAnnotations(
  tiff_path = NULL,
  ome_doc = NULL,
  decode_masks = FALSE,
  bit_order = c("msb", "lsb"),
  include_summary = TRUE
)
}
\arguments{
\item{tiff_path}{Path to the TIFF image file. Required when \code{ome_doc} is NULL.}
\item{ome_doc}{Optional xml2 document.}
\item{decode_masks}{If TRUE, decode mask bitmaps to point coordinates.}
\item{bit_order}{Bit order to use when decoding mask payloads.}
\item{include_summary}{If TRUE, include a node-type summary table.}
}
\value{
A named list with ROI annotations: \code{summary}, \code{roi}, \code{labels},
\code{polygons}, \code{ellipses}, \code{masks}, and \code{mask_points}.
}
\description{
Reads OME-XML metadata and extracts ROI-linked shape annotations
(labels, polygons, ellipses, and masks).
}
\examples{
\dontrun{
ann <- fetchTiffAnnotations(exampleTiff())
names(ann)
}
}
