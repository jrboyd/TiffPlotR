% Generated manually to match current package docs workflow
\name{decode_ome_masks}
\alias{decode_ome_masks}
\title{Decode OME mask annotations}
\usage{
decode_ome_masks(mask_df, bit_order = c("msb", "lsb"), output = c("points", "matrix"))
}
\arguments{
\item{mask_df}{Data frame containing \code{node_text}, \code{Width}, \code{Height}, \code{X}, and \code{Y}.}
\item{bit_order}{Bit order used when interpreting packed bits.}
\item{output}{Either \code{"points"} or \code{"matrix"}.}
}
\value{
When \code{output = "matrix"}, returns \code{mask_df} with a \code{mask_matrix}
list-column. When \code{output = "points"}, returns point coordinates with
metadata columns carried through from \code{mask_df}.
}
\description{
Decodes base64-encoded OME \code{Mask} payloads into either binary matrices
or sparse point coordinates.
}
\examples{
df <- data.frame(
  node_text = "gA==",
  Width = 2,
  Height = 2,
  X = 10,
  Y = 20,
  stringsAsFactors = FALSE
)
decode_ome_masks(df, output = "points")
}
