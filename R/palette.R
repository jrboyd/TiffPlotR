.set1_colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
                  "#ff7f00", "#ffff33", "#a65628", "#f781bf")

#' Named categorical palette covering every level
#'
#' Automatic colours are Set1 for up to 8 levels, else \code{hcl.colors(n, "Dark 3")}.
#' A named \code{palette} then overrides levels by name; an unnamed one overrides
#' positionally. Every level always gets a colour.
#'
#' @param levels character vector of category levels, in display order
#' @param palette optional named or unnamed vector of colours
#' @return named character vector of colours, names = \code{levels}
#' @noRd
.category_palette <- function(levels, palette = NULL) {
  levels <- as.character(levels)
  n <- length(levels)
  auto <- if (n <= 8L) .set1_colors[seq_len(n)] else grDevices::hcl.colors(n, "Dark 3")
  pal <- stats::setNames(auto, levels)
  if (!is.null(palette)) {
    if (!is.null(names(palette))) {
      common <- intersect(names(palette), levels)
      pal[common] <- palette[common]
    } else {
      m <- min(length(palette), n)
      pal[seq_len(m)] <- palette[seq_len(m)]
    }
  }
  pal
}
