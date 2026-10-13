#' Add coloured points to a TiffPlotR plot without losing the channel legend
#'
#' The rgb/colorized plots use the `colour` scale for their channel legend, so
#' adding a second `scale_colour_manual()` silently replaces it. `add_points()`
#' adds the points on a new colour scale (via ggnewscale) so both legends survive,
#' and does so without touching any other copy of `p`.
#'
#' @param p A ggplot, e.g. `img@plots$colorized`.
#' @param df data.frame of points.
#' @param x,y Column names in `df` holding point coordinates (same pixel space as `p`).
#' @param colour_by Optional column name in `df` to colour points by (treated as
#'   categorical). `NULL` draws all points in one colour.
#' @param palette Colours for `colour_by` levels. A named vector overrides levels by
#'   name, an unnamed one positionally; levels left over get automatic colours.
#'   When `colour_by` is `NULL`, the single point colour (default `"white"`).
#' @param size,alpha,shape Passed to `geom_point()`.
#' @param legend Show a legend for `colour_by`.
#' @param name Legend title.
#' @param drop Drop unused factor levels from the legend.
#' @return `p` with a point layer added.
#' @details Level order is the factor levels when `df[[colour_by]]` is a factor,
#'   otherwise most frequent first.
#'
#'   Why not plain `ggnewscale::new_scale_colour()`: in ggplot2 >= 3.5, `p$guides`
#'   is a ggproto object (an environment) shared by every copy of a plot, and
#'   `new_scale_colour()` renames the colour guide in place. Every sibling plot
#'   built from the same `img@plots$colorized` would then lose its channel legend.
#'   `p$guides` is therefore replaced with a fresh Guides object first.
#' @export
#' @examples
#' p <- ggplot2::ggplot()
#' pts <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3), gene = c("A", "B", "A"))
#' add_points(p, pts, colour_by = "gene")
add_points <- function(p, df, x = "x", y = "y", colour_by = NULL, palette = NULL,
                       size = 0.6, alpha = 1, shape = 16, legend = TRUE,
                       name = colour_by, drop = TRUE) {
  if (is.null(df) || !nrow(df)) { message("no points to add"); return(p) }
  df <- as.data.frame(df)
  missing_cols <- setdiff(c(x, y, colour_by), colnames(df))
  if (length(missing_cols)) stop("columns not in df: ", paste(missing_cols, collapse = ", "), call. = FALSE)

  if (is.null(colour_by)) {
    col <- if (is.null(palette)) "white" else palette[1]
    return(p + ggplot2::geom_point(data = df, ggplot2::aes(x = .data[[x]], y = .data[[y]]),
                                   colour = col, size = size, alpha = alpha, shape = shape,
                                   inherit.aes = FALSE))
  }

  v <- df[[colour_by]]
  lev <- if (is.factor(v)) levels(v) else names(sort(table(v), decreasing = TRUE))
  pal <- .category_palette(lev, palette)
  df[[colour_by]] <- factor(as.character(v), levels = lev)

  if (p$scales$has_scale("colour")) {
    if (!requireNamespace("ggnewscale", quietly = TRUE))
      stop("p already has a colour scale; install ggnewscale to add a second one", call. = FALSE)
    # fresh Guides object so new_scale_colour() can't rename guides on copies of p;
    # ggproto(NULL, p$guides) looks equivalent but overflows the S7 node stack
    g <- p$guides$guides
    p$guides <- ggplot2::ggplot()$guides
    if (length(g)) p <- p + do.call(ggplot2::guides, g)
    p <- p + ggnewscale::new_scale_colour()
  }

  p +
    ggplot2::geom_point(data = df,
                        ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = .data[[colour_by]]),
                        size = size, alpha = alpha, shape = shape, inherit.aes = FALSE) +
    ggplot2::scale_colour_manual(values = pal, drop = drop, name = name,
                                 guide = if (legend) ggplot2::guide_legend(override.aes = list(size = 3)) else "none")
}
