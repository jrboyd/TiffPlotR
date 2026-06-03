#' Add a micron scale bar to a plot
#'
#' Adds a horizontal scale bar labeled in microns to a ggplot or to the active
#' plot inside a \linkS4class{TiffPlotData} object.
#'
#' @param p A ggplot object or a \linkS4class{TiffPlotData} object.
#' @param unit_per_pixel Microns per pixel (numeric scalar, must be > 0). If
#'   NULL and \code{p} is \code{TiffPlotData}, this is inferred from
#'   \code{p@unit_per_pixel} and \code{p@unit_name} when available.
#' @param bar_um Length of the scale bar in microns. If \code{NULL}, a
#'   "nice" value near \code{bar_fraction} of the visible width is chosen.
#' @param label Optional custom label. If \code{NULL}, uses
#'   \code{"<bar_um> um"}.
#' @param position One of \code{"bottomleft"}, \code{"bottomright"},
#'   \code{"topleft"}, \code{"topright"}.
#' @param bar_fraction Target fraction of visible width used when
#'   \code{bar_um = NULL}.
#' @param pad_fraction Padding from plot edges, as a fraction of panel width/
#'   height.
#' @param label_offset_fraction Vertical offset of the label from the bar, as
#'   a fraction of panel height.
#' @param line_color Scale-bar line color.
#' @param text_color Label text color.
#' @param text_size Label text size.
#' @param line_width Scale-bar line width.
#' @param xlim Optional numeric length-2 x-range in pixel units. Most users
#'   should leave this as \code{NULL}.
#' @param ylim Optional numeric length-2 y-range in pixel units. Most users
#'   should leave this as \code{NULL}.
#'
#' @return A ggplot object, or a \linkS4class{TiffPlotData} object if
#'   \code{p} is \code{TiffPlotData}.
#' @export
add_um_scale_bar <- function(p,
                             unit_per_pixel = NULL,
                             unit_name = NULL,
                             bar_um = NULL,

                             label = NULL,
                             position = c("bottomleft", "bottomright", "topleft", "topright"),
                             bar_fraction = 0.20,
                             pad_fraction = 0.04,
                             label_offset_fraction = 0.03,
                             line_color = "white",
                             text_color = line_color,
                             text_size = 3.5,
                             line_width = 1,
                             xlim = NULL,
                             ylim = NULL) {
  position <- match.arg(position)

  if (!is.numeric(bar_fraction) || length(bar_fraction) != 1L || !is.finite(bar_fraction) || bar_fraction <= 0 || bar_fraction >= 1) {
    stop("bar_fraction must be a single numeric value in (0, 1)")
  }

  if (!is.numeric(pad_fraction) || length(pad_fraction) != 1L || !is.finite(pad_fraction) || pad_fraction < 0 || pad_fraction >= 0.5) {
    stop("pad_fraction must be a single numeric value in [0, 0.5)")
  }

  if (!is.numeric(label_offset_fraction) || length(label_offset_fraction) != 1L || !is.finite(label_offset_fraction) || label_offset_fraction < 0) {
    stop("label_offset_fraction must be a single non-negative numeric value")
  }

  if (is(p, "TiffPlotData")) {
    active_name <- p@activePlot
    if (!active_name %in% names(p@plots)) {
      stop("activePlot is not present in plots")
    }

    xlim <- c(min(p@rect@coords$xmin), max(p@rect@coords$xmax))
    ylim <- c(min(p@rect@coords$ymin), max(p@rect@coords$ymax))

    # if (is.null(unit_per_pixel)) {
    #   if (is.finite(p@unit_per_pixel) && !is.na(p@unit_name) && nzchar(p@unit_name)) {
    #     unit_per_pixel <- .scale_bar_convert_to_um(p@unit_per_pixel, p@unit_name)
    #   }
    # }

    if(is.null(unit_per_pixel)){
        unit_per_pixel = p@unit_per_pixel
    }
    if(is.null(unit_name)){
        unit_name = p@unit_per_pixel
    }


    p@plots[[active_name]] <- add_um_scale_bar(
      p@plots[[active_name]],
      unit_per_pixel = unit_per_pixel,
      unit_name = unit_name,
      bar_um = bar_um,
      label = label,
      position = position,
      bar_fraction = bar_fraction,
      pad_fraction = pad_fraction,
      label_offset_fraction = label_offset_fraction,
      line_color = line_color,
      text_color = text_color,
      text_size = text_size,
      line_width = line_width,
      xlim = xlim,
      ylim = ylim
    )
    validObject(p)
    return(p)
  }

  if (!is.numeric(unit_per_pixel) || length(unit_per_pixel) != 1L || !is.finite(unit_per_pixel) || unit_per_pixel <= 0) {
    stop("unit_per_pixel must be provided as a single positive numeric value")
  }

  if (!inherits(p, "ggplot")) {
    stop("p must be a ggplot or TiffPlotData object")
  }

  if (is.null(xlim) || is.null(ylim)) {
    panel_ranges <- .scale_bar_panel_ranges(p)
    if (is.null(xlim)) xlim <- panel_ranges$x
    if (is.null(ylim)) ylim <- panel_ranges$y
  }

  if (!is.numeric(xlim) || length(xlim) != 2L || any(!is.finite(xlim))) {
    stop("xlim must be numeric length 2")
  }
  if (!is.numeric(ylim) || length(ylim) != 2L || any(!is.finite(ylim))) {
    stop("ylim must be numeric length 2")
  }

  x_min <- min(xlim)
  x_max <- max(xlim)
  y_min <- min(ylim)
  y_max <- max(ylim)

  width_px <- x_max - x_min
  height_px <- y_max - y_min

  if (width_px <= 0 || height_px <= 0) {
    stop("invalid panel extent: zero or negative width/height")
  }

  if (is.null(bar_um)) {
    target_um <- width_px * unit_per_pixel * bar_fraction
    bar_um <- .scale_bar_nice_value(target_um)
  }

  if (!is.numeric(bar_um) || length(bar_um) != 1L || !is.finite(bar_um) || bar_um <= 0) {
    stop("bar_um must be a single positive numeric value")
  }

  bar_px <- bar_um / unit_per_pixel
  if (bar_px >= width_px) {
    stop("bar_um is too large for the visible x-range")
  }

  x_rev <- .scale_bar_axis_reversed(p, "x")
  y_rev <- .scale_bar_axis_reversed(p, "y")

  is_left <- grepl("left$", position)
  is_bottom <- grepl("^bottom", position)

  x_start <- if (xor(is_left, x_rev)) x_min + width_px * pad_fraction else x_max - width_px * pad_fraction - bar_px
  x_end <- x_start + bar_px

  y_bar <- if (xor(is_bottom, y_rev)) y_min + height_px * pad_fraction else y_max - height_px * pad_fraction

  # Place labels away from the nearest edge so they remain legible.
  label_direction <- if (xor(is_bottom, y_rev)) 1 else -1
  y_label <- y_bar + label_direction * (height_px * label_offset_fraction)

  if (is.null(label)) {
    label <- paste0(formatC(bar_um, format = "fg", digits = 3), " ", unit_name)
  }

  p +
    ggplot2::annotate(
      "segment",
      x = x_start,
      xend = x_end,
      y = y_bar,
      yend = y_bar,
      color = line_color,
      linewidth = line_width,
      lineend = "butt"
    ) +
    ggplot2::annotate(
      "text",
      x = (x_start + x_end) / 2,
      y = y_label,
      label = label,
      color = text_color,
      size = text_size,
      vjust = if (label_direction > 0) 0 else 1
    )
}
#
# .scale_bar_convert_to_um <- function(value, unit_name) {
#   unit_key <- tolower(gsub("\\s+", "", as.character(unit_name)))
#   mult <- switch(
#     unit_key,
#     "um" = 1,
#     "µm" = 1,
#     "micrometer" = 1,
#     "micrometers" = 1,
#     "micron" = 1,
#     "microns" = 1,
#     "nm" = 1e-3,
#     "nanometer" = 1e-3,
#     "nanometers" = 1e-3,
#     "mm" = 1e3,
#     "millimeter" = 1e3,
#     "millimeters" = 1e3,
#     "cm" = 1e4,
#     "centimeter" = 1e4,
#     "centimeters" = 1e4,
#     "m" = 1e6,
#     "meter" = 1e6,
#     "meters" = 1e6,
#     NA_real_
#   )
#
#   if (!is.finite(mult)) {
#     stop("Cannot convert unit_name to microns automatically; please pass unit_per_pixel explicitly")
#   }
#
#   as.numeric(value) * mult
# }

.scale_bar_nice_value <- function(x) {
  if (!is.finite(x) || x <= 0) {
    stop("target scale-bar length must be positive")
  }

  expo <- floor(log10(x))
  base <- x / (10^expo)

  mantissa <- if (base <= 1) {
    1
  } else if (base <= 2) {
    2
  } else if (base <= 5) {
    5
  } else {
    10
  }

  mantissa * (10^expo)
}

.scale_bar_axis_reversed <- function(p, axis = c("x", "y")) {
  axis <- match.arg(axis)
  sc <- p$scales$get_scales(axis)
  if (is.null(sc) || is.null(sc$trans) || is.null(sc$trans$name)) {
    return(FALSE)
  }
  identical(sc$trans$name, "reverse")
}

.scale_bar_panel_ranges <- function(p) {
  b <- ggplot2::ggplot_build(p)
  pp <- b$layout$panel_params[[1]]

  x <- NULL
  y <- NULL

  if (!is.null(pp$x.range)) x <- pp$x.range
  if (!is.null(pp$y.range)) y <- pp$y.range

  if (is.null(x) && !is.null(pp$x$range$range)) x <- pp$x$range$range
  if (is.null(y) && !is.null(pp$y$range$range)) y <- pp$y$range$range

  if (is.null(x) || is.null(y) || any(!is.finite(x)) || any(!is.finite(y))) {
    stop("Could not infer plot range. Provide explicit xlim and ylim.")
  }

  list(x = x, y = y)
}
