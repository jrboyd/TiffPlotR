# Plot builders shared by the fetch functions and the plotTiffData* wrappers.
# Each takes tidy image data (one row per pixel and channel) and returns a
# ggplot limited to rect with coord_fixed().

# Single-channel images faceted by channel, filled by value_var on a gradient.
.plot_channels = function(df, rect, value_var = "norm_value", scale_colors = .default_scale_colors(),
                          facets = facet_wrap(~channel_name)){
  p = ggplot(df, aes(x = i, y = j, fill = !!sym(value_var))) +
    facets +
    scale_y_reverse() +
    .fill_gradient_layers(scale_colors)
  .apply_coord_rect(p, rect, ggplot2::coord_fixed)
}

# Mask-filtered images: a channel by segment (Text) grid.
.plot_masked = function(df, rect, value_var = "norm_value", scale_colors = .default_scale_colors()){
  .plot_channels(df, rect, value_var = value_var, scale_colors = scale_colors,
                 facets = facet_grid(channel_name~Text)) +
    labs(x = "pixels", y = "pixels")
}

# Three channels as the red, green and blue of one image.
.plot_rgb = function(df, channel_table, rgb_channels, value_var = "norm_value", rect){
  rgb_df = convertTidyToRGB(df, red_channel = rgb_channels[1], green_channel = rgb_channels[2],
                            blue_channel = rgb_channels[3], value_var = value_var)
  leg_df = data.frame(channel = rgb_channels)
  leg_df$colors = c("red", "green", "blue")
  leg_df$channel_name = channel_table$channel_name[match(leg_df$channel, channel_table$channel)]
  leg_cols = stats::setNames(leg_df$colors, leg_df$channel_name)
  .plot_composite(rgb_df, leg_df, leg_cols, rect,
                  recipe = list(type = "rgb", channels = rgb_channels, value_var = value_var))
}

# Any number of channels, each tinted by its colour and blended.
.plot_colorized = function(df, channel_map, value_var = "norm_value", rect){
  rgb_df = .convert_tidy_to_colorized(img_df = df, channel_map = channel_map, value_var = value_var)
  leg_df = channel_map
  leg_df$channel_name = factor(leg_df$channel_name, levels = leg_df$channel_name)
  leg_cols = stats::setNames(leg_df$color, as.character(leg_df$channel_name))
  .plot_composite(rgb_df, leg_df, leg_cols, rect,
                  recipe = list(type = "colorized", channel_map = channel_map, value_var = value_var))
}

# rgb/colorized plot from precomputed colours. The channel legend is an
# invisible point layer on the colour scale. recipe goes in
# p$meta$tiffplotr_composite so renormalize() can rebuild the plot.
.plot_composite = function(rgb_df, leg_df, leg_cols, rect, recipe){
  suppressWarnings({
    p = ggplot() +
      .fill_identity_layers(rgb_df) +
      geom_point(data = leg_df, aes(color = channel_name), x = NA, y = NA, alpha = 0, na.rm = TRUE) +
      scale_color_manual(values = leg_cols, name = NULL) +
      guides(color = guide_legend(override.aes = list(size = 12, shape = 16, alpha = 1),
                                  theme = theme(legend.background = element_blank(), legend.key = element_blank()))) +
      scale_y_reverse() +
      theme(panel.spacing = unit(0, "npc")) +
      labs(x = "pixel", y = "pixel")
  })
  p = .apply_coord_rect(p, rect, ggplot2::coord_fixed)
  p$meta$tiffplotr_composite = recipe
  p
}

#' Plot a TiffPlotData object
#'
#' Builds a new plot from the data already in a \linkS4class{TiffPlotData}
#' object, without reading the TIFF again. Use it to change which channels are
#' shown, their order, the colours or the region after a fetch.
#'
#' \itemize{
#'   \item \code{plotTiffData()}: one panel per channel, as
#'     \code{fetchTiffData()} draws.
#'   \item \code{plotTiffDataMasked()}: a channel by segment grid, as
#'     \code{fetchTiffDataMasked()} draws. Needs the \code{Text} column of
#'     masked data.
#'   \item \code{plotTiffData.rgb()}: three channels as red, green and blue, as
#'     \code{fetchTiffData.rgb()} draws.
#'   \item \code{plotTiffData.colorized()}: channels tinted by colour and
#'     blended, as \code{fetchTiffData.colorized()} draws.
#' }
#'
#' Only channels present in \code{@data} can be plotted, that is those kept by
#' the fetch's \code{selected_channels}.
#'
#' @param img_data A \linkS4class{TiffPlotData} object (or subclass).
#' @param selected_channels Channels to show, as numbers or names, in the order
#'   to show them. NULL shows every channel in \code{@data}, in their current
#'   order.
#' @param value_var \code{"norm_value"} (the default) to plot normalized values,
#'   or \code{"value"} to plot raw values.
#' @param scale_colors Colours of the fill gradient, low to high. NULL uses
#'   \code{@plot_parameters$scale_colors}, i.e. the colours the object was
#'   fetched with.
#' @param rect A \linkS4class{TiffRect} giving the region to show. NULL uses
#'   \code{@rect}. A smaller rect zooms in; the data are not cropped.
#' @param ncol,nrow Number of facet columns or rows, passed to
#'   \code{facet_wrap()}.
#' @param um_axes If TRUE, label the axes in microns (see
#'   \code{set_axes_um()}). NULL uses \code{@plot_parameters$um_axes}, else
#'   the \code{TPR_UM_AXES} option (see \link{TiffPlotR-package}), else FALSE.
#' @param scale_bar If TRUE, add a micron scale bar (see
#'   \code{add_um_scale_bar()}). NULL uses \code{@plot_parameters$scale_bar},
#'   else the \code{TPR_UM_SCALE_BAR} option, else FALSE. Both need the
#'   image's physical pixel size (\code{@unit_per_pixel}); without it the plot
#'   stays in pixels, with a warning.
#' @param scale_bar_style A named list of \code{add_um_scale_bar()}
#'   arguments for the bar, e.g. \code{list(line_color = "black", position =
#'   "bottomright")}. NULL uses \code{@plot_parameters$scale_bar_style}, else
#'   the \code{TPR_SCALE_BAR_STYLE} option, else the bar's defaults.
#' @param plot_theme A ggplot2 theme added to the plot. NULL uses
#'   \code{@plot_parameters$plot_theme}, else the \code{TPR_THEME} option,
#'   else none.
#' @param return_object If FALSE (the default), return the ggplot. If TRUE,
#'   return \code{img_data} with the plot added to \code{@plots} as
#'   \code{plot_name} and made the active plot. \code{um_axes},
#'   \code{scale_bar}, \code{scale_bar_style} and \code{plot_theme} are
#'   recorded in \code{@plot_parameters}, and for gradient plots
#'   \code{scale_colors} too.
#' @param plot_name Name for the plot in \code{@plots} when
#'   \code{return_object = TRUE}. Defaults to the name the fetch functions use:
#'   \code{"normalized"} or \code{"raw"}, \code{"masked"}, \code{"rgb"} or
#'   \code{"colorized"}. An existing plot of that name is replaced.
#'
#' @returns A ggplot, or the updated \code{img_data} if
#'   \code{return_object = TRUE}.
#' @export
#'
#' @examples
#' img <- fetchTiffData(exampleTiff(), TiffRect(1, 1024, 1, 1024),
#'                      channel_names = "ome")
#' plotTiffData(img, selected_channels = c("Opal 690", "DAPI"), ncol = 1,
#'              scale_colors = c("black", "white"))
#' plotTiffData.rgb(img, red_channel = "Opal 690", green_channel = "Autofluorescence",
#'                  blue_channel = "DAPI")
#' plotTiffData.colorized(img, channel_colors = list(DAPI = "blue", "Opal 690" = "yellow"),
#'                        rect = TiffRect(1, 600, 1, 600))
#' img <- plotTiffData(img, value_var = "value", return_object = TRUE)
#' names(img@plots)
plotTiffData = function(img_data,
                        selected_channels = NULL,
                        value_var = c("norm_value", "value"),
                        scale_colors = NULL,
                        rect = NULL,
                        ncol = NULL,
                        nrow = NULL,
                        um_axes = NULL,
                        scale_bar = NULL,
                        scale_bar_style = NULL,
                        plot_theme = NULL,
                        return_object = FALSE,
                        plot_name = NULL){
  img_data = .check_plot_input(img_data)
  value_var = match.arg(value_var)
  scale_colors = .plot_scale_colors(img_data, scale_colors)
  df = .plot_select_channels(img_data, selected_channels)
  rect = .plot_rect(img_data, rect)
  p = .plot_channels(df, rect, value_var = value_var, scale_colors = scale_colors,
                     facets = facet_wrap(~channel_name, ncol = ncol, nrow = nrow))
  if(is.null(plot_name)) plot_name = if(value_var == "value") "raw" else "normalized"
  .plot_return(img_data, p, rect, .resolve_plot_options(img_data, um_axes, scale_bar, scale_bar_style, plot_theme),
               return_object, plot_name, scale_colors)
}

#' @rdname plotTiffData
#' @export
plotTiffDataMasked = function(img_data,
                              selected_channels = NULL,
                              value_var = c("norm_value", "value"),
                              scale_colors = NULL,
                              rect = NULL,
                              um_axes = NULL,
                              scale_bar = NULL,
                              scale_bar_style = NULL,
                              plot_theme = NULL,
                              return_object = FALSE,
                              plot_name = "masked"){
  img_data = .check_plot_input(img_data)
  value_var = match.arg(value_var)
  if(!"Text" %in% colnames(img_data@data)){
    stop("img_data has no mask segments (a Text column); fetch it with fetchTiffDataMasked().")
  }
  scale_colors = .plot_scale_colors(img_data, scale_colors)
  df = .plot_select_channels(img_data, selected_channels)
  rect = .plot_rect(img_data, rect)
  p = .plot_masked(df, rect, value_var = value_var, scale_colors = scale_colors)
  .plot_return(img_data, p, rect, .resolve_plot_options(img_data, um_axes, scale_bar, scale_bar_style, plot_theme),
               return_object, plot_name, scale_colors)
}

#' @rdname plotTiffData
#' @param red_channel,green_channel,blue_channel Channels (number or name) for
#'   the red, green and blue components.
#' @export
plotTiffData.rgb = function(img_data,
                            red_channel = 2,
                            green_channel = 3,
                            blue_channel = 1,
                            value_var = c("norm_value", "value"),
                            rect = NULL,
                            um_axes = NULL,
                            scale_bar = NULL,
                            scale_bar_style = NULL,
                            plot_theme = NULL,
                            return_object = FALSE,
                            plot_name = "rgb"){
  img_data = .check_plot_input(img_data)
  value_var = match.arg(value_var)
  rgb_channels = vapply(list(red_channel, green_channel, blue_channel), .resolve_channels, numeric(1),
                        channel_table = img_data@channels, arg = "red/green/blue_channel")
  .plot_require_channels(img_data, rgb_channels)
  rect = .plot_rect(img_data, rect)
  p = .plot_rgb(img_data@data, img_data@channels, rgb_channels, value_var = value_var, rect = rect)
  .plot_return(img_data, p, rect, .resolve_plot_options(img_data, um_axes, scale_bar, scale_bar_style, plot_theme),
               return_object, plot_name)
}

#' @rdname plotTiffData
#' @param channel_colors Channel-to-colour mapping, as in
#'   \code{fetchTiffData.colorized()}: an unnamed list by channel number (NULL
#'   or NA entries are skipped), or a list named by channel name.
#' @export
plotTiffData.colorized = function(img_data,
                                  channel_colors,
                                  value_var = c("norm_value", "value"),
                                  rect = NULL,
                                  um_axes = NULL,
                                  scale_bar = NULL,
                                  scale_bar_style = NULL,
                                  plot_theme = NULL,
                                  return_object = FALSE,
                                  plot_name = "colorized"){
  img_data = .check_plot_input(img_data)
  value_var = match.arg(value_var)
  if(!is.list(channel_colors)) stop("channel_colors must be a list.")
  channel_map = .resolve_channel_color_map(channel_colors = channel_colors, channel_table = img_data@channels)
  .plot_require_channels(img_data, channel_map$channel)
  rect = .plot_rect(img_data, rect)
  p = .plot_colorized(img_data@data, channel_map, value_var = value_var, rect = rect)
  .plot_return(img_data, p, rect, .resolve_plot_options(img_data, um_axes, scale_bar, scale_bar_style, plot_theme),
               return_object, plot_name)
}

.check_plot_input = function(img_data){
  if(!is(img_data, "TiffPlotData")) stop("img_data must be a TiffPlotData object.")
  img_data = .check_TiffPlotData_version(img_data)
  if(nrow(img_data@channels) == 0 || !all(c("channel", "channel_name") %in% colnames(img_data@data))){
    stop("img_data has no channel information to plot; make it with a fetchTiffData*() function.")
  }
  img_data
}

.plot_scale_colors = function(img_data, scale_colors){
  if(is.null(scale_colors)) scale_colors = img_data@plot_parameters$scale_colors
  if(is.null(scale_colors)) scale_colors = .default_scale_colors()
  .check_scale_colors(scale_colors)
}

.plot_rect = function(img_data, rect){
  if(is.null(rect)) return(img_data@rect)
  if(!is(rect, "TiffRect") || nrow(rect@coords) != 1) stop("rect must be a TiffRect with exactly one row.")
  rect
}

.plot_require_channels = function(img_data, channels){
  missing = setdiff(channels, img_data@data$channel)
  if(length(missing) > 0){
    nm = img_data@channels$channel_name[match(missing, img_data@channels$channel)]
    stop("Channels not in img_data: ", paste(nm, collapse = ", "),
         ". Fetched channels: ", paste(levels(droplevels(img_data@data$channel_name)), collapse = ", "))
  }
}

# @data limited to selected_channels, with channel_name levels in that order.
.plot_select_channels = function(img_data, selected_channels){
  df = img_data@data
  if(is.null(selected_channels)) return(df)
  ch = .resolve_channels(selected_channels, img_data@channels, arg = "selected_channels")
  .plot_require_channels(img_data, ch)
  df = df[df$channel %in% ch, , drop = FALSE]
  df$channel_name = factor(as.character(df$channel_name),
                           levels = img_data@channels$channel_name[match(ch, img_data@channels$channel)])
  df
}

.plot_return = function(img_data, p, rect, opts, return_object, plot_name, scale_colors = NULL){
  p = .apply_plot_options(p, img_data, rect, opts)
  if(!isTRUE(return_object)) return(p)
  if(!is.character(plot_name) || length(plot_name) != 1 || !nzchar(plot_name)){
    stop("plot_name must be a single non-empty string.")
  }
  img_data@plots[[plot_name]] = p
  img_data@activePlot = plot_name
  if(!is.null(scale_colors)) img_data@plot_parameters$scale_colors = scale_colors
  img_data = .record_plot_options(img_data, opts)
  validObject(img_data)
  img_data
}

# Plot options applied after the builders, in both the fetch and the replot
# paths: um_axes, scale_bar, scale_bar_style and plot_theme. Getters for their
# defaults are in R/options.R.

# Explicit argument, else the value recorded with the object, else the
# option. check_FUN validates whichever is used. A recorded NULL (no theme)
# counts as recorded.
.plot_option = function(img_data, x, name, default_FUN, check_FUN){
  if(is.null(x)){
    x = if(name %in% names(img_data@plot_parameters)) img_data@plot_parameters[[name]] else default_FUN()
  }
  check_FUN(x, name)
}

# Resolve the four plot options for a replot of img_data.
.resolve_plot_options = function(img_data, um_axes, scale_bar, scale_bar_style, plot_theme){
  list(um_axes = .plot_option(img_data, um_axes, "um_axes", .default_um_axes, .check_flag),
       scale_bar = .plot_option(img_data, scale_bar, "scale_bar", .default_scale_bar, .check_flag),
       scale_bar_style = .plot_option(img_data, scale_bar_style, "scale_bar_style",
                                      .default_scale_bar_style, .check_scale_bar_style),
       plot_theme = .plot_option(img_data, plot_theme, "plot_theme", .default_plot_theme, .check_plot_theme))
}

# Micron axes, micron scale bar and theme on p. Images without a physical
# pixel size stay in pixels, with a warning. The bar is placed from rect,
# which avoids building the plot to find its range.
.apply_plot_options = function(p, img_data, rect, opts){
  if(opts$um_axes || opts$scale_bar){
    upp = img_data@unit_per_pixel
    if(!is.numeric(upp) || length(upp) != 1 || !is.finite(upp) || upp <= 0){
      warning("The image records no physical pixel size, so ",
              paste(c("micron axes", "a micron scale bar")[c(opts$um_axes, opts$scale_bar)], collapse = " and "),
              " can't be drawn; the plot stays in pixels.", call. = FALSE)
    }else{
      unit_name = img_data@unit_name
      if(is.na(unit_name) || !nzchar(unit_name)) unit_name = "um"
      if(opts$um_axes) p = set_axes_um(p, unit_per_pixel = upp, unit_name = unit_name)
      if(opts$scale_bar){
        bar_args = c(list(p, unit_per_pixel = upp, unit_name = unit_name,
                          xlim = c(rect@coords$xmin, rect@coords$xmax),
                          ylim = c(rect@coords$ymin, rect@coords$ymax)),
                     opts$scale_bar_style)
        p = do.call(add_um_scale_bar, bar_args)
      }
    }
  }
  if(!is.null(opts$plot_theme)) p = p + opts$plot_theme
  p
}

.record_plot_options = function(img_data, opts){
  for(nm in names(opts)){
    # [nm] = list(NULL) keeps a NULL plot_theme as an entry instead of dropping it
    img_data@plot_parameters[nm] = list(opts[[nm]])
  }
  img_data
}

# Final step of the public fetch functions: apply the plot options to the
# object's plots and record them in @plot_parameters.
.finish_plot_options = function(img_data, um_axes, scale_bar, scale_bar_style, plot_theme){
  opts = list(um_axes = .check_flag(um_axes, "um_axes"),
              scale_bar = .check_flag(scale_bar, "scale_bar"),
              scale_bar_style = .check_scale_bar_style(scale_bar_style, "scale_bar_style"),
              plot_theme = .check_plot_theme(plot_theme, "plot_theme"))
  img_data@plots = lapply(img_data@plots, .apply_plot_options, img_data = img_data,
                          rect = img_data@rect, opts = opts)
  .record_plot_options(img_data, opts)
}
