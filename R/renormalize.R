#' Renormalize a TiffPlotData object with new channel limits
#'
#' Recomputes \code{norm_value} from the stored raw \code{value} using new
#' per-channel \code{min_value} / \code{max_value} limits, without re-reading
#' the TIFF. Only the channels named are changed; every other channel keeps its
#' current limits.
#'
#' There are two ways to give the new limits:
#' \itemize{
#'   \item \strong{Small}: \code{channels} plus at least one of
#'     \code{min_value} or \code{max_value}. Each limit is either a single value
#'     applied to every channel, or one value per channel. A limit left as
#'     \code{NULL} keeps its current value.
#'   \item \strong{Large}: a \code{precalc_max} data.frame with columns
#'     \code{channel}, \code{min_value} and \code{max_value}, in the same form
#'     \code{fetchTiffData()} accepts. It may cover a subset of channels. If it
#'     has per-resolution rows, the rows for this object's resolution are used.
#' }
#'
#' Channels can be given by number or by name. Names are looked up in
#' \code{@channels}: the \code{channel_names} used at fetch time first, then
#' the OME names. Objects made before TiffPlotR recorded \code{@channels} must
#' be fetched again.
#'
#' Plots are rebuilt from the new data. Layers added afterwards (for example by
#' \code{add_points()} or shape annotations) are kept.
#'
#' @param img_data A \linkS4class{TiffPlotData} object (or subclass, such as
#'   \linkS4class{TiffPlotDataMasked}).
#' @param channels Channels to update, as channel numbers or names.
#' @param min_value,max_value New limits for \code{channels}: length 1 or
#'   \code{length(channels)}.
#' @param precalc_max A data.frame of new limits; an alternative to
#'   \code{channels}/\code{min_value}/\code{max_value}.
#'
#' @returns A new object of the same class as \code{img_data}, with updated
#'   \code{@data}, \code{@precalc_max} and \code{@plots}.
#' @export
#'
#' @examples
#' \dontrun{
#' img <- fetchTiffData(exampleTiff(), TiffRect(900, 2300, 1400, 2800),
#'                      channel_names = c("DAPI", paste("channel", LETTERS[1:5])))
#' # small: raise the ceiling on two channels
#' img2 <- renormalize(img, channels = c("DAPI", "channel B"), max_value = 2000)
#' # large: apply a precalc_max table to a subset of channels
#' pm <- data.frame(channel = 1:2, min_value = c(100, 50), max_value = c(3000, 800))
#' img3 <- renormalize(img, precalc_max = pm)
#' }
renormalize = function(img_data,
                       channels = NULL,
                       min_value = NULL,
                       max_value = NULL,
                       precalc_max = NULL){
  if(!is(img_data, "TiffPlotData")) stop("img_data must be a TiffPlotData object.")
  if(!.hasSlot(img_data, "channels") || nrow(img_data@channels) == 0){
    stop("img_data does not record its channels; it was made by an older TiffPlotR. Fetch it again.")
  }
  channel_table = img_data@channels
  old_pm = img_data@precalc_max
  if(nrow(old_pm) == 0) stop("img_data has no precalc_max to update.")

  if(!is.null(precalc_max)){
    if(!is.null(channels) || !is.null(min_value) || !is.null(max_value)){
      stop("Supply either precalc_max, or channels with min_value/max_value, not both.")
    }
    upd = .handle_precalc_max(precalc_max, tidy_img = NULL, resolution = img_data@resolution,
                              channel_table = channel_table, quantile_norm = NULL)
    missing = setdiff(upd$channel, old_pm$channel)
    if(length(missing) > 0){
      stop("precalc_max has channels not present in img_data: ", paste(missing, collapse = ", "))
    }
  }else{
    upd = .resolve_channel_limits(channels, min_value, max_value, old_pm, channel_table)
  }

  bad = !is.finite(upd$min_value) | !is.finite(upd$max_value) | upd$max_value <= upd$min_value
  if(any(bad)){
    stop("max_value must be greater than min_value (and both finite) for channels: ",
         paste(upd$channel[bad], collapse = ", "))
  }

  new_pm = old_pm
  k = match(upd$channel, new_pm$channel)
  new_pm$min_value[k] = upd$min_value
  new_pm$max_value[k] = upd$max_value

  df = img_data@data
  changed = df$channel %in% upd$channel
  k = match(df$channel[changed], new_pm$channel)
  nv = (df$value[changed] - new_pm$min_value[k]) / (new_pm$max_value[k] - new_pm$min_value[k])
  df$norm_value[changed] = pmin(1, pmax(0, nv))

  img_data@data = df
  img_data@precalc_max = new_pm

  not_rebuilt = character(0)
  for(nm in names(img_data@plots)){
    p = .renormalize_plot(img_data@plots[[nm]], df)
    if(is.null(p)){
      not_rebuilt = c(not_rebuilt, nm)
    }else{
      img_data@plots[[nm]] = p
    }
  }
  if(length(not_rebuilt) > 0){
    warning("Could not rebuild plot(s), left unchanged: ", paste(not_rebuilt, collapse = ", "),
            ". Re-fetch to get plots that support renormalize().")
  }
  validObject(img_data)
  img_data
}

.recycle_limit = function(x, n, arg){
  if(!is.numeric(x) || !(length(x) %in% c(1, n))){
    stop(arg, " must be numeric with length 1 or length(channels).")
  }
  rep_len(x, n)
}

.resolve_channel_limits = function(channels, min_value, max_value, old_pm, channel_table){
  if(is.null(channels)) stop("channels is required unless precalc_max is supplied.")
  if(is.null(min_value) && is.null(max_value)){
    stop("Supply at least one of min_value or max_value.")
  }
  k = match(.resolve_channels(channels, channel_table), old_pm$channel)
  if(anyNA(k)) stop("Channels not found in precalc_max: ", paste(channels[is.na(k)], collapse = ", "))
  upd = old_pm[k, , drop = FALSE]
  if(!is.null(min_value)) upd$min_value = .recycle_limit(min_value, nrow(upd), "min_value")
  if(!is.null(max_value)) upd$max_value = .recycle_limit(max_value, nrow(upd), "max_value")
  upd
}

# Returns the rebuilt plot, or NULL if this plot can't be rebuilt.
.renormalize_plot = function(p, df){
  spec = p$meta$tiffplotr_composite
  if(!is.null(spec)){
    if(spec$type == "rgb"){
      rgb_df = convertTidyToRGB(df,
                                red_channel = spec$channels[1],
                                green_channel = spec$channels[2],
                                blue_channel = spec$channels[3],
                                value_var = spec$value_var)
    }else{
      rgb_df = .convert_tidy_to_colorized(df, spec$channel_map, value_var = spec$value_var)
    }
    # a fresh layer, since layers are environments shared with the original plot
    k = which(vapply(p$layers, function(l) is.data.frame(l$data) && "chex" %in% colnames(l$data), logical(1)))
    p$layers[[k[1]]] = geom_raster(data = rgb_df, aes(x = i, y = j, fill = chex))
    return(p)
  }
  if(is.data.frame(p$data) && all(c("channel", "value", "norm_value") %in% colnames(p$data))){
    p$data = df
    return(p)
  }
  NULL
}
