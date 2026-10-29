#' Update a TiffPlotData object to the current TiffPlotR format
#'
#' Objects saved with \code{saveRDS()} keep the format of the TiffPlotR version
#' that made them. \code{update_TiffPlotData()} converts such an object to the
#' current format. Functions that take a \linkS4class{TiffPlotData} do this
#' automatically, with a warning, but the update is repeated on every call
#' until the updated object is saved.
#'
#' Objects from TiffPlotR 0.1.19 and earlier have no \code{@channels} or
#' \code{@package_version}, and their \code{@data$channel} holds channel names
#' whenever \code{channel_names} was given. The update:
#' \itemize{
#'   \item builds \code{@channels}, reading the OME names and colors from
#'     \code{@tiff_path} when the file is still readable;
#'   \item rewrites \code{channel} in \code{@data} and in each plot's data as
#'     the channel number, and adds the \code{channel_name} factor, keeping the
#'     original labels and plotting order;
#'   \item makes facets on \code{channel} facet on \code{channel_name} instead;
#'   \item sets \code{@package_version} to the current version.
#' }
#'
#' Channel names can only be turned back into numbers if they are the OME names
#' in the TIFF, or if \code{channel_names} gives the names that were used when
#' the object was fetched. Otherwise the update stops with an error.
#'
#' rgb and colorized plots from these versions don't record how they were
#' built, so \code{renormalize()} leaves them unchanged.
#'
#' Objects from 0.1.20 and earlier have no \code{@plot_parameters}. It is
#' filled with \code{scale_colors} set to the magma palette, which is what
#' their plots used. Their plots are not changed.
#'
#' @param img_data A \linkS4class{TiffPlotData} object (or subclass, such as
#'   \linkS4class{TiffPlotDataMasked}).
#' @param channel_names The \code{channel_names} given to the fetch that made
#'   \code{img_data}, naming every channel in order. Only needed when the
#'   object's channels are labelled with names that aren't the TIFF's OME names.
#'
#' @returns \code{img_data} in the current format. An object that is already
#'   current is returned unchanged.
#' @export
#'
#' @examples
#' \dontrun{
#' img <- readRDS("old_img.rds")
#' img <- update_TiffPlotData(img, channel_names = c("DAPI", "CD45", "PanCK"))
#' saveRDS(img, "old_img.rds")
#' }
update_TiffPlotData = function(img_data, channel_names = NULL){
  if(!is(img_data, "TiffPlotData")) stop("img_data must be a TiffPlotData object.")
  if(!.needs_update(img_data)) return(img_data)
  unversioned = !methods::.hasSlot(img_data, "package_version")
  had_plot_parameters = methods::.hasSlot(img_data, "plot_parameters")
  obj = .copy_into_current_class(img_data)
  if(unversioned){
    obj = .update_from_unversioned(obj, channel_names)
  }
  if(!had_plot_parameters){
    # what these plots were drawn with, whatever TPR_SCALE_COLORS is now
    obj@plot_parameters = list(scale_colors = .magma_scale_colors())
  }
  validObject(obj)
  obj
}

# Called at the top of every function that takes a TiffPlotData: returns the
# object in the current format, warning when it had to be updated.
.check_TiffPlotData_version = function(img_data){
  if(!.needs_update(img_data)) return(img_data)
  made_by = if(methods::.hasSlot(img_data, "package_version")) img_data@package_version else "0.1.19 or earlier"
  warning("This TiffPlotData object was made by TiffPlotR ", made_by, ". ",
          "It has been updated for this call; run update_TiffPlotData() and save the result ",
          "to avoid updating it every time.", call. = FALSE)
  update_TiffPlotData(img_data)
}

# So far every format change added a slot: @channels and @package_version
# (0.1.20), @plot_parameters (after 0.1.20). A change that keeps the slots should
# compare @package_version against the version that introduced it.
.needs_update = function(img_data){
  !all(vapply(methods::slotNames("TiffPlotData"), methods::.hasSlot, logical(1), object = img_data))
}

# Copy the slots the old object has into a new one; new() fills the rest from
# the prototype, and stamps the current version if there was none.
.copy_into_current_class = function(img_data){
  cls = class(img_data)
  old_slots = attributes(img_data)
  old_slots = old_slots[intersect(names(old_slots), methods::slotNames(cls))]
  do.call(methods::new, c(list(Class = cls), old_slots))
}

# 0.1.19 and earlier: no channel table, and data$channel may hold names.
.update_from_unversioned = function(obj, channel_names){
  df = obj@data
  if(!"channel" %in% colnames(df)) return(obj)
  obj@channels = .unversioned_channel_table(df$channel, obj@tiff_path, channel_names)
  obj@data = .unversioned_channel_columns(df, obj@channels)
  obj@plots = lapply(obj@plots, function(p){
    if(is.data.frame(p$data) && "channel" %in% colnames(p$data)){
      p$data = .unversioned_channel_columns(p$data, obj@channels)
    }
    .facet_channel_to_channel_name(p)
  })
  obj
}

# The channel table for an old object. Its channel column held numbers,
# number labels (a factor, when selected_channels was given) or display names.
.unversioned_channel_table = function(channel, tiff_path, channel_names){
  labels = unique(as.character(channel[!is.na(channel)]))
  is_number = .labels_are_numbers(channel)
  has_tiff = !is.na(tiff_path) && file.exists(tiff_path)
  if(!is.null(channel_names)){
    channel_table = if(has_tiff){
      .channel_table(tiff_path, channel_names)
    }else{
      data.frame(channel = seq_along(channel_names), channel_name = as.character(channel_names),
                 ome_name = NA_character_, color = NA_character_, stringsAsFactors = FALSE)
    }
    if(is_number){
      .resolve_channels(as.integer(labels), channel_table, arg = "channels in img_data")
    }else{
      .resolve_channels(labels, channel_table, arg = "channel labels in img_data")
    }
    return(channel_table)
  }
  if(is_number){
    if(has_tiff) return(.channel_table(tiff_path))
    return(.default_channel_table(data.frame(channel = as.integer(labels))))
  }
  if(has_tiff){
    channel_table = tryCatch(.channel_table(tiff_path, "ome"), error = function(e) NULL)
    if(!is.null(channel_table) && all(labels %in% channel_table$channel_name)){
      return(channel_table)
    }
  }
  stop("img_data labels its channels by name (", paste(labels, collapse = ", "), "), and ",
       "these can't be matched to channel numbers", if(has_tiff) " through the TIFF's OME names" else "",
       ". Supply the channel_names used when it was fetched: ",
       "update_TiffPlotData(img_data, channel_names = ...).", call. = FALSE)
}

# Old channel column -> channel number plus a channel_name factor, keeping the
# old plotting order (factor levels, else ascending channel number).
.unversioned_channel_columns = function(df, channel_table){
  old = df$channel
  labels = as.character(old)
  if(.labels_are_numbers(old)){
    num = as.integer(labels)
    order_num = if(is.factor(old)) as.integer(levels(old)) else sort(unique(num))
  }else{
    num = channel_table$channel[match(labels, channel_table$channel_name)]
    order_num = channel_table$channel[match(if(is.factor(old)) levels(old) else unique(labels),
                                            channel_table$channel_name)]
  }
  order_num = order_num[order_num %in% num]
  df$channel = num
  df$channel_name = factor(channel_table$channel_name[match(num, channel_table$channel)],
                           levels = channel_table$channel_name[match(order_num, channel_table$channel)])
  df
}

# Without channel_names, 0.1.19 kept channel numbers, as a factor of number
# labels when selected_channels was given.
.labels_are_numbers = function(channel){
  if(is.numeric(channel)) return(TRUE)
  labels = unique(as.character(channel[!is.na(channel)]))
  !anyNA(suppressWarnings(as.integer(labels)))
}

# Old plots facet on channel (facet_wrap(~channel), facet_grid(channel~Text)),
# which now holds numbers; point the facet at channel_name.
.facet_channel_to_channel_name = function(p){
  f = p$facet
  params = f$params
  hit = FALSE
  for(k in intersect(c("facets", "rows", "cols"), names(params))){
    q = params[[k]]
    i = which(names(q) == "channel")
    if(length(i) == 1){
      q[[i]] = vars(channel_name)[[1]]
      names(q)[i] = "channel_name"
      params[[k]] = q
      hit = TRUE
    }
  }
  if(!hit) return(p)
  # a new facet from its class, since ggproto objects are shared between plot copies
  facet_class = get(class(f)[1], envir = asNamespace("ggplot2"))
  p$facet = ggproto(NULL, facet_class, shrink = f$shrink, params = params)
  p
}
