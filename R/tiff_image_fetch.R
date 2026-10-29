.rect_null_check = function(rect, tiff_path){
  if(is.null(rect)){
    img_info = read_tiff_meta_data(tiff_path)
    max_info = subset(img_info, resolutionLevel == 1)
    rect = TiffRect(1, max_info$sizeX, 1, max_info$sizeY)
  }
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect object")
  if(!length(rect) == 1) stop("fetchTiffData requires a TiffRect with exactly one row")
  rect
}

.resolution_null_check = function(resolution, tiff_path, rect, max_pixels){
  if(is.null(resolution)){
    img_info = read_tiff_meta_data(tiff_path)
    max_info = subset(img_info, resolutionLevel == 1)
    #select the highest resolution under max_pixels
    x_pixels = rect$width* img_info$sizeX / max_info$sizeX
    y_pixels = rect$height* img_info$sizeY / max_info$sizeY
    x_under = x_pixels <= max_pixels
    y_under = y_pixels <= max_pixels
    if(!any(x_under) | !any(y_under)){
      resolution = max(img_info$resolutionLevel)
      message("selected lowest available resolution ", resolution, " though it exceeds max ", max_pixels, " pixels")
    }else{
      k = max(
        min(which(x_under)),
        min(which(y_under))
      )
      resolution = img_info$resolutionLevel[k]
      message("selected resolution ", resolution, " to keep final max dimension under ", max_pixels, " pixels")
    }
  }
  resolution
}

# precalc_max keyed by channel number. Channels given by name are resolved
# through channel_table (see .channel_table()).
.handle_precalc_max = function(precalc_max, tidy_img, resolution, channel_table, quantile_norm){
  if(is.null(precalc_max)){
    precalc_max = tidy_img  %>% group_by(channel) %>% summarise(min_value = 0, max_value = quantile(value, quantile_norm))
  }else{
    stopifnot(c("channel", "min_value", "max_value") %in% colnames(precalc_max))
    precalc_max = as.data.frame(dplyr::ungroup(precalc_max))
    if(!is.numeric(precalc_max$channel)){
      precalc_max$channel = .resolve_channels(precalc_max$channel, channel_table,
                                              arg = "precalc_max channels", allow_duplicates = TRUE)
    }
    if(any(precalc_max %>% dplyr::pull(channel) %>% duplicated)){
      #now resolution must be present and fetch resolution must be present
      stopifnot("resolution" %in% colnames(precalc_max))
      stopifnot(resolution %in% precalc_max$resolution)
      precalc_max = precalc_max[precalc_max$resolution == resolution,]
      #error if channel is still defined multiple times
      if(any(precalc_max %>% dplyr::pull(channel) %>% duplicated)){
        stop("Multiple values for each channel after resolution selection. Is this precalc_max for a single tiff file?")
      }
    }
    precalc_max = precalc_max %>% select(channel, min_value, max_value)
  }
  as.data.frame(precalc_max)
}

#' Fetch TIFF image data for a rectangular region
#'
#' Reads a rectangular subset of a multi-resolution TIFF and returns a
#' normalized, tidy representation with a faceted ggplot wrapped in a
#' \linkS4class{TiffPlotData} object.
#'
#' @param tiff_path Path to the TIFF image file.
#' @param rect A \linkS4class{TiffRect} object defining the requested region.
#'   If NULL, the full image extent at maximum resolution is used.
#' @param resolution Resolution level to read. If NULL, a resolution is selected
#'   automatically to keep dimensions under \code{max_pixels}.
#' @param max_pixels Maximum pixel dimension used when \code{resolution = NULL}.
#' @param precalc_max Optional data frame with \code{channel},
#'   \code{min_value}, and \code{max_value} for normalization. \code{channel}
#'   may hold channel numbers or names.
#' @param show_raw If TRUE, plot raw values. If FALSE (default), plot
#'   normalized values.
#' @param quantile_norm Quantile used to estimate per-channel max values when
#'   \code{precalc_max} is NULL.
#' @param channel_names Names for the channels: NULL (the default) labels them
#'   by number, \code{"ome"} uses the names in the TIFF's OME-XML (see
#'   \code{read_tiff_channels()}), and a character vector names every channel
#'   in order. Names must be unique.
#' @param selected_channels Optional subset of channels to keep, as channel
#'   numbers or names. Also sets the plotting order. Names match
#'   \code{channel_names} first, then the OME names.
#' @param scale_colors Colours of the fill gradient, low to high, passed to
#'   \code{scale_fill_gradientn()}. Defaults to the magma palette used by
#'   earlier versions. Stored in \code{@plot_parameters$scale_colors}.
#'
#' @returns A \linkS4class{TiffPlotData} object. \code{@data} has one row per
#'   pixel and channel, with \code{channel} (always the channel number) and
#'   \code{channel_name} (a factor in plotting order), plus \code{value} and
#'   \code{norm_value}. \code{@channels} lists every channel in the image with
#'   its name, OME name and OME color. The faceted plot is in \code{@plots}.
#' @export
#'
#' @examples
#' tiff_path <- exampleTiff()
#' view_rect <- TiffRect(900, 2300, 1400, 2800)
#' img <- fetchTiffData(tiff_path, view_rect)
#'
#' fetchTiffData(
#'   tiff_path,
#'   view_rect,
#'   channel_names = c("DAPI", paste("channel", LETTERS[1:5])),
#'   selected_channels = c("channel B", "channel E", "DAPI")
#' )
#'
#' fetchTiffData(tiff_path, view_rect, channel_names = "ome",
#'               selected_channels = c("DAPI", "Opal 570"))
fetchTiffData = function(tiff_path, rect = NULL, resolution = NULL, max_pixels = 800, precalc_max = NULL, show_raw = FALSE, quantile_norm = .999, channel_names = NULL, selected_channels = NULL, scale_colors = .default_scale_colors()){
  rect = .rect_null_check(rect, tiff_path)
  .fetch_tiff_data(tiff_path,
                   rect,
                   resolution = resolution,
                   max_pixels = max_pixels,
                   precalc_max = precalc_max,
                   show_raw = show_raw,
                   quantile_norm = quantile_norm,
                   channel_names = channel_names,
                   selected_channels = selected_channels,
                   scale_colors = scale_colors
  )
}

.fetch_ome_pixel_scale <- function(tiff_path) {
  defaults <- list(unit_per_pixel = NA_real_, unit_name = NA_character_)
  
  tryCatch({
    ome_doc <- .read_tiff_ome_xml(tiff_path)
    pixel_info <- .fetch_ome_nodes(ome_doc, "Pixels", output = "data.frame")
    
    if (nrow(pixel_info) < 1) {
      return(defaults)
    }
    
    px <- suppressWarnings(as.numeric(pixel_info$PhysicalSizeX[[1]]))
    py <- suppressWarnings(as.numeric(pixel_info$PhysicalSizeY[[1]]))
    unit <- pixel_info$PhysicalSizeXUnit[[1]]
    
    if (!is.finite(px)) {
      return(defaults)
    }
    
    if (is.finite(py) && !isTRUE(all.equal(px, py))) {
      warning("PhysicalSizeX and PhysicalSizeY differ; using PhysicalSizeX for unit_per_pixel.")
    }
    
    list(
      unit_per_pixel = px,
      unit_name = ifelse(is.null(unit) || !nzchar(as.character(unit)), NA_character_, as.character(unit))
    )
  }, error = function(e) {
    defaults
  })
}

.to_rgba_hex <- function(val) {
  val = as.numeric(val)
  if (val < 0) {
    val <- val + 2^32
  }
  
  a <- floor(val / 256^3) %% 256
  r <- floor(val / 256^2) %% 256
  g <- floor(val / 256) %% 256
  b <- val %% 256
  
  paste0("#", sprintf("%02X%02X%02X%02X", r, g, b, a))
  
}
to_rgba_hex = function(int_cols){
  sapply(int_cols, .to_rgba_hex)
}


.ome_fill_color_to_hex <- function(fill_color) {
  hex_vals = to_rgba_hex(as.numeric(fill_color))
  paste0("#", hex_vals)
}


.build_mask_color_mappings <- function(mask_points) {
  if (nrow(mask_points) == 0) {
    return(data.frame())
  }
  
  keep_cols <- intersect(c("ROI_ID", "Text", "FillColor"), colnames(mask_points))
  if (length(keep_cols) == 0) {
    return(data.frame())
  }
  
  out <- unique(mask_points[, keep_cols, drop = FALSE])
  if ("FillColor" %in% colnames(out)) {
    out$mask_color <- .ome_fill_color_to_hex(out$FillColor)
  }
  out
}


#' Fetch TIFF signal from mask-covered pixels across all channels
#'
#' Reads TIFF data similarly to \code{fetchTiffData()}, then filters the signal
#' to pixels that overlap decoded OME \code{Mask} annotations. Mask metadata,
#' including \code{ROI_ID}, \code{Text}, and \code{FillColor}, are preserved.
#'
#' @param tiff_path Path to the TIFF image file.
#' @param rect A \linkS4class{TiffRect} object defining the requested region.
#'   If NULL, the full image extent at maximum resolution is used.
#' @param resolution Resolution level to read. If NULL, a resolution is selected
#'   automatically to keep dimensions under \code{max_pixels}.
#' @param max_pixels Maximum pixel dimension used when \code{resolution = NULL}.
#' @param precalc_max Optional data frame with \code{channel}, \code{min_value},
#'   and \code{max_value} for normalization.
#' @param show_raw If TRUE, plot raw values. If FALSE (default), plot normalized values.
#' @param quantile_norm Quantile used to estimate per-channel max values when
#'   \code{precalc_max} is NULL.
#' @param channel_names Channel names, as in \code{fetchTiffData()}: NULL,
#'   \code{"ome"}, or a character vector naming every channel.
#' @param selected_channels Optional subset of channels to keep, as channel numbers
#'   or names.
#' @param bit_order Bit order to use when decoding OME mask payloads.
#' @param scale_colors Colours of the fill gradient, low to high, passed to
#'   \code{scale_fill_gradientn()}. Defaults to the magma palette used by
#'   earlier versions. Stored in \code{@plot_parameters$scale_colors}.
#'
#' @returns A \linkS4class{TiffPlotDataMasked} object with:
#' \itemize{
#'   \item \code{@data}: mask-filtered signal values for all selected channels
#'   \item \code{@mask_points}: decoded mask points with metadata
#'   \item \code{@mask_color_mappings}: per-mask color mapping (defaulting to XML FillColor)
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' masked <- fetchTiffDataMasked(exampleTiff())
#' head(masked@data)
#' head(masked@mask_color_mappings)
#' }
fetchTiffDataMasked <- function(tiff_path,
                                rect = NULL,
                                resolution = NULL,
                                max_pixels = 800,
                                precalc_max = NULL,
                                show_raw = FALSE,
                                quantile_norm = .999,
                                channel_names = NULL,
                                selected_channels = NULL,
                                mask_points = NULL,
                                retain_unmasked = FALSE,
                                segment_var = "Text",
                                bit_order = c("msb", "lsb"),
                                scale_colors = .default_scale_colors()) {
  bit_order <- match.arg(bit_order)
  rect <- .rect_null_check(rect, tiff_path)
  
  base_obj <- .fetch_tiff_data(
    tiff_path = tiff_path,
    rect = rect,
    resolution = resolution,
    max_pixels = max_pixels,
    precalc_max = precalc_max,
    show_raw = show_raw,
    quantile_norm = quantile_norm,
    channel_names = channel_names,
    selected_channels = selected_channels,
    scale_colors = scale_colors
  )
  
  if(is.null(mask_points)){
    ann <- fetchTiffAnnotations(
      tiff_path = tiff_path,
      decode_masks = TRUE,
      bit_order = bit_order,
      include_summary = FALSE
    )
    mask_points <- ann$mask_points
  }else{
    message("Using supplied mask_points.")
  }
  
  if (nrow(mask_points) == 0) {
    stop("No mask points found in OME annotations for this TIFF.")
  }
  
  required_mask_cols <- c("i", "j")
  if (!all(required_mask_cols %in% colnames(mask_points))) {
    stop("Decoded mask points must contain i and j columns.")
  }
  
  in_rect <- (mask_points$i >= rect@coords$xmin) & (mask_points$i <= rect@coords$xmax) &
    (mask_points$j >= rect@coords$ymin) & (mask_points$j <= rect@coords$ymax)
  mask_points <- mask_points[in_rect, , drop = FALSE]
  if (nrow(mask_points) == 0) {
    warning("No decoded mask points overlap the requested rect.")
  }
  
  signal_df <- base_obj@data
  
  # for lower resolutions, many mask points can match a single pixel
  # need to convert mask points to pixel resolution
  delta_i = signal_df$i %>% unique %>% sort %>% diff %>% table %>% sort %>% rev %>% head(n=1) %>% names %>% as.numeric
  delta_j = signal_df$j %>% unique %>% sort %>% diff %>% table %>% sort %>% rev %>% head(n=1) %>% names %>% as.numeric
  
  remain_i = signal_df$i %% delta_i %>% table %>% sort %>% rev %>% head(n = 1) %>% names %>% as.numeric
  remain_j = signal_df$j %% delta_j %>% table %>% sort %>% rev %>% head(n = 1) %>% names %>% as.numeric
  
  # join will be done on i_bin and j_bin
  signal_df = signal_df %>%
    dplyr::mutate(i_bin = round((i-remain_i) / delta_i)) %>%
    dplyr::mutate(j_bin = round((j-remain_j) / delta_j))
  
  # join will be done on i_bin and j_bin
  mask_points = mask_points %>%
    dplyr::mutate(i_bin = round((i-remain_i) / delta_i)) %>%
    dplyr::mutate(j_bin = round((j-remain_j) / delta_j))
  
  # select most common mask assignment per ROI_ID in pixel
  mask_pixels = mask_points %>%
    dplyr::group_by(Text, ROI_ID, i_bin, j_bin) %>%
    dplyr::summarise(N = length(Text)) %>%
    dplyr::group_by(ROI_ID, i_bin, j_bin) %>%
    dplyr::mutate(fraction = N / sum(N)) %>%
    dplyr::filter(fraction > .5)
  
  mask_pixels = mask_pixels %>%
    dplyr::mutate(i = i_bin*delta_i+remain_i, j = j_bin*delta_j+remain_j)
  
  mask_merge = mask_pixels %>%
    dplyr::ungroup() %>%
    dplyr::select(Text, ROI_ID, i, j)
  
  masked_df = merge(signal_df, mask_merge, by = c("i", "j"), all.x = retain_unmasked)
  
  if (nrow(masked_df) == 0) {
    stop(
      paste(
        "No overlap was found between sampled TIFF pixels and decoded mask points at this resolution.",
        "Try a higher resolution (smaller resolutionLevel value) or a broader rect."
      )
    )
  }
  
  fill_var <- if (isTRUE(show_raw)) "value" else "norm_value"
  p <- ggplot(masked_df, aes(x = i, y = j, fill = !!ensym(fill_var))) +
    facet_grid(channel_name~Text) +
    scale_y_reverse() +
    .fill_gradient_layers(base_obj@plot_parameters$scale_colors) +
    # theme(panel.background = element_rect(fill = "gray100"), panel.grid = element_blank()) +
    labs(x = "pixels") +
    labs(y = "pixels")
  
  p <- .apply_coord_rect(p, rect, ggplot2::coord_fixed)
  plots_list <- list(masked = p)
  
  mask_color_mappings <- .build_mask_color_mappings(mask_points)
  
  out_obj <- TiffPlotDataMasked(
    data = masked_df,
    plots = plots_list,
    activePlot = names(plots_list)[1],
    tiff_path = tiff_path,
    resolution = base_obj@resolution,
    precalc_max = base_obj@precalc_max,
    rect = rect,
    img_info = base_obj@img_info,
    plot_parameters = base_obj@plot_parameters,
    mask_points = mask_points,
    mask_color_mappings = mask_color_mappings
  )
  
  out_obj@unit_per_pixel <- base_obj@unit_per_pixel
  out_obj@unit_name <- base_obj@unit_name
  out_obj@channels <- base_obj@channels
  out_obj
}


#' Convert a TIFF image array to a tidy data frame
#'
#' Melts a multi-channel image array into a long-format data frame with columns
#' for pixel coordinates, channel, and pixel value.
#'
#' @param img_mat A multi-dimensional image array (e.g. from RBioFormats::read.image)
#'
#' @returns A data frame with columns `i` (x coordinate), `j` (y coordinate),
#'   `channel` (channel index), and `value` (pixel intensity)
#' @export
#'
#' @examples
#' \dontrun{
#'   img_mat <- RBioFormats::read.image("image.tiff", normalize = FALSE)
#'   tidy <- makeImageTidy(img_mat)
#'   head(tidy)
#' }
makeImageTidy = function(img_mat){
  tidy_img <- as.data.frame.table(img_mat, responseName = "value") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(x = as.integer(x), y = as.integer(y), c = as.integer(c)) %>%
    dplyr::rename(i = x, j = y, channel = c)
  
  tidy_img
}

#' Convert a tidy image data frame to RGB
#'
#' Combines three channels from a tidy image data frame into a single RGB data
#' frame with a hex color column, suitable for display with ggplot2.
#'
#' @param img_df A tidy data frame with columns `i`, `j`, `channel`, and a
#'   value column (see `value_var`)
#' @param red_channel Channel number to map to red (default 1)
#' @param green_channel Channel number to map to green (default 2)
#' @param blue_channel Channel number to map to blue (default 3)
#' @param value_var Name of the column to use for RGB values (default "norm_value")
#'
#' @returns A data frame with columns `i`, `j`, `red`, `green`, `blue`, and
#'   `chex` (hex color string)
#' @export
#'
#' @examples
#' \dontrun{
#'   tiff_path <- exampleTiff()
#'   img_obj <- fetchTiffData(tiff_path, TiffRect(900, 2300, 1400, 2800))
#'   rgb_df <- convertTidyToRGB(img_obj@data, red_channel = 6, green_channel = 1, blue_channel = 5)
#' }
convertTidyToRGB = function(img_df, red_channel = 1, green_channel = 2, blue_channel = 3, value_var = "norm_value"){
  rv = red_channel
  gv = green_channel
  bv = blue_channel
  stopifnot(is.character(value_var))
  stopifnot(length(value_var) == 1)
  stopifnot(value_var %in% colnames(img_df))
  
  rgb_df = subset(img_df, channel %in% c(rv, gv, bv)) %>%
    tidyr::pivot_wider(id_cols = c("i", "j"), names_from = "channel", values_from = tidyr::all_of(value_var))
  rgb_df = rgb_df %>% mutate(i, j, red = !!sym(as.character(rv)), green = !!sym(as.character(gv)), blue = !!sym(as.character(bv)), .keep = "none")
  
  #strip NA values
  rgb_df = rgb_df %>%
    mutate(red = ifelse(is.na(red), 0, red)) %>%
    mutate(green = ifelse(is.na(green), 0, green)) %>%
    mutate(blue = ifelse(is.na(blue), 0, blue))
  # an all-zero channel stays black instead of giving 0/0
  .scale_to_max = function(v){
    v_max = max(v, na.rm = TRUE)
    if(is.finite(v_max) && v_max > 0) v / v_max else v * 0
  }
  rgb_df = rgb_df %>% mutate(chex = rgb(.scale_to_max(red), .scale_to_max(green), .scale_to_max(blue)))
  rgb_df
}

.resolve_channel_color_map = function(channel_colors, channel_table){
  if(is.null(channel_colors)){
    stop("channel_colors cannot be NULL.")
  }
  
  color_values = if(is.list(channel_colors)) channel_colors else as.list(channel_colors)
  raw_names = names(channel_colors)
  has_nonempty_name = !is.null(raw_names) && any(nzchar(raw_names))
  
  if(has_nonempty_name){
    if(any(!nzchar(raw_names))){
      stop("Named channel_colors must have a non-empty name for every entry.")
    }
    channel_idx = .resolve_channels(raw_names, channel_table, arg = "channel_colors names",
                                    allow_duplicates = TRUE)
  }else{
    channel_idx = seq_along(color_values)
  }
  
  out_channels = integer(0)
  out_colors = character(0)
  for(i in seq_along(color_values)){
    clr = color_values[[i]]
    if(is.null(clr) || (length(clr) == 1 && is.na(clr))){
      next
    }
    if(!(is.character(clr) && length(clr) == 1)){
      stop("Each channel_colors entry must be a single character color string, NULL, or NA.")
    }
    
    ch = channel_idx[i]
    if(!(ch %in% channel_table$channel)){
      stop("Mapped channel index ", ch, " is not available in this image. Available channels: ",
           paste(channel_table$channel, collapse = ", "))
    }
    
    out_channels = c(out_channels, ch)
    out_colors = c(out_colors, clr)
  }
  
  if(length(out_channels) == 0){
    stop("No valid channel mappings found in channel_colors after removing NULL/NA entries.")
  }
  if(anyDuplicated(out_channels)){
    dup = unique(out_channels[duplicated(out_channels)])
    stop("Each channel can only be assigned one color. Duplicate channels: ",
         paste(dup, collapse = ", "))
  }
  
  # Validate all colors up front to provide clear errors.
  tryCatch(
    grDevices::col2rgb(out_colors),
    error = function(e) {
      stop("Invalid color in channel_colors: ", e$message)
    }
  )
  
  data.frame(
    channel = as.integer(out_channels),
    channel_name = channel_table$channel_name[match(out_channels, channel_table$channel)],
    color = as.character(out_colors),
    stringsAsFactors = FALSE
  )
}

.convert_tidy_to_colorized = function(img_df, channel_map, value_var = "norm_value"){
  stopifnot(is.character(value_var))
  stopifnot(length(value_var) == 1)
  stopifnot(value_var %in% colnames(img_df))
  
  sel_channels = channel_map$channel
  wide_df = subset(img_df, channel %in% sel_channels) %>%
    tidyr::pivot_wider(id_cols = c("i", "j"), names_from = "channel", values_from = dplyr::all_of(value_var))
  
  n = nrow(wide_df)
  out_red = rep(0, n)
  out_green = rep(0, n)
  out_blue = rep(0, n)
  
  for(i in seq_len(nrow(channel_map))){
    ch = as.character(channel_map$channel[i])
    clr = channel_map$color[i]
    
    vals = wide_df[[ch]]
    if(is.null(vals)){
      vals = rep(0, n)
    }
    vals[is.na(vals)] = 0
    
    ch_max = max(vals, na.rm = TRUE)
    if(is.finite(ch_max) && ch_max > 0){
      vals = vals / ch_max
    }else{
      vals = rep(0, n)
    }
    
    clr_rgb = as.numeric(grDevices::col2rgb(clr)) / 255
    out_red = out_red + vals * clr_rgb[1]
    out_green = out_green + vals * clr_rgb[2]
    out_blue = out_blue + vals * clr_rgb[3]
  }
  
  out_red = pmin(1, pmax(0, out_red))
  out_green = pmin(1, pmax(0, out_green))
  out_blue = pmin(1, pmax(0, out_blue))
  
  data.frame(
    i = wide_df$i,
    j = wide_df$j,
    red = out_red,
    green = out_green,
    blue = out_blue,
    chex = grDevices::rgb(out_red, out_green, out_blue)
  )
}


#' Read a TIFF region as an array
#'
#' Convenience wrapper around \code{RBioFormats::read.image()} that applies
#' \linkS4class{TiffRect}-based coordinate subsetting and resolution selection.
#'
#' @param tiff_path Path to the TIFF image file.
#' @param rect A \linkS4class{TiffRect} object defining the requested region.
#'   If NULL, the full image extent at maximum resolution is used.
#' @param resolution Resolution level to read. If NULL, a resolution is selected
#'   automatically to keep dimensions under \code{max_pixels}.
#' @param max_pixels Maximum pixel dimension used when \code{resolution = NULL}.
#'
#' @returns An image array returned by \code{RBioFormats::read.image()} for the
#'   selected region and resolution.
#' @export
#'
#' @examples
#' \dontrun{
#' arr <- fetchTiffArray(exampleTiff(), rect = TiffRect(900, 2300, 1400, 2800))
#' dim(arr)
#' }
fetchTiffArray = function(tiff_path, rect = NULL,
                          resolution = NULL, max_pixels = 800){
  rect = .rect_null_check(rect, tiff_path)
  img_info = read_tiff_meta_data(tiff_path)
  max_info = dplyr::filter(img_info, resolutionLevel == 1)
  
  resolution = .resolution_null_check(resolution = resolution, tiff_path = tiff_path, rect = rect, max_pixels = max_pixels)
  res_info = dplyr::filter(img_info, resolutionLevel == resolution)
  
  x_ratio = res_info$sizeX / max_info$sizeX
  y_ratio = res_info$sizeY / max_info$sizeY
  RBioFormats::read.image(
    tiff_path,
    normalize = FALSE,
    series = 1,
    resolution = resolution,
    subset = list(
      X = seq(rect$xmin*x_ratio, (rect$xmin + rect$width)*x_ratio),
      Y = seq(rect$ymin*y_ratio, (rect$ymin + rect$height)* y_ratio)
    ))
}

#' Plot a TIFF image with custom dimensions
#'
#' Reads a region from a TIFF image file and creates a ggplot visualization.
#' Automatically handles multi-resolution TIFF files and selects appropriate
#' resolution based on desired pixel dimensions.
#'
#' @param tiff_path Path to the TIFF image file
#' @param rect A \linkS4class{TiffRect} object defining the rectangle region
#' @param resolution Resolution level to read from the TIFF file. If NULL, automatically selects resolution to keep image under max_pixels
#' @param max_pixels Maximum dimension in pixels for the plotted image. Used for automatic resolution selection
#' @param precalc_max Optional data frame with precalculated min/max values per channel for normalization
#' @param show_raw If TRUE, displays raw pixel values; if FALSE (default), displays normalized values
#' @param quantile_norm Quantile for normalization (default 0.999). Values are divided by this quantile
#' @param scale_colors Colours of the fill gradient, low to high (see \code{fetchTiffData()}).
#'
#' @returns A TiffPlotData object containing the sparse image data and a named list of ggplot objects. It also records `tiff_path`, `resolution`, `precalc_max` and `rect` slots.
#' @importFrom dplyr group_by mutate select summarise
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom RBioFormats read.image
#'
.fetch_tiff_data = function(tiff_path, rect,
                            resolution = NULL,
                            max_pixels = 800,
                            precalc_max = NULL,
                            show_raw = FALSE,
                            quantile_norm = .999,
                            channel_names = NULL,
                            selected_channels = NULL,
                            scale_colors = .default_scale_colors()){
  scale_colors = .check_scale_colors(scale_colors)
  # resolve names before the read, so bad names fail fast
  channel_table = .channel_table(tiff_path, channel_names)
  selected_channels = .resolve_channels(selected_channels, channel_table, arg = "selected_channels")
  img_data = fetchTiffArray(
    tiff_path = tiff_path,
    rect = rect,
    resolution = resolution,
    max_pixels = max_pixels)
  
  # 1. Assume 'img_data' is a 2D matrix (or array converted to matrix)
  # 2. Find indices of non-zero pixels
  # non_zero_coords <- which(img_data != 0, arr.ind = TRUE)
  # if(nrow(non_zero_coords) == 0){
  # stop("No data found in area.")
  # }
  if(max(img_data) == 0){
    stop("No data found in area.")
  }
  
  # report plotted image stats
  # x_pix = diff(range(non_zero_coords[, "x"])) %>% as.numeric
  # y_pix = diff(range(non_zero_coords[, "y"])) %>% as.numeric
  x_pix = ncol(img_data)
  y_pix = nrow(img_data)
  pix_area = x_pix*y_pix
  if(pix_area >= 1e5){
    pix_area = paste0(round(pix_area / 1e6, 2), "M")
  }else if(pix_area >= 1e3){
    pix_area = paste0(round(pix_area / 1e3, 2), "k")
  }
  message("plotted image is ", x_pix, "x", y_pix, " (", pix_area,  " pixels)")
  
  # 3. Create sparse matrix
  resolution = .resolution_null_check(resolution = resolution, tiff_path = tiff_path, rect = rect, max_pixels = max_pixels)
  
  img_info = read_tiff_meta_data(tiff_path)
  max_info = subset(img_info, resolutionLevel == 1)
  res_info = subset(img_info, resolutionLevel == resolution)
  
  tidy_img = makeImageTidy(img_data@.Data)
  x_ratio = res_info$sizeX / max_info$sizeX
  y_ratio = res_info$sizeY / max_info$sizeY
  tidy_img$i = (tidy_img$i + rect$xmin*x_ratio)/x_ratio
  tidy_img$j = (tidy_img$j + rect$ymin*y_ratio)/y_ratio
  
  precalc_max = .handle_precalc_max(precalc_max, tidy_img, resolution, channel_table, quantile_norm)

  tidy_img = merge(tidy_img, precalc_max %>% select(channel, min_value, max_value), all.x = TRUE)
  tidy_img = tidy_img %>% mutate(norm_value = (value - min_value) / (max_value - min_value))
  tidy_img = tidy_img %>% select(!c(min_value, max_value))
  
  #cap norm_value to 1
  tidy_img = tidy_img %>% mutate(norm_value = ifelse(norm_value > 1, 1, norm_value))
  tidy_img = tidy_img %>% mutate(norm_value = ifelse(norm_value < 0, 0, norm_value))
  
  
  # report fullresolution image stats
  x_pix = diff(range(tidy_img$i)) %>% as.numeric
  y_pix = diff(range(tidy_img$j)) %>% as.numeric
  pix_area = x_pix*y_pix
  if(pix_area >= 1e5){
    pix_area = paste0(round(pix_area / 1e6, 2), "M")
  }else if(pix_area >= 1e3){
    pix_area = paste0(round(pix_area / 1e3, 2), "k")
  }
  message("full resolution image would have been ", round(x_pix), "x", round(y_pix), " (", pix_area,  " pixels)")
  
  # channel stays the channel number; channel_name is the label, in plotting order
  if(!is.null(selected_channels)){
    tidy_img = tidy_img %>% dplyr::filter(channel %in% selected_channels)
  }
  shown = if(is.null(selected_channels)) channel_table$channel else selected_channels
  tidy_img$channel_name = factor(channel_table$channel_name[match(tidy_img$channel, channel_table$channel)],
                                 levels = channel_table$channel_name[match(shown, channel_table$channel)])
  
  # return_data parameter removed; functions now return a TiffPlotData object
  if(show_raw){
    p = ggplot(tidy_img, aes(x = i, y = j, fill = value))
  }else{
    p = ggplot(tidy_img, aes(x = i, y = j, fill = norm_value))
  }
  
  p = p +
    facet_wrap(~channel_name) +
    scale_y_reverse() +
    .fill_gradient_layers(scale_colors) #+
  # theme(panel.background = element_rect(fill = "gray20"), panel.grid = element_blank())
  
  p = .apply_coord_rect(p, rect, ggplot2::coord_fixed)
  
  plots_list = list()
  plots_list[[ifelse(show_raw, "raw", "normalized")]] = p
  data_df <- as.data.frame(tidy_img)
  pixel_scale <- .fetch_ome_pixel_scale(tiff_path)
  # record rectangle corresponding to requested region
  new("TiffPlotData",
      data = data_df,
      plots = plots_list,
      activePlot = names(plots_list)[1],
      tiff_path = tiff_path,
      resolution = resolution,
      precalc_max = if(is.null(precalc_max)) data.frame() else precalc_max,
      rect = rect,
      img_info = read_tiff_meta_data(tiff_path),
      unit_per_pixel = pixel_scale$unit_per_pixel,
      unit_name = pixel_scale$unit_name,
      channels = channel_table,
      plot_parameters = list(scale_colors = scale_colors))
}

#' Plot a rectangular region of a TIFF image as RGB
#'
#' Creates an RGB composite visualization from a rectangular region of a TIFF image.
#' Combines three specified channels into red, green, and blue components.
#'
#' @param tiff_path Path to the TIFF image file
#' @param rect A \linkS4class{TiffRect} object defining the rectangle region
#' @param resolution Resolution level to read from the TIFF file. If NULL, automatically selects resolution
#' @param max_pixels Maximum dimension in pixels for the plotted image
#' @param precalc_max Optional data frame with precalculated min/max values per channel for normalization
#' @param red_channel Channel (number or name) for the red component (default 2)
#' @param green_channel Channel (number or name) for the green component (default 3)
#' @param blue_channel Channel (number or name) for the blue component (default 1)
#' @param channel_names Channel names, as in \code{fetchTiffData()}: NULL,
#'   \code{"ome"}, or a character vector naming every channel.
#' @param value_var Variable to use for RGB values, either "norm_value" or "value" (default "norm_value")
#' @param quantile_norm Quantile for normalization (default 0.999)
#'
#' @returns A TiffPlotData object containing the RGB data and a named ggplot object. The object includes metadata slots `tiff_path`, `resolution`, `precalc_max`, `rect`, `img_info` and `channels`.
#' @export
#' @import ggplot2
#'
#' @examples
#' tiff_path = exampleTiff()
#' fetchTiffData.rgb(tiff_path, rect = TiffRect(900,2300,1400,2800), red_channel=2, green_channel=3, blue_channel=1)
#'
#' channel_names = c("DAPI", "probe1", "probe2", "probe3", "probe4", "autofluor")
#' fetchTiffData.rgb(tiff_path, rect = TiffRect(900,2300,1400,2800), red_channel=2, green_channel=3, blue_channel=1, channel_names = channel_names)
#'
#' fetchTiffData.rgb(tiff_path, rect = TiffRect(900,2300,1400,2800),
#'   red_channel = "Opal 690", green_channel = "Opal 520", blue_channel = "DAPI", channel_names = "ome")
fetchTiffData.rgb = function(tiff_path,
                             rect = NULL,
                             resolution = NULL,
                             max_pixels = 800,
                             red_channel = 2,
                             green_channel = 3,
                             blue_channel = 1,
                             channel_names = NULL,
                             value_var = "norm_value",
                             precalc_max = NULL,
                             quantile_norm = .999
){
  rect = .rect_null_check(rect, tiff_path)
  if(nrow(rect@coords) != 1) stop("fetchTiffData.rgb requires a TiffRect with exactly one row")
  .fetch_tiff_data.rgb(tiff_path,
                       rect = rect,
                       resolution = resolution,
                       max_pixels = max_pixels,
                       red_channel = red_channel,
                       green_channel = green_channel,
                       blue_channel = blue_channel,
                       channel_names = channel_names,
                       value_var = value_var,
                       precalc_max = precalc_max,
                       quantile_norm = quantile_norm
  )
}

#' Plot a rectangular region of a TIFF image with multi-channel color mapping
#'
#' Creates a colorized composite visualization from a rectangular TIFF region.
#' Supports mapping any number of channels to named colors, then blends channel
#' contributions with additive clamping to RGB.
#'
#' @param tiff_path Path to the TIFF image file
#' @param rect A \linkS4class{TiffRect} object defining the rectangle region
#' @param resolution Resolution level to read from the TIFF file. If NULL, automatically selects resolution
#' @param max_pixels Maximum dimension in pixels for the plotted image
#' @param channel_colors Channel-to-color mapping. Use an unnamed positional list/vector (index = channel number),
#'   where NULL/NA entries are ignored, or a named list/vector whose names are channel names: those from
#'   `channel_names`, or the OME names (see \code{read_tiff_channels()}).
#' @param channel_names Channel names, as in \code{fetchTiffData()}: NULL,
#'   \code{"ome"}, or a character vector naming every channel.
#' @param value_var Variable to use for channel intensities, either "norm_value" or "value" (default "norm_value")
#' @param precalc_max Optional data frame with precalculated min/max values per channel for normalization
#' @param quantile_norm Quantile for normalization (default 0.999)
#'
#' @returns A TiffPlotData object containing the colorized data and a named ggplot object.
#' @export
#' @import ggplot2
#' 
#' @examples
#' library(TiffPlotR)
#' tf = exampleTiff()
#' fetchTiffData(tf, rect = TiffRect(2500, 3000, 2500, 3000))
#' fetchTiffData.colorized(tf,
#'   channel_colors = list("blue", NULL, NULL, NULL, "purple", "cyan"),
#'   rect = TiffRect(2500, 3000, 2500, 3000)
#' )
#' fetchTiffData.colorized(tf,
#'   channel_colors = list("A" = "blue", E = "purple", F = "cyan"),
#'   rect = TiffRect(2500, 3000, 2500, 3000),
#'   channel_names = LETTERS[1:6]
#' )
#' fetchTiffData.colorized(tf,
#'   channel_colors = list(DAPI = "blue", "Opal 570" = "yellow"),
#'   rect = TiffRect(2500, 3000, 2500, 3000)
#' )
fetchTiffData.colorized = function(tiff_path,
                                   rect = NULL,
                                   resolution = NULL,
                                   max_pixels = 800,
                                   channel_colors,
                                   channel_names = NULL,
                                   value_var = "norm_value",
                                   precalc_max = NULL,
                                   quantile_norm = .999){
  rect = .rect_null_check(rect, tiff_path)
  if(nrow(rect@coords) != 1) stop("fetchTiffData.colorized requires a TiffRect with exactly one row")
  if(!is.list(channel_colors)) stop("channel_colors must be a list.")
  .fetch_tiff_data.colorized(
    tiff_path = tiff_path,
    rect = rect,
    resolution = resolution,
    max_pixels = max_pixels,
    channel_colors = channel_colors,
    channel_names = channel_names,
    value_var = value_var,
    precalc_max = precalc_max,
    quantile_norm = quantile_norm
  )
}

.apply_coord_rect = function(p, r, coord_FUN){
  p + coord_FUN(xlim = c(r@coords$xmin, r@coords$xmax), ylim = c(r@coords$ymin, r@coords$ymax))
}

.apply_coord_FUN = function(img_data, coord_FUN){
  img_data = .check_TiffPlotData_version(img_data)
  r = img_data@rect
  img_data@plots = lapply(img_data@plots, function(p){
    .apply_coord_rect(p, r, coord_FUN)
  })
  img_data
}

#' Apply \code{coord_fixed()} to stored plots
#'
#' Applies \code{ggplot2::coord_fixed()} to each plot stored in a
#' \linkS4class{TiffPlotData} object using its rectangle extent.
#'
#' @param img_data A \linkS4class{TiffPlotData} object.
#'
#' @returns The modified \linkS4class{TiffPlotData} object.
#' @export
#'
#' @examples
#' \dontrun{
#' obj <- fetchTiffData(exampleTiff(), TiffRect(900, 2300, 1400, 2800))
#' obj <- apply_coord_fixed(obj)
#' }
apply_coord_fixed = function(img_data){
  .apply_coord_FUN(img_data, ggplot2::coord_fixed)
}

#' Apply \code{coord_cartesian()} to stored plots
#'
#' Applies \code{ggplot2::coord_cartesian()} to each plot stored in a
#' \linkS4class{TiffPlotData} object using its rectangle extent.
#'
#' @param img_data A \linkS4class{TiffPlotData} object.
#'
#' @returns The modified \linkS4class{TiffPlotData} object.
#' @export
#'
#' @examples
#' \dontrun{
#' obj <- fetchTiffData(exampleTiff(), TiffRect(900, 2300, 1400, 2800))
#' obj <- apply_coord_cartesian(obj)
#' }
apply_coord_cartesian = function(img_data){
  .apply_coord_FUN(img_data, ggplot2::coord_cartesian)
}

#' Plot a TIFF image as RGB with custom dimensions
#'
#' Reads a region from a TIFF image and creates an RGB composite visualization.
#' Combines three specified channels into red, green, and blue components with
#' automatic quantile normalization and legend display.
#'
#' @param tiff_path Path to the TIFF image file
#' @param rect A \linkS4class{TiffRect} object defining the rectangle region
#' @param resolution Resolution level to read from the TIFF file. If NULL, automatically selects resolution
#' @param max_pixels Maximum dimension in pixels for the plotted image
#' @param precalc_max Optional data frame with precalculated min/max values per channel for normalization
#' @param red_channel Channel number to use for red component (default 6)
#' @param green_channel Channel number to use for green component (default 1)
#' @param blue_channel Channel number to use for blue component (default 5)
#' @param value_var Variable to use for RGB values, either "norm_value" or "value" (default "norm_value")
#'
#' @returns A TiffPlotData object containing the RGB data and a named ggplot object. The object includes metadata slots `tiff_path`, `resolution`, `precalc_max`, `rect`, and `img_info`.
#' @import ggplot2
#' @importFrom data.table as.data.table
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' tiff_path = exampleTiff()
#' .fetch_tiff_data(tiff_path,
#'   rect = TiffRect(900, 2300, 1400, 2800))
#' fetchTiffData(tiff_path,
#'   rect = TiffRect(900,2300,1400,2800))
#'
#' .fetch_tiff_data.rgb(tiff_path, rect = TiffRect(900, 2300, 1400, 2800), red_channel=6, green_channel=1, blue_channel=5)
#' fetchTiffData.rgb(tiff_path, rect = TiffRect(900,2300,1400,2800), red_channel=6, green_channel=1, blue_channel=5)
.fetch_tiff_data.rgb = function(
    tiff_path,
    rect = NULL,
    resolution = NULL,
    max_pixels = 800,
    red_channel = 6,
    green_channel = 1,
    blue_channel = 5,
    channel_names = NULL,
    value_var = "norm_value",
    precalc_max = NULL,
    quantile_norm = .999
){
  img_obj = .fetch_tiff_data(
    tiff_path = tiff_path,
    rect = rect,
    resolution = resolution,
    max_pixels = max_pixels,
    precalc_max = precalc_max,
    quantile_norm = quantile_norm,
    channel_names = channel_names
  )
  channel_table = img_obj@channels
  rgb_channels = vapply(list(red_channel, green_channel, blue_channel), .resolve_channels, numeric(1),
                        channel_table = channel_table, arg = "red/green/blue_channel")
  img_df = img_obj@data
  rgb_df = convertTidyToRGB(img_df, red_channel = rgb_channels[1], green_channel = rgb_channels[2], blue_channel = rgb_channels[3], value_var = value_var)
  
  leg_df = data.frame(channel = rgb_channels)
  leg_df$colors = c("red", "green", "blue")
  leg_cols = leg_df$colors
  leg_df$channel_name = channel_table$channel_name[match(leg_df$channel, channel_table$channel)]
  names(leg_cols) = leg_df$channel_name
  suppressWarnings({
    p_rgb = ggplot() +
      .fill_identity_layers(rgb_df) +
      geom_point(data = leg_df, aes(color = channel_name), x = NA, y = NA, alpha = 0, na.rm = TRUE) +
      scale_color_manual(values = leg_cols, name = NULL) +
      guides(color = guide_legend(override.aes = list(size = 12, shape = 16, alpha = 1),
                                  theme = theme(legend.background = element_blank(), legend.key = element_blank()))) +
      scale_y_reverse() +
      theme(panel.spacing = unit(0, "npc")) + #, panel.background = element_blank(), axis.text = element_text(size = 6)) +
      # theme(legend.position = "bottom") +
      labs(x= "pixel", y = "pixel")
  })
  p_rgb = .apply_coord_rect(p_rgb, rect, ggplot2::coord_fixed)
  # lets renormalize() rebuild the composite
  p_rgb$meta$tiffplotr_composite = list(type = "rgb",
                                        channels = rgb_channels,
                                        value_var = value_var)
  img_obj@plots = list(rgb = p_rgb)
  img_obj@activePlot = "rgb"
  img_obj
}

.fetch_tiff_data.colorized = function(
    tiff_path,
    rect = NULL,
    resolution = NULL,
    max_pixels = 800,
    channel_colors,
    channel_names = NULL,
    value_var = "norm_value",
    precalc_max = NULL,
    quantile_norm = .999
){
  img_obj = .fetch_tiff_data(
    tiff_path = tiff_path,
    rect = rect,
    resolution = resolution,
    max_pixels = max_pixels,
    precalc_max = precalc_max,
    quantile_norm = quantile_norm,
    channel_names = channel_names
  )
  
  img_df = img_obj@data
  channel_map = .resolve_channel_color_map(
    channel_colors = channel_colors,
    channel_table = img_obj@channels
  )
  
  rgb_df = .convert_tidy_to_colorized(
    img_df = img_df,
    channel_map = channel_map,
    value_var = value_var
  )
  
  leg_df = channel_map
  leg_df$channel_name = factor(leg_df$channel_name, levels = leg_df$channel_name)
  leg_cols = leg_df$color
  names(leg_cols) = as.character(leg_df$channel_name)
  
  suppressWarnings({
    p_rgb = ggplot() +
      .fill_identity_layers(rgb_df) +
      geom_point(data = leg_df, aes(color = channel_name), x = NA, y = NA, alpha = 0, na.rm = TRUE) +
      scale_color_manual(values = leg_cols, name = NULL) +
      guides(color = guide_legend(override.aes = list(size = 12, shape = 16, alpha = 1),
                                  theme = theme(legend.background = element_blank(), legend.key = element_blank()))) +
      scale_y_reverse() +
      theme(panel.spacing = unit(0, "npc")) +
      labs(x= "pixel", y = "pixel")
  })
  
  p_rgb = .apply_coord_rect(p_rgb, rect, ggplot2::coord_fixed)
  # lets renormalize() rebuild the composite
  p_rgb$meta$tiffplotr_composite = list(type = "colorized",
                                        channel_map = channel_map,
                                        value_var = value_var)
  img_obj@plots = list(colorized = p_rgb)
  img_obj@activePlot = "colorized"
  img_obj
}

#' Read TIFF image metadata
#'
#' Extracts metadata information from a TIFF image file including image
#' dimensions and available resolution levels.
#'
#' @param tiff_path Path to the TIFF image file
#'
#' @returns A data frame with columns:
#'   \item{sizeX}{Image width in pixels}
#'   \item{sizeY}{Image height in pixels}
#'   \item{sizeC}{Number of channels}
#'   \item{resolutionLevel}{Resolution level number}
#' @export
#' @importFrom RBioFormats read.metadata
#'
#' @examples
#' \dontrun{
#'   img_info <- read_tiff_meta_data("image.tiff")
#'   print(img_info)
#' }
#'
read_tiff_meta_data = function(tiff_path){
  img_info_df = RBioFormats::read.metadata(tiff_path)
  if(!is(img_info_df, "ImageMetadataList")){
    if(is(img_info_df, "ImageMetadata")){
      img_info_df = list(img_info_df)
    }else{
      stop('unexpected class type from RBioFormats::read.metadata')
    }
  }
  df.l = lapply(img_info_df@.Data, function(tmp){
    as.data.frame(tmp$coreMetadata[c("series", "sizeX", "sizeY", "sizeC", "resolutionLevel")]  )
  })
  img_info_df = do.call(rbind, df.l)
  subset(img_info_df, series == 1)
}



#' Calculate quantile values for image channels
#'
#' Computes quantile statistics (0%, 10%, 50%, 75%, 90%, 95%, 99.5%, 99.8%, 99.99%)
#' for each channel at a specified resolution level. Primarily used for determining
#' appropriate normalization parameters.
#'
#' @param tiff_path Path to the TIFF image file
#' @param resolution Resolution level to analyze
#'
#' @returns A data frame with columns:
#'   \item{channel}{Channel number}
#'   \item{max_value}{99.8th percentile value for the channel}
#'   \item{min_value}{90th percentile value for the channel}
#'   \item{resolution}{Resolution level analyzed}
#' @export
#' @importFrom RBioFormats read.image
#'
#' @examples
#' \dontrun{
#'   qmax_values <- calc_qmax("image.tiff", resolution=3)
#' }
#'
calc_qmax = function(tiff_path, resolution){
  message("calc_qmax for ", resolution)
  img_data = RBioFormats::read.image(
    tiff_path,
    normalize = FALSE,
    series = 1,
    resolution = resolution
  )
  # qmaxes = apply(img_data, MARGIN = 3, FUN = quantile, probs = .995)
  
  qres = apply(img_data, MARGIN = 3, FUN = quantile, probs = c(0, .1, .5, .75, .9, .95, .995, .998, .9999))
  qres
  qmaxes = qres["99.8%",]
  qmins = qres["90%",]
  data.frame(channel = seq_along(qmaxes), max_value = qmaxes, min_value = qmins, resolution = resolution)
}

#' Find high-intensity regions in a TIFF image
#'
#' Identifies regions in an image where pixel intensities exceed the 99.8th percentile.
#' Uses k-means clustering to group high-intensity pixels and identify distinct regions.
#' Useful for finding regions of interest that warrant further analysis.
#'
#' @param tiff_path Path to the TIFF image file
#' @param resolution Resolution level to analyze (default 5). Higher values = lower resolution
#'
#' @returns A data frame with columns:
#'   \item{i}{X coordinate of the region center}
#'   \item{j}{Y coordinate of the region center}
#'   \item{channel}{Channel number where the region was detected}
#' @export
#'
#' @examples
#' \dontrun{
#'   regions <- find_qmax_regions("image.tiff", resolution=5)
#'   head(regions)
#' }
#'
find_qmax_regions = function(tiff_path, resolution = 5){
  info_df = read_tiff_meta_data(tiff_path)
  info_df.max = subset(info_df, resolutionLevel == min(resolutionLevel))
  info_df.sel = subset(info_df, resolutionLevel == resolution)
  img_obj = fetchTiffData(tiff_path, 0, info_df.max$sizeX, 0, info_df.max$sizeY, resolution = resolution)
  img_data = img_obj@data
  img_qmaxes = img_data %>% group_by(channel) %>% summarise(qmax = quantile(value, .998))
  qmaxes_todo = split(img_qmaxes$qmax, img_qmaxes$channel)
  
  img_data = as.data.table(img_data)
  region_data = list()
  for(i_chan in as.integer(img_qmaxes$channel)){
    i_qmax = subset(img_qmaxes, channel == i_chan)$qmax
    over_qmax = img_data[channel == i_chan][value >= i_qmax]
    # use kmeans to find regions that aren't too close to one another
    kres = kmeans(over_qmax, centers = 20)
    over_qmax
    cent_dt = as.data.table(kres$centers)
    # centroids aren't necessarily points, find closest actual point to center
    cent_dt[, c("nearest_i", "nearest_j") := {min_idx = which.min(sqrt((i - over_qmax$i)^2 + (j - over_qmax$j) ^ 2)); list(over_qmax$i[min_idx], over_qmax$j[min_idx])}, .(i, j)]
    
    region_data[[i_chan]] = cent_dt[, .(i = nearest_i, j = nearest_j, channel)]
  }
  do.call(rbind, region_data)
}


#' Path to the example tiff file for testing and examples.
#'
#' @returns Path to the example tiff file
#' @export
#'
#' @examples
#' exampleTiff()
exampleTiff = function(){
  system.file("extdata/301_CellPellet_NegCTL_Scan1_PC3Guise.ome.tiff", package = "TiffPlotR", mustWork = TRUE)
}
