#' Plot a rectangular region of a TIFF image
#'
#' Creates a visualization of a rectangular region from a TIFF image file.
#' This function is a wrapper around .fetch_tiff_data that converts rectangle
#' coordinates to image dimensions.
#'
#' @param tiff_path Path to the TIFF image file
#' @param rect A \linkS4class{TiffRect} object defining the rectangle region
#' @param resolution Resolution level to read from the TIFF file. If NULL, automatically selects resolution to keep image under max_pixels
#' @param max_pixels Maximum dimension in pixels for the plotted image. Used for automatic resolution selection
#' @param precalc_max Optional data frame with precalculated min/max values per channel for normalization
#' @param show_raw If TRUE, displays raw pixel values; if FALSE (default), displays normalized values
#' @param quantile_norm Quantile for normalization (default 0.999). Values are divided by this quantile
#'
#' @returns A TiffPlotData object containing the sparse image data and a named ggplot object.  The returned object also has slots `tiff_path`, `resolution`, `precalc_max`, `rect`, and `img_info` describing provenance of the image region.
#' @export
#' @importFrom dplyr group_by mutate select
#' @importFrom magrittr %>%
#' @import ggplot2
#'
#' @examples
#' tiff_path = exampleTiff()
#' view_rect = TiffRect(900,2300,1400,2800)
#' fetchTiffData(
#'   tiff_path,
#'   view_rect
#' )
#' view_rect2 = rect_shift(view_rect, dx = 1000, dy = 500)
#' fetchTiffData(
#'   tiff_path,
#'   view_rect2
#' )
#' 
#' view_rect3 = resize_rect_mult(view_rect2, 3)
#' fetchTiffData(
#'   tiff_path,
#'   view_rect3
#' )
#' tiff_data = .Last.value
#' class(tiff_data)
#' p = ggplot2::last_plot() 
#' rect_annotate(p, view_rect2) 
fetchTiffData = function(tiff_path, rect, resolution = NULL, max_pixels = 800, precalc_max = NULL, show_raw = FALSE, quantile_norm = .999){
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect object")
  .fetch_tiff_data(tiff_path,
                  x_start = rect@xmin,
                  x_width = rect@xmax - rect@xmin,
                  y_start = rect@ymin,
                  y_width = rect@ymax - rect@ymin,
                  resolution = resolution,
                  max_pixels = max_pixels,
                  precalc_max = precalc_max,
                  show_raw = show_raw,
                  quantile_norm = quantile_norm
  )
}

#' Plot a TIFF image with custom dimensions
#'
#' Reads a region from a TIFF image file and creates a ggplot visualization.
#' Automatically handles multi-resolution TIFF files and selects appropriate
#' resolution based on desired pixel dimensions.
#'
#' @param tiff_path Path to the TIFF image file
#' @param x_start Starting x coordinate (in original image coordinates)
#' @param x_width Width of region to read in x direction (in original image coordinates)
#' @param y_start Starting y coordinate (in original image coordinates)
#' @param y_width Width of region to read in y direction (in original image coordinates)
#' @param resolution Resolution level to read from the TIFF file. If NULL, automatically selects resolution to keep image under max_pixels
#' @param max_pixels Maximum dimension in pixels for the plotted image. Used for automatic resolution selection
#' @param precalc_max Optional data frame with precalculated min/max values per channel for normalization
#' @param show_raw If TRUE, displays raw pixel values; if FALSE (default), displays normalized values
#' @param quantile_norm Quantile for normalization (default 0.999). Values are divided by this quantile
#'
#' @returns A TiffPlotData object containing the sparse image data and a named list of ggplot objects. It also records `tiff_path`, `resolution`, `precalc_max` and `rect` slots.
#' @importFrom dplyr group_by mutate select summarise
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom RBioFormats read.image
#'
#' @examples
#' \dontrun{
#'   .fetch_tiff_data("image.tiff", x_start=100, x_width=400, y_start=200, y_width=400)
#' }
.fetch_tiff_data = function(tiff_path, x_start, x_width, y_start, y_width, resolution = NULL, max_pixels = 800, precalc_max = NULL, show_raw = FALSE, quantile_norm = .999){
  img_info = read_tiff_meta_data(tiff_path)
  max_info = subset(img_info, resolutionLevel == 1)
  if(is.null(resolution)){
    #select the highest resolution under max_pixels
    x_pixels = x_width* img_info$sizeX / max_info$sizeX
    y_pixels = y_width* img_info$sizeY / max_info$sizeY
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
  res_info = subset(img_info, resolutionLevel == resolution)

  x_ratio = res_info$sizeX / max_info$sizeX
  y_ratio = res_info$sizeY / max_info$sizeY
  img_data = RBioFormats::read.image(
    tiff_path,
    normalize = FALSE,
    series = 1,
    resolution = resolution,
    subset = list(
      X = seq(x_start*x_ratio, (x_start + x_width)*x_ratio),
      Y = seq(y_start*y_ratio, (y_start + y_width)* y_ratio)
    ))

  annotated_image = img_data
  # 1. Assume 'annotated_image' is a 2D matrix (or array converted to matrix)
  # 2. Find indices of non-zero pixels
  non_zero_coords <- which(annotated_image != 0, arr.ind = TRUE)
  if(nrow(non_zero_coords) == 0){
    stop("No data found in area.")
  }

  # report plotted image stats
  x_pix = diff(range(non_zero_coords[, "x"])) %>% as.numeric
  y_pix = diff(range(non_zero_coords[, "y"])) %>% as.numeric
  pix_area = x_pix*y_pix
  if(pix_area >= 1e5){
    pix_area = paste0(round(pix_area / 1e6, 2), "M")
  }else if(pix_area >= 1e3){
    pix_area = paste0(round(pix_area / 1e3, 2), "k")
  }
  message("plotted image is ", x_pix, "x", y_pix, " (", pix_area,  " pixels)")

  # 3. Create sparse matrix
  sparse_img <- data.frame(
    i = (non_zero_coords[, "x"] + x_start*x_ratio)/x_ratio,
    j = (non_zero_coords[, "y"] + y_start*y_ratio)/y_ratio,
    channel = non_zero_coords[, "c"],
    value = annotated_image[non_zero_coords]
  )

  sparse_img$i = as.integer(sparse_img$i)
  sparse_img$j = as.integer(sparse_img$j)

  if(is.null(precalc_max)){
    precalc_max = sparse_img  %>% group_by(channel) %>% summarise(min_value = 0, max_value = quantile(value, quantile_norm))
  }
  stopifnot(c("channel", "min_value", "max_value") %in% colnames(precalc_max))

  sparse_img = merge(sparse_img, precalc_max %>% select(channel, min_value, max_value), all.x = TRUE)
  sparse_img = sparse_img %>% mutate(norm_value = (value - min_value) / (max_value - min_value))
  sparse_img = sparse_img %>% select(!c(min_value, max_value))

  #cap norm_value to 1
  sparse_img = sparse_img %>% mutate(norm_value = ifelse(norm_value > 1, 1, norm_value))
  sparse_img = sparse_img %>% mutate(norm_value = ifelse(norm_value < 0, 0, norm_value))


  # report fullresolution image stats
  x_pix = diff(range(sparse_img$i)) %>% as.numeric
  y_pix = diff(range(sparse_img$j)) %>% as.numeric
  pix_area = x_pix*y_pix
  if(pix_area >= 1e5){
    pix_area = paste0(round(pix_area / 1e6, 2), "M")
  }else if(pix_area >= 1e3){
    pix_area = paste0(round(pix_area / 1e3, 2), "k")
  }
  message("full resolution image would have been ", x_pix, "x", y_pix, " (", pix_area,  " pixels)")

  # return_data parameter removed; functions now return a TiffPlotData object
  if(show_raw){
    p = ggplot(sparse_img, aes(x = i, y = j, fill = value))
  }else{
    p = ggplot(sparse_img, aes(x = i, y = j, fill = norm_value))
  }
  p = p +
    facet_wrap(~channel) +
    scale_y_reverse() +
    geom_raster() +
    scale_fill_viridis_c(option = "magma") +
    theme(panel.background = element_rect(fill = "gray20"), panel.grid = element_blank())

  plots_list <- list(channels = p)
  data_df <- as.data.frame(sparse_img)
  # record rectangle corresponding to requested region
  rect_obj <- TiffRect(x_start, x_start + x_width, y_start, y_start + y_width)
  new("TiffPlotData",
      data = data_df,
      plots = plots_list,
      activePlot = names(plots_list)[1],
      tiff_path = tiff_path,
      resolution = resolution,
      precalc_max = if(is.null(precalc_max)) data.frame() else precalc_max,
      rect = rect_obj,
      img_info = img_info)
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
#' @param red_channel Channel number to use for red component (default 6)
#' @param green_channel Channel number to use for green component (default 1)
#' @param blue_channel Channel number to use for blue component (default 5)
#' @param value_var Variable to use for RGB values, either "norm_value" or "value" (default "norm_value")
#'
#' @returns A TiffPlotData object containing the RGB data and a named ggplot object. The object includes metadata slots `tiff_path`, `resolution`, `precalc_max`, `rect`, and `img_info`.
#' @export
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#'   r <- TiffRect(100,500,200,600)
#'   fetchTiffData.rgb("image.tiff", r,
#'                      red_channel=6, green_channel=1, blue_channel=5)
#' }
fetchTiffData.rgb = function(tiff_path,
                              rect,
                              resolution = NULL,
                              max_pixels = 800,
                              red_channel = 6,
                              green_channel = 1,
                              blue_channel = 5,
                              value_var = "norm_value"){
  if(!is(rect, "TiffRect")) stop("rect must be a TiffRect object")
  .fetch_tiff_data.rgb(tiff_path,
                      x_start = rect@xmin,
                      x_width = rect@xmax - rect@xmin,
                      y_start = rect@ymin,
                      y_width = rect@ymax - rect@ymin,
                      resolution = resolution,
                      max_pixels = max_pixels,
                      red_channel = red_channel,
                      green_channel = green_channel,
                      blue_channel = blue_channel,
                      value_var = value_var
  )
}

#' Plot a TIFF image as RGB with custom dimensions
#'
#' Reads a region from a TIFF image and creates an RGB composite visualization.
#' Combines three specified channels into red, green, and blue components with
#' automatic quantile normalization and legend display.
#'
#' @param tiff_path Path to the TIFF image file
#' @param x_start Starting x coordinate (in original image coordinates)
#' @param x_width Width of region to read in x direction (in original image coordinates)
#' @param y_start Starting y coordinate (in original image coordinates)
#' @param y_width Width of region to read in y direction (in original image coordinates)
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
#'   x_start=900, x_width=1400,
#'   y_start=1400, y_width=1400)
#' fetchTiffData(tiff_path,
#'   rect = TiffRect(900,2300,1400,2800))
#'
#' .fetch_tiff_data.rgb(tiff_path, x_start=900, x_width=1400, y_start=1400, y_width=1400, red_channel=6, green_channel=1, blue_channel=5)
#' fetchTiffData.rgb(tiff_path, rect = TiffRect(900,2300,1400,2800), red_channel=6, green_channel=1, blue_channel=5)
.fetch_tiff_data.rgb = function(
    tiff_path,
    x_start, x_width,
    y_start, y_width,
    resolution = NULL,
    max_pixels = 800,
    red_channel = 6,
    green_channel = 1,
    blue_channel = 5,
    value_var = "norm_value"
){
  img_obj = .fetch_tiff_data(
    tiff_path = tiff_path,
    x_start = x_start,
    x_width = x_width,
    y_start = y_start,
    y_width = y_width,
    resolution = resolution,
    max_pixels = max_pixels
  )
  img_df = img_obj@data
  rv = red_channel
  gv = green_channel
  bv = blue_channel

  rgb_df = subset(img_df, channel %in% c(rv, gv, bv)) %>%
    tidyr::pivot_wider(id_cols = c("i", "j"), names_from = "channel", values_from = all_of(value_var))
  rgb_df = rgb_df %>% mutate(i, j, red = !!sym(as.character(rv)), green = !!sym(as.character(gv)), blue = !!sym(as.character(bv)), .keep = "none")

  #strip NA values
  rgb_df = rgb_df %>%
    mutate(red = ifelse(is.na(red), 0, red)) %>%
    mutate(green = ifelse(is.na(green), 0, green)) %>%
    mutate(blue = ifelse(is.na(blue), 0, blue))
  rgb_df = rgb_df %>% mutate(chex = rgb(red/max(red, na.rm = TRUE), green/max(green, na.rm = TRUE), blue/max(blue, na.rm = TRUE)))
  # rgb_df %>% mutate(chex = rgb(red/max(red, na.rm = TRUE), green/max(green, na.rm = TRUE), blue/max(blue, na.rm = TRUE)))

  leg_df = data.frame(channel = c(red_channel, green_channel, blue_channel))
  leg_df$dummy = LETTERS[seq_along(leg_df$channel)]
  leg_df$dummy = factor(leg_df$dummy)
  leg_df$colors = c("red", "green", "blue")
  leg_cols = leg_df$colors
  names(leg_cols) = leg_df$dummy
  # leg_df$channel = factor(leg_df$channel, levels = leg_df$channel)

  suppressWarnings({
    p_rgb = ggplot() +
      geom_raster(data = rgb_df, aes(x = i, y = j, fill = chex)) +
      geom_point(data = leg_df, aes(color = dummy), x = NA, y = NA, alpha = 0) +
      scale_color_manual(values = leg_cols, labels = leg_df$channel) +
      guides(color = guide_legend(override.aes = list(size = 12, shape = 16, alpha = 1),
                                  theme = theme(legend.background = element_blank(), legend.key = element_blank()))) +
      scale_y_reverse() +
      theme(panel.spacing = unit(0, "npc"), panel.background = element_blank(), axis.text = element_text(size = 6)) +
      theme(legend.position = "bottom") +
      scale_fill_identity() +
      labs(x= "pixel", y = "pixel") +
      coord_fixed()
  })
  plots_list <- list(rgb = p_rgb)
  data_df <- as.data.frame(rgb_df)
  new("TiffPlotData",
      data = data_df,
      plots = plots_list,
      activePlot = names(plots_list)[1],
      tiff_path = tiff_path,
      resolution = img_obj@resolution,
      precalc_max = img_obj@precalc_max,
      rect = img_obj@rect,
      img_info = img_obj@img_info)
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

  browser()
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

