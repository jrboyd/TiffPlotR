
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
#'   \code{min_value}, and \code{max_value} for normalization.
#' @param show_raw If TRUE, plot raw values. If FALSE (default), plot
#'   normalized values.
#' @param quantile_norm Quantile used to estimate per-channel max values when
#'   \code{precalc_max} is NULL.
#' @param channel_names Optional character vector used to rename channels.
#' @param selected_channels Optional subset of channels to keep, as channel
#'   indices or names.
#'
#' @returns A \linkS4class{TiffPlotData} object with image data in
#'   \code{@data} and a faceted plot in \code{@plots}.
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
fetchTiffData = function(tiff_path, rect = NULL, resolution = NULL, max_pixels = 800, precalc_max = NULL, show_raw = FALSE, quantile_norm = .999, channel_names = NULL, selected_channels = NULL){
    rect = .rect_null_check(rect, tiff_path)
    .fetch_tiff_data(tiff_path,
                     rect,
                     resolution = resolution,
                     max_pixels = max_pixels,
                     precalc_max = precalc_max,
                     show_raw = show_raw,
                     quantile_norm = quantile_norm,
                     channel_names = channel_names,
                     selected_channels = selected_channels
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
#' @param channel_names Optional character vector used to rename channels.
#' @param selected_channels Optional subset of channels to keep, as channel indices
#'   or names.
#' @param bit_order Bit order to use when decoding OME mask payloads.
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
                                bit_order = c("msb", "lsb")) {
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
        selected_channels = selected_channels
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
        facet_grid(channel~Text) +
        scale_y_reverse() +
        geom_raster() +
        scale_fill_viridis_c(option = "magma") +
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
        mask_points = mask_points,
        mask_color_mappings = mask_color_mappings
    )

    out_obj@unit_per_pixel <- base_obj@unit_per_pixel
    out_obj@unit_name <- base_obj@unit_name
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
        tidyr::pivot_wider(id_cols = c("i", "j"), names_from = "channel", values_from = all_of(value_var))
    rgb_df = rgb_df %>% mutate(i, j, red = !!sym(as.character(rv)), green = !!sym(as.character(gv)), blue = !!sym(as.character(bv)), .keep = "none")

    #strip NA values
    rgb_df = rgb_df %>%
        mutate(red = ifelse(is.na(red), 0, red)) %>%
        mutate(green = ifelse(is.na(green), 0, green)) %>%
        mutate(blue = ifelse(is.na(blue), 0, blue))
    rgb_df = rgb_df %>% mutate(chex = rgb(red/max(red, na.rm = TRUE), green/max(green, na.rm = TRUE), blue/max(blue, na.rm = TRUE)))
    rgb_df
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
#'
#' @returns A TiffPlotData object containing the sparse image data and a named list of ggplot objects. It also records `tiff_path`, `resolution`, `precalc_max` and `rect` slots.
#' @importFrom dplyr group_by mutate select summarise
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom RBioFormats read.image
#'
.fetch_tiff_data = function(tiff_path, rect,
                            resolution = NULL, max_pixels = 800,
                            precalc_max = NULL, show_raw = FALSE,
                            quantile_norm = .999,
                            channel_names = NULL,
                            selected_channels = NULL){
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

    if(is.null(precalc_max)){
        precalc_max = tidy_img  %>% group_by(channel) %>% summarise(min_value = 0, max_value = quantile(value, quantile_norm))
    }
    stopifnot(c("channel", "min_value", "max_value") %in% colnames(precalc_max))

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

    if(!is.null(channel_names)){
        nchan = length(unique(tidy_img$channel))
        if(length(channel_names) != nchan){
            if(length(channel_names) < nchan){
                stop("Not enough channel names specified, expected ", nchan, " channels.")
            }else{
                warning("Too many channel names specified, only found ", nchan, " channels.")
            }
        }
        tidy_img$channel = channel_names[tidy_img$channel]
        tidy_img$channel = factor(tidy_img$channel, levels = channel_names)
    }
    # wait to selectchannels until possibly converted to names
    if(!is.null(selected_channels)){
        if(is.numeric(selected_channels)){
            if(!is.null(channel_names)){
                selected_channels = channel_names[selected_channels]
            }
        }
        if(!(is.numeric(selected_channels) & is.numeric(tidy_img$channel))){
            .is_char_or_fact = function(x){
                is.character(x) | is.factor(x)
            }
            if(!(.is_char_or_fact(selected_channels) & .is_char_or_fact(tidy_img$channel))){
                stop("selected channels and channel are not compatible types.")
            }
        }
        if(!all(selected_channels %in% unique(tidy_img$channel))){
            stop("Not all selected_channels are present. Offending values:\n",
                 paste(setdiff(selected_channels, unique(tidy_img$channel)), collapse = "\n")
            )
        }
        tidy_img = tidy_img %>% dplyr::filter(channel %in% selected_channels)
        #selected channels also impose order
        tidy_img$channel = factor(tidy_img$channel, levels = selected_channels)
    }

    # return_data parameter removed; functions now return a TiffPlotData object
    if(show_raw){
        p = ggplot(tidy_img, aes(x = i, y = j, fill = value))
    }else{
        p = ggplot(tidy_img, aes(x = i, y = j, fill = norm_value))
    }

    p = p +
        facet_wrap(~channel) +
        scale_y_reverse() +
        geom_raster() +
        scale_fill_viridis_c(option = "magma") #+
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
        unit_name = pixel_scale$unit_name)
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
#' tiff_path = exampleTiff()
#' fetchTiffData.rgb(tiff_path, rect = TiffRect(900,2300,1400,2800), red_channel=2, green_channel=3, blue_channel=1)
#'
#' channel_names = c("DAPI", "probe1", "probe2", "probe3", "probe4")
#' fetchTiffData.rgb(tiff_path, rect = TiffRect(900,2300,1400,2800), red_channel=2, green_channel=3, blue_channel=1, channel_names = channel_names)
#'
#' fetchTiffData.rgb(tiff_path, rect = TiffRect(900,2300,1400,2800), red_channel=5, green_channel=4, blue_channel=1, channel_names = channel_names)
fetchTiffData.rgb = function(tiff_path,
                             rect = NULL,
                             resolution = NULL,
                             max_pixels = 800,
                             red_channel = 2,
                             green_channel = 3,
                             blue_channel = 1,
                             channel_names = NULL,
                             value_var = "norm_value"){
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
                         value_var = value_var
    )
}

.apply_coord_rect = function(p, r, coord_FUN){
    p + coord_FUN(xlim = c(r@coords$xmin, r@coords$xmax), ylim = c(r@coords$ymin, r@coords$ymax))
}

.apply_coord_FUN = function(img_data, coord_FUN){
    r = img_data@rect
    img_data@plots = lapply(img_data@plots, function(p){
        .apply_coord_rect(p, r, coord_FUN)
    })

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
        value_var = "norm_value"
){
    img_obj = .fetch_tiff_data(
        tiff_path = tiff_path,
        rect = rect,
        resolution = resolution,
        max_pixels = max_pixels,
    )
    img_df = img_obj@data
    rgb_df = convertTidyToRGB(img_df, red_channel = red_channel, green_channel = green_channel, blue_channel = blue_channel, value_var = value_var)

    leg_df = data.frame(channel = c(red_channel, green_channel, blue_channel))
    leg_df$dummy = as.character(leg_df$channel)
    leg_df$dummy = factor(leg_df$dummy)
    leg_df$colors = c("red", "green", "blue")
    leg_cols = leg_df$colors

    # leg_df$channel = factor(leg_df$channel, levels = leg_df$channel)

    if(is.null(channel_names)){
        leg_df$channel_name = as.character(leg_df$dummy)

    }else{
        leg_df$channel_name = channel_names[leg_df$channel]
    }
    names(leg_cols) = leg_df$channel_name
    suppressWarnings({
        p_rgb = ggplot() +
            geom_raster(data = rgb_df, aes(x = i, y = j, fill = chex)) +
            geom_point(data = leg_df, aes(color = channel_name), x = NA, y = NA, alpha = 0) +
            scale_color_manual(values = leg_cols, name = NULL) +
            guides(color = guide_legend(override.aes = list(size = 12, shape = 16, alpha = 1),
                                        theme = theme(legend.background = element_blank(), legend.key = element_blank()))) +
            scale_y_reverse() +
            theme(panel.spacing = unit(0, "npc")) + #, panel.background = element_blank(), axis.text = element_text(size = 6)) +
            # theme(legend.position = "bottom") +
            scale_fill_identity() +
            labs(x= "pixel", y = "pixel")
    })
    p_rgb = .apply_coord_rect(p_rgb, rect, ggplot2::coord_fixed)
    plots_list <- list(rgb = p_rgb)
    # data_df <- as.data.frame(rgb_df)
    new("TiffPlotData",
        data = img_obj@data,
        plots = plots_list,
        activePlot = names(plots_list)[1],
        tiff_path = tiff_path,
        resolution = img_obj@resolution,
        precalc_max = img_obj@precalc_max,
        rect = img_obj@rect,
        img_info = img_obj@img_info,
        unit_per_pixel = img_obj@unit_per_pixel,
        unit_name = img_obj@unit_name)
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

#' Collect channel quantiles across all TIFF resolutions
#'
#' Reads every available resolution level once and computes the requested
#' quantiles for each channel, returning a long-format table that can be
#' inspected before choosing normalization cutoffs.
#'
#' @param tiff_path Path to the TIFF image file.
#' @param probs Numeric vector of quantiles to compute. Defaults to:
#'   \code{sort(c(seq(0, 100) / 100, 0.99 + seq(1, 9) / 1000))}.
#' @param max_full_fetch Maximum number of pixels to read as a full image for a
#'   given resolution. Resolutions above this threshold are sampled using
#'   informative regions.
#' @param n_each Number of sampled regions per intensity band (low/mid/high)
#'   when sampling is used.
#' @param sample_rect_width Optional sampled region width in full-resolution
#'   pixels. If NULL, selected automatically from \code{max_full_fetch} and
#'   \code{n_each}.
#' @param sample_rect_height Optional sampled region height in full-resolution
#'   pixels. If NULL, selected automatically from \code{max_full_fetch} and
#'   \code{n_each}.
#' @param scout_resolution Optional resolution level used to choose informative
#'   regions.
#' @param sample_summary_fun Summary statistic used to score candidate regions:
#'   one of \code{"mean"}, \code{"median"}, \code{"max"}.
#' @param sample_channel Optional channel index used for region scoring.
#' @param sample_seed Optional seed for reproducible sampled region selection.
#'
#' @returns A data.frame with columns \code{channel}, \code{resolution},
#'   \code{prob}, and \code{value}. Additional columns
#'   \code{source_mode} (\code{"full"} or \code{"sampled"}) and
#'   \code{pixels_used} report how each resolution was processed.
#' @export
#'
#' @examples
#' \dontrun{
#' qtab <- collect_channel_quantiles_all_resolutions(exampleTiff())
#' subset(qtab, channel == 1)
#' }
collect_channel_quantiles_all_resolutions <- function(
        tiff_path,
        probs = sort(c(seq(0, 100) / 100, 0.99 + seq(1, 9) / 1000)),
        max_full_fetch = 5e6,
        n_each = 10,
        sample_rect_width = NULL,
        sample_rect_height = NULL,
        scout_resolution = NULL,
        sample_summary_fun = c("mean", "median", "max"),
        sample_channel = NULL,
        sample_seed = NULL
) {
    sample_summary_fun <- match.arg(sample_summary_fun)

    if (!is.numeric(probs) || length(probs) < 1L || any(!is.finite(probs)) || any(probs < 0) || any(probs > 1)) {
        stop("probs must be a numeric vector with values in [0, 1]")
    }
    if (!is.numeric(max_full_fetch) || length(max_full_fetch) != 1L || !is.finite(max_full_fetch) || max_full_fetch <= 0) {
        stop("max_full_fetch must be a single positive numeric value")
    }
    if (!is.numeric(n_each) || length(n_each) != 1L || !is.finite(n_each) || n_each < 1) {
        stop("n_each must be a single integer >= 1")
    }
    n_each <- as.integer(n_each)

    probs <- sort(unique(as.numeric(probs)))

    img_info <- read_tiff_meta_data(tiff_path)
    resolutions <- sort(unique(img_info$resolutionLevel))
    max_info <- subset(img_info, resolutionLevel == 1)
    full_w <- as.numeric(max_info$sizeX[[1]])
    full_h <- as.numeric(max_info$sizeY[[1]])

    sampled_rect <- NULL

    .sampled_quantiles <- function(rect_obj, resolution) {
        n_rect <- nrow(rect_obj@coords)
        sampled_arrays <- lapply(seq_len(n_rect), function(i) {
            fetchTiffArray(
                tiff_path = tiff_path,
                rect = rect_obj[i],
                resolution = resolution
            )
        })

        first_dim <- dim(sampled_arrays[[1]])
        n_channels <- if (length(first_dim) >= 3) first_dim[[3]] else 1
        ch_vals <- replicate(n_channels, numeric(0), simplify = FALSE)
        pixels_used <- 0

        for (arr in sampled_arrays) {
            arr_dim <- dim(arr)
            pixels_used <- pixels_used + as.numeric(arr_dim[[1]] * arr_dim[[2]])
            if (length(arr_dim) >= 3) {
                for (ch in seq_len(n_channels)) {
                    ch_vals[[ch]] <- c(ch_vals[[ch]], as.numeric(arr[, , ch]))
                }
            } else {
                ch_vals[[1]] <- c(ch_vals[[1]], as.numeric(arr))
            }
        }

        qres <- do.call(cbind, lapply(seq_len(n_channels), function(ch) {
            stats::quantile(ch_vals[[ch]], probs = probs, na.rm = TRUE, names = FALSE)
        }))

        list(qres = qres, pixels_used = pixels_used)
    }

    quantile_rows <- lapply(resolutions, function(resolution) {
        res_info <- subset(img_info, resolutionLevel == resolution)
        res_pixels <- as.numeric(res_info$sizeX[[1]] * res_info$sizeY[[1]])

        message("collecting quantiles for resolution ", resolution)
        if (res_pixels <= max_full_fetch) {
            img_data <- RBioFormats::read.image(
                tiff_path,
                normalize = FALSE,
                series = 1,
                resolution = resolution
            )

            qres <- apply(
                img_data,
                MARGIN = 3,
                FUN = quantile,
                probs = probs,
                na.rm = TRUE,
                names = FALSE
            )
            source_mode <- "full"
            pixels_used <- res_pixels
        } else {
            if (is.null(sampled_rect)) {
                if (is.null(sample_rect_width) || is.null(sample_rect_height)) {
                    target_region_pixels <- max(64^2, floor(max_full_fetch / max(1, 3 * n_each)))
                    target_side <- max(64, floor(sqrt(target_region_pixels)))
                    if (is.null(sample_rect_width)) {
                        sample_rect_width <- min(full_w, target_side)
                    }
                    if (is.null(sample_rect_height)) {
                        sample_rect_height <- min(full_h, target_side)
                    }
                }

                sampled_rect <- select_informative_regions_lowres(
                    tiff_path = tiff_path,
                    rect_width = sample_rect_width,
                    rect_height = sample_rect_height,
                    n_each = n_each,
                    scout_resolution = scout_resolution,
                    summary_fun = sample_summary_fun,
                    channel = sample_channel,
                    seed = sample_seed,
                    output = "rect"
                )
            }

            sampled_res <- .sampled_quantiles(sampled_rect, resolution)
            qres <- sampled_res$qres
            source_mode <- "sampled"
            pixels_used <- sampled_res$pixels_used
        }

        if (is.null(dim(qres))) {
            qres <- matrix(qres, nrow = length(probs), ncol = 1)
        }

        do.call(rbind, lapply(seq_len(ncol(qres)), function(channel_idx) {
            data.frame(
                channel = channel_idx,
                resolution = resolution,
                prob = probs,
                value = qres[, channel_idx],
                source_mode = source_mode,
                pixels_used = pixels_used,
                row.names = NULL
            )
        }))
    })

    do.call(rbind, quantile_rows)
}


.resolve_precalc_agg <- function(fun, label) {
    if (is.function(fun)) {
        return(fun)
    }
    if (!is.character(fun) || length(fun) != 1L) {
        stop(label, " must be a function or one of: min, median, mean, max")
    }
    switch(
        fun,
        min = function(x) min(x, na.rm = TRUE),
        median = function(x) stats::median(x, na.rm = TRUE),
        mean = function(x) mean(x, na.rm = TRUE),
        max = function(x) max(x, na.rm = TRUE),
        stop(label, " must be one of: min, median, mean, max")
    )
}


#' Convert collected quantiles into a reusable precalc_max table
#'
#' Reduces the long-format output from
#' \code{collect_channel_quantiles_all_resolutions()} into the
#' \code{precalc_max} structure expected by \code{fetchTiffData()}.
#'
#' @param quantile_df Data frame produced by
#'   \code{collect_channel_quantiles_all_resolutions()}.
#' @param min_prob Quantile to use for \code{min_value}.
#' @param max_prob Quantile to use for \code{max_value}.
#' @param min_agg Aggregation function for per-resolution minima.
#' @param max_agg Aggregation function for per-resolution maxima.
#'
#' @returns A data.frame with columns \code{channel}, \code{min_value},
#'   \code{max_value}.
#' @export
quantiles_to_precalc_max <- function(quantile_df,
                                     min_prob = 0.90,
                                     max_prob = 0.998,
                                     min_agg = "min",
                                     max_agg = "max") {
    required_cols <- c("channel", "resolution", "prob", "value")
    if (!is.data.frame(quantile_df) || !all(required_cols %in% colnames(quantile_df))) {
        stop("quantile_df must be a data.frame with columns: channel, resolution, prob, value")
    }
    if (!is.numeric(min_prob) || length(min_prob) != 1L || !is.finite(min_prob) || min_prob < 0 || min_prob > 1) {
        stop("min_prob must be a single numeric value in [0, 1]")
    }
    if (!is.numeric(max_prob) || length(max_prob) != 1L || !is.finite(max_prob) || max_prob < 0 || max_prob > 1) {
        stop("max_prob must be a single numeric value in [0, 1]")
    }
    if (min_prob >= max_prob) {
        stop("min_prob must be smaller than max_prob")
    }

    if (!any(quantile_df$prob == min_prob)) {
        stop("min_prob is not present in quantile_df$prob")
    }
    if (!any(quantile_df$prob == max_prob)) {
        stop("max_prob is not present in quantile_df$prob")
    }

    min_agg_fun <- .resolve_precalc_agg(min_agg, "min_agg")
    max_agg_fun <- .resolve_precalc_agg(max_agg, "max_agg")

    min_df <- subset(quantile_df, prob == min_prob, select = c("channel", "resolution", "value"))
    max_df <- subset(quantile_df, prob == max_prob, select = c("channel", "resolution", "value"))
    names(min_df)[names(min_df) == "value"] <- "min_value"
    names(max_df)[names(max_df) == "value"] <- "max_value"

    merged_df <- merge(min_df, max_df, by = c("channel", "resolution"), all = FALSE)

    merged_df %>%
        dplyr::group_by(channel) %>%
        dplyr::summarise(
            min_value = min_agg_fun(min_value),
            max_value = max_agg_fun(max_value)
        ) %>%
        as.data.frame()
}


#' Precompute normalization ranges across all TIFF resolutions
#'
#' Convenience wrapper around \code{collect_channel_quantiles_all_resolutions()}
#' and \code{quantiles_to_precalc_max()}.
#'
#' @param tiff_path Path to the TIFF image file.
#' @param min_prob Quantile used for \code{min_value} (default \code{0.90}).
#' @param max_prob Quantile used for \code{max_value} (default \code{0.998}).
#' @param min_agg Aggregation function for per-resolution minima.
#' @param max_agg Aggregation function for per-resolution maxima.
#' @param probs Numeric vector of quantiles to compute up front. Must include
#'   \code{min_prob} and \code{max_prob}.
#' @param max_full_fetch Maximum number of pixels to read fully per resolution.
#' @param n_each Number of sampled regions per intensity band when sampling.
#' @param sample_rect_width Optional sampled region width in full-resolution
#'   pixels.
#' @param sample_rect_height Optional sampled region height in full-resolution
#'   pixels.
#' @param scout_resolution Optional resolution level used to choose informative
#'   regions.
#' @param sample_summary_fun Summary statistic used to score candidate regions:
#'   one of \code{"mean"}, \code{"median"}, \code{"max"}.
#' @param sample_channel Optional channel index used for region scoring.
#' @param sample_seed Optional seed for reproducible sampled region selection.
#' @param return_by_resolution If TRUE, return collected quantiles alongside
#'   aggregated output.
#'
#' @returns By default, a data.frame with columns \code{channel},
#'   \code{min_value}, \code{max_value}. If \code{return_by_resolution = TRUE},
#'   returns a list with elements \code{precalc_max} and \code{quantiles}.
#' @export
precalc_max_all_resolutions <- function(
        tiff_path,
        min_prob = 0.90,
        max_prob = 0.998,
        min_agg = "min",
        max_agg = "max",
        probs = sort(unique(c(seq(0, 100) / 100, 0.99 + seq(1, 9) / 1000, min_prob, max_prob))),
        max_full_fetch = 5e6,
        n_each = 10,
        sample_rect_width = NULL,
        sample_rect_height = NULL,
        scout_resolution = NULL,
        sample_summary_fun = c("mean", "median", "max"),
        sample_channel = NULL,
        sample_seed = NULL,
        return_by_resolution = FALSE
) {
    sample_summary_fun <- match.arg(sample_summary_fun)

    quantile_df <- collect_channel_quantiles_all_resolutions(
        tiff_path = tiff_path,
        probs = probs,
        max_full_fetch = max_full_fetch,
        n_each = n_each,
        sample_rect_width = sample_rect_width,
        sample_rect_height = sample_rect_height,
        scout_resolution = scout_resolution,
        sample_summary_fun = sample_summary_fun,
        sample_channel = sample_channel,
        sample_seed = sample_seed
    )

    precalc_max <- quantiles_to_precalc_max(
        quantile_df = quantile_df,
        min_prob = min_prob,
        max_prob = max_prob,
        min_agg = min_agg,
        max_agg = max_agg
    )

    if (isTRUE(return_by_resolution)) {
        return(list(precalc_max = precalc_max, quantiles = quantile_df))
    }
    precalc_max
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

