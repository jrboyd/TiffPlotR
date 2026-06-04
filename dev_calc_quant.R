
#' Select informative regions using a low-resolution scout pass
#'
#' Uses a single low-resolution image read to score a regular grid of candidate
#' regions, then selects high-, middle-, and low-intensity rectangles to fetch
#' later at full resolution.
#'
#' @param tiff_path Path to the TIFF image file.
#' @param rect_width Candidate region width in full-resolution pixels.
#' @param rect_height Candidate region height in full-resolution pixels.
#' @param n_each Number of regions to select for each intensity group
#'   (high/mid/low).
#' @param scout_resolution Resolution level used for scouting. If NULL,
#'   uses the lowest-resolution pyramid level (largest \code{resolutionLevel}).
#' @param summary_fun Summary statistic for region scoring: one of
#'   \code{"mean"}, \code{"median"}, \code{"max"}.
#' @param channel Optional channel index to score. If NULL, scores with the
#'   mean across channels.
#' @param output Either \code{"rect"} to return only \linkS4class{TiffRect}, or
#'   \code{"list"} to return both rectangles and a scoring table.
#'
#' @returns
#' If \code{output = "rect"}, returns a \linkS4class{TiffRect} containing the
#' selected regions.
#'
#' If \code{output = "list"}, returns a list with elements:
#' \itemize{
#'   \item \code{rect}: selected \linkS4class{TiffRect}
#'   \item \code{scores}: data.frame with per-tile scores and group labels
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' sel <- select_informative_regions_lowres(exampleTiff(), rect_width = 400, rect_height = 400)
#' obj <- fetchTiffData(exampleTiff(), rect = sel[1])
#' }
select_informative_regions_lowres <- function(tiff_path,
                                              rect_width,
                                              rect_height,
                                              n_each = 10,
                                              scout_resolution,
                                              summary_fun = c("mean", "median", "max"),
                                              channel = NULL,
                                              output = c("rect", "list")) {
    summary_fun <- match.arg(summary_fun)
    output <- match.arg(output)

    if (!is.numeric(rect_width) || length(rect_width) != 1L || !is.finite(rect_width) || rect_width <= 0) {
        stop("rect_width must be a single positive numeric value")
    }
    if (!is.numeric(rect_height) || length(rect_height) != 1L || !is.finite(rect_height) || rect_height <= 0) {
        stop("rect_height must be a single positive numeric value")
    }
    if (!is.numeric(n_each) || length(n_each) != 1L || !is.finite(n_each) || n_each < 1) {
        stop("n_each must be a single integer >= 1")
    }
    n_each <- as.integer(n_each)

    img_info <- read_tiff_meta_data(tiff_path)
    res_info <- subset(img_info, resolutionLevel == scout_resolution)

    if (is.null(scout_resolution)) {
        scout_resolution <- max(img_info$resolutionLevel)
    }
    scout_info <- subset(img_info, resolutionLevel == scout_resolution)
    if (nrow(scout_info) != 1) {
        stop("Could not identify exactly one scout resolution in metadata")
    }

    full_w <- as.numeric(res_info$sizeX[[1]])
    full_h <- as.numeric(res_info$sizeY[[1]])

    nx <- floor(full_w / rect_width)
    ny <- floor(full_h / rect_height)
    if (nx < 1 || ny < 1) {
        stop("rect_width/rect_height are too large for this image")
    }

    scout_arr <- fetchTiffArray(
        tiff_path = tiff_path,
        rect = TiffRect(1, full_w, 1, full_h),
        resolution = scout_resolution
    )

    scout_dim <- dim(scout_arr)
    if (length(scout_dim) < 2) {
        stop("Unexpected scout image dimensions")
    }

    if (is.null(channel)) {
        if (length(scout_dim) >= 3) {
            scout_img <- apply(scout_arr, c(1, 2), mean, na.rm = TRUE)
        } else {
            scout_img <- scout_arr
        }
    } else {
        if (!is.numeric(channel) || length(channel) != 1L || !is.finite(channel) || channel < 1) {
            stop("channel must be a single positive integer when provided")
        }
        channel <- as.integer(channel)
        if (length(scout_dim) < 3 || channel > scout_dim[[3]]) {
            stop("channel index is out of range for scout image")
        }
        scout_img <- scout_arr[, , channel]
    }

    x_scale <- as.numeric(scout_info$sizeX[[1]]) / full_w
    y_scale <- as.numeric(scout_info$sizeY[[1]]) / full_h

    score_fun <- switch(
        summary_fun,
        mean = function(x) mean(x, na.rm = TRUE),
        median = function(x) stats::median(x, na.rm = TRUE),
        max = function(x) max(x, na.rm = TRUE)
    )

    tile_rows <- vector("list", nx * ny)
    idx <- 1L
    for (ix in seq_len(nx)) {
        xmin <- (ix - 1) * rect_width + 1
        xmax <- ix * rect_width
        for (iy in seq_len(ny)) {
            ymin <- (iy - 1) * rect_height + 1
            ymax <- iy * rect_height

            sx1 <- max(1, floor((xmin - 1) * x_scale) + 1)
            sx2 <- min(ncol(scout_img), ceiling(xmax * x_scale))
            sy1 <- max(1, floor((ymin - 1) * y_scale) + 1)
            sy2 <- min(nrow(scout_img), ceiling(ymax * y_scale))

            if(sx1 == 376){
                browser()
            }
            message(paste(sx1, sx2, sy1, sy2))

            tile <- scout_img[sy1:sy2, sx1:sx2, drop = FALSE]
            score <- score_fun(as.numeric(tile))

            tile_rows[[idx]] <- data.frame(
                xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax,
                score = score,
                stringsAsFactors = FALSE
            )
            idx <- idx + 1L
        }
    }

    score_df <- do.call(rbind, tile_rows)
    score_df <- score_df[is.finite(score_df$score), , drop = FALSE]
    if (nrow(score_df) < 3) {
        stop("Not enough finite-scored regions were found")
    }

    .pick_unique <- function(candidates, n, used_idx) {
        candidates <- setdiff(candidates, used_idx)
        if (length(candidates) == 0) {
            return(integer(0))
        }
        n_take <- min(n, length(candidates))
        candidates[seq(n_take)]
        # sample(candidates, size = n_take, replace = FALSE)
    }

    ord <- order(score_df$score)
    low_cand <- ord
    high_cand <- rev(ord)
    med_score <- stats::median(score_df$score, na.rm = TRUE)
    mid_cand <- order(abs(score_df$score - med_score))

    used <- integer(0)
    low_idx <- .pick_unique(low_cand, n_each, used)
    used <- c(used, low_idx)
    mid_idx <- .pick_unique(mid_cand, n_each, used)
    used <- c(used, mid_idx)
    high_idx <- .pick_unique(high_cand, n_each, used)

    keep_idx <- c(low_idx, mid_idx, high_idx)
    keep_group <- c(rep("low", length(low_idx)), rep("mid", length(mid_idx)), rep("high", length(high_idx)))

    picked <- score_df[keep_idx, , drop = FALSE]
    picked$group <- keep_group
    picked$name <- paste0(picked$group, "_", ave(seq_len(nrow(picked)), picked$group, FUN = seq_along))

    rect <- TiffRect(
        xmin = picked$xmin,
        xmax = picked$xmax,
        ymin = picked$ymin,
        ymax = picked$ymax,
        name = picked$name
    )

    if (identical(output, "list")) {
        return(list(rect = rect, scores = picked))
    }
    rect
}

.calculate_full_image_quantiles = function(tiff_path, resolution, probs){
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
}

.calculate_representative_image_quantiles = function(tiff_path, resolution, probs){

}

collect_channel_quantiles_all_resolutions <- function(
        tiff_path,
        probs = sort(c(seq(0, 100) / 100, 0.99 + seq(1, 9) / 1000)),
        max_full_fetch = 5e6,
        n_each = 10,
        sample_rect_width = NULL,
        sample_rect_height = NULL,
        scout_resolution = NULL,
        sample_summary_fun = c("mean", "median", "max"),
        sample_channel = NULL
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
            .calculate_full_image_quantiles(tiff_path = tiff_path, resolution = resolution, probs = probs)
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

tiff_path = exampleTiff()
read_tiff_meta_data(tiff_path)

# select_informative_regions_lowres(tiff_path, rect_width = 40, rect_height = 40, scout_resolution = 3)
debug(select_informative_regions_lowres)
select_informative_regions_lowres(tiff_path, rect_width = 40, rect_height = 40, scout_resolution = 5)
