#' Read ImageJ/FIJI ROI files or ROI zip exports
#'
#' Loads ImageJ/FIJI `.roi` files directly, including `.zip` exports that
#' contain multiple ROI files, and converts supported shapes into TiffPlotR
#' shape objects.
#'
#' Supported ROI types are rectangles, ovals, and polygon-like ROIs
#' (`polygon`, `freehand`, and `traced`). Unsupported ROI types are reported
#' in the returned `unsupported` table.
#'
#' @param path Path(s) to one or more `.roi` files and/or `.zip` archives
#'   exported from ImageJ/FIJI.
#'
#' @returns A named list with elements:
#' \itemize{
#'   \item \code{roi_info}: data frame describing every parsed ROI entry.
#'   \item \code{rects}: combined \linkS4class{TiffRect} object or `NULL`.
#'   \item \code{ellipses}: combined \linkS4class{TiffEllipse} object or `NULL`.
#'   \item \code{polygons}: combined \linkS4class{TiffPolygon} object or `NULL`.
#'   \item \code{unsupported}: data frame of ROI files that could not be mapped.
#' }
#' @export
#'
#' @examples
#' roi_files = dir(system.file(package = "TiffPlotR", "extdata"), pattern = "roi$", full.names = TRUE)
#' roi <- readImageJRois(roi_files)
#'
#' roi_zip_file = system.file(package = "TiffPlotR", "extdata/TomatoRed_CD45_PanCK_3124_TMA_rects.zip", mustWork = TRUE)
#' all_roi <- readImageJRois(roi_zip_file)
readImageJRois <- function(path) {
    if (!is.character(path) || length(path) < 1 || anyNA(path) || any(!nzchar(path))) {
        stop("path must be a non-empty character vector of .roi and/or .zip file paths.", call. = FALSE)
    }

    missing_paths <- path[!file.exists(path)]
    if (length(missing_paths) > 0) {
        stop("path does not exist: ", paste(missing_paths, collapse = ", "), call. = FALSE)
    }

    source_info <- .collect_imagej_roi_sources(path)
    roi_sources <- source_info$roi_sources

    cleanup_dirs <- unique(source_info$cleanup_dirs)
    cleanup_dirs <- cleanup_dirs[nzchar(cleanup_dirs)]
    if (length(cleanup_dirs) > 0) {
        on.exit(lapply(cleanup_dirs, unlink, recursive = TRUE, force = TRUE), add = TRUE)
    }

    parsed <- lapply(seq_len(nrow(roi_sources)), function(i) {
        .read_imagej_roi_file(roi_sources$file_path[[i]], roi_sources$source_name[[i]])
    })

    roi_info <- dplyr::bind_rows(lapply(parsed, function(x) x$info))
    unsupported <- dplyr::bind_rows(lapply(parsed, function(x) x$unsupported))

    rect_parts <- Filter(function(x) !is.null(x) && is(x, "TiffRect"), lapply(parsed, function(x) x$shape))
    ellipse_parts <- Filter(function(x) !is.null(x) && is(x, "TiffEllipse"), lapply(parsed, function(x) x$shape))
    polygon_parts <- Filter(function(x) !is.null(x) && is(x, "TiffPolygon"), lapply(parsed, function(x) x$shape))

    list(
        roi_info = roi_info,
        rects = .combine_imagej_shapes(rect_parts),
        ellipses = .combine_imagej_shapes(ellipse_parts),
        polygons = .combine_imagej_shapes(polygon_parts),
        unsupported = unsupported
    )
}


.combine_imagej_shapes <- function(parts) {
    if (length(parts) == 0) {
        return(NULL)
    }
    if (length(parts) == 1) {
        return(parts[[1]])
    }

    do.call(c, parts)
}


.collect_imagej_roi_sources <- function(path) {
    source_rows <- list()
    cleanup_dirs <- character()

    for (p in path) {
        ext <- tolower(tools::file_ext(p))

        if (identical(ext, "roi")) {
            source_rows[[length(source_rows) + 1]] <- data.frame(
                file_path = p,
                source_name = basename(p),
                stringsAsFactors = FALSE
            )
            next
        }

        if (!identical(ext, "zip")) {
            stop("All paths must end in .roi or .zip. Invalid path: ", p, call. = FALSE)
        }

        zip_listing <- utils::unzip(p, list = TRUE)
        keep <- grepl("\\.roi$", zip_listing$Name, ignore.case = TRUE)
        if (!any(keep)) {
            stop("No .roi files found inside zip archive: ", p, call. = FALSE)
        }

        exdir <- tempfile("imagej-roi-")
        dir.create(exdir)

        target_names <- zip_listing$Name[keep]
        utils::unzip(p, files = target_names, exdir = exdir)

        source_rows[[length(source_rows) + 1]] <- data.frame(
            file_path = file.path(exdir, target_names),
            source_name = target_names,
            stringsAsFactors = FALSE
        )
        cleanup_dirs <- c(cleanup_dirs, exdir)
    }

    list(
        roi_sources = dplyr::bind_rows(source_rows),
        cleanup_dirs = cleanup_dirs
    )
}


.read_imagej_roi_file <- function(file_path, source_name = basename(file_path)) {
    file_size <- file.info(file_path)$size
    raw_roi <- readBin(file_path, what = "raw", n = file_size)
    .parse_imagej_roi(raw_roi, source_name = source_name)
}


.parse_imagej_roi <- function(raw_roi, source_name) {
    if (length(raw_roi) < 64) {
        stop("ImageJ ROI file is too short to contain a valid header.", call. = FALSE)
    }

    if (!identical(rawToChar(raw_roi[1:4]), "Iout")) {
        stop("Invalid ImageJ ROI file: missing Iout magic header.", call. = FALSE)
    }

    version <- .imagej_read_uint16(raw_roi, 5)
    roi_type_code <- as.integer(raw_roi[[7]])
    top <- .imagej_read_int16(raw_roi, 9)
    left <- .imagej_read_int16(raw_roi, 11)
    bottom <- .imagej_read_int16(raw_roi, 13)
    right <- .imagej_read_int16(raw_roi, 15)
    n_coords <- .imagej_read_uint16(raw_roi, 17)
    subtype <- .imagej_read_uint16(raw_roi, 49)
    options <- .imagej_read_uint16(raw_roi, 51)
    roi_name <- tools::file_path_sans_ext(basename(source_name))
    roi_type <- .imagej_roi_type_label(roi_type_code)

    info <- data.frame(
        name = roi_name,
        source_name = source_name,
        roi_type = roi_type,
        roi_type_code = roi_type_code,
        version = version,
        subtype = subtype,
        n_coordinates = n_coords,
        supported = FALSE,
        stringsAsFactors = FALSE
    )

    unsupported <- data.frame()
    shape <- NULL

    if (identical(roi_type, "rect")) {
        info$supported <- TRUE
        shape <- TiffRect(
            xmin = left,
            xmax = right,
            ymin = top,
            ymax = bottom,
            name = roi_name
        )
    } else if (identical(roi_type, "oval")) {
        info$supported <- TRUE
        shape <- TiffEllipse(
            x0 = (left + right) / 2,
            y0 = (top + bottom) / 2,
            radius_x = (right - left) / 2,
            radius_y = (bottom - top) / 2,
            name = roi_name
        )
    } else if (roi_type %in% c("polygon", "freehand", "traced")) {
        polygon_coords <- .imagej_parse_polygon_coords(
            raw_roi = raw_roi,
            left = left,
            top = top,
            n_coords = n_coords,
            options = options
        )

        if (length(polygon_coords$x) < 3) {
            unsupported <- data.frame(
                name = roi_name,
                source_name = source_name,
                roi_type = roi_type,
                reason = "Polygon ROI has fewer than 3 vertices.",
                stringsAsFactors = FALSE
            )
        } else {
            info$supported <- TRUE
            shape <- TiffPolygon(
                x = polygon_coords$x,
                y = polygon_coords$y,
                name = roi_name
            )
        }
    } else {
        unsupported <- data.frame(
            name = roi_name,
            source_name = source_name,
            roi_type = roi_type,
            reason = "ROI type is not currently supported.",
            stringsAsFactors = FALSE
        )
    }

    list(info = info, shape = shape, unsupported = unsupported)
}


.imagej_parse_polygon_coords <- function(raw_roi, left, top, n_coords, options) {
    if (n_coords < 1) {
        return(list(x = numeric(), y = numeric()))
    }

    x_offset <- 65
    y_offset <- x_offset + 2 * n_coords
    x_rel <- .imagej_read_uint16_vec(raw_roi, x_offset, n_coords)
    y_rel <- .imagej_read_uint16_vec(raw_roi, y_offset, n_coords)

    has_subpixel <- bitwAnd(as.integer(options), 128L) != 0L
    subpixel_offset <- y_offset + 2 * n_coords

    if (isTRUE(has_subpixel) && length(raw_roi) >= subpixel_offset + 8 * n_coords - 1) {
        x_vals <- .imagej_read_float32_vec(raw_roi, subpixel_offset, n_coords)
        y_vals <- .imagej_read_float32_vec(raw_roi, subpixel_offset + 4 * n_coords, n_coords)
        return(list(x = x_vals, y = y_vals))
    }

    list(
        x = left + x_rel,
        y = top + y_rel
    )
}


.imagej_roi_type_label <- function(type_code) {
    labels <- c(
        "polygon", "rect", "oval", "line", "freeline",
        "polyline", "no_roi", "freehand", "traced", "angle", "point"
    )

    if (is.na(type_code) || type_code < 0 || type_code >= length(labels)) {
        return("unknown")
    }

    labels[[type_code + 1]]
}


.imagej_read_uint16 <- function(raw_vec, offset) {
    bytes <- as.integer(raw_vec[offset:(offset + 1)])
    bytes[[1]] * 256L + bytes[[2]]
}


.imagej_read_int16 <- function(raw_vec, offset) {
    val <- .imagej_read_uint16(raw_vec, offset)
    if (val >= 32768L) {
        val <- val - 65536L
    }
    val
}


.imagej_read_uint16_vec <- function(raw_vec, offset, n) {
    vapply(seq_len(n), function(i) {
        .imagej_read_uint16(raw_vec, offset + 2L * (i - 1L))
    }, numeric(1))
}


.imagej_read_float32_vec <- function(raw_vec, offset, n) {
    end <- offset + 4L * n - 1L
    con <- rawConnection(raw_vec[offset:end], open = "rb")
    on.exit(close(con), add = TRUE)
    readBin(con, what = "numeric", n = n, size = 4, endian = "big")
}
