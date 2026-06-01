#' Fetch OME ROI annotations from a TIFF
#'
#' Reads OME-XML metadata and extracts ROI-linked shape annotations
#' (labels, polygons, ellipses, and masks). This is a package-safe version
#' of the exploratory workflow in `dev_polygons.R`.
#'
#' @param tiff_path Path to the TIFF image file. Required when `ome_doc` is NULL.
#' @param ome_doc Optional xml2 document (as returned by \code{xml2::read_xml}).
#'   Supplying this skips OME-XML loading from disk.
#' @param decode_masks If TRUE, decode mask bitmaps to point coordinates.
#' @param bit_order Bit order to use when decoding mask payloads.
#' @param include_summary If TRUE, include a node-type summary table.
#'
#' @returns A named list with elements:
#' \itemize{
#'   \item \code{summary}: node-type summary (or NULL when disabled)
#'   \item \code{roi}: ROI table with \code{ROI_ID}
#'   \item \code{labels}: ROI label rows
#'   \item \code{polygons}: ROI polygon rows
#'   \item \code{ellipses}: ROI ellipse rows
#'   \item \code{masks}: ROI mask rows
#'   \item \code{mask_points}: decoded mask points (or empty data.frame)
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' ann <- fetchTiffAnnotations(exampleTiff())
#' names(ann)
#' }
fetchTiffAnnotations <- function(tiff_path = NULL,
                                 ome_doc = NULL,
                                 decode_masks = FALSE,
                                 bit_order = c("msb", "lsb"),
                                 include_summary = TRUE) {
    bit_order <- match.arg(bit_order)

    if (is.null(ome_doc)) {
        if (is.null(tiff_path) || !is.character(tiff_path) || length(tiff_path) != 1) {
            stop("Provide a single tiff_path when ome_doc is NULL.", call. = FALSE)
        }
        ome_doc <- .read_tiff_ome_xml(tiff_path)
    }

    if (!inherits(ome_doc, "xml_document")) {
        stop("ome_doc must be an xml2 document.", call. = FALSE)
    }

    roi_nodes <- .fetch_ome_nodes(ome_doc, "ROI", output = "nodes")
    roi_tbl <- .fetch_ome_nodes(ome_doc, "ROI", output = "data.frame")
    roi_tbl$ROI_ID <- xml2::xml_attr(roi_nodes, "ID")

    labels <- .extract_roi_type(roi_nodes, "Label")
    polygons <- .extract_roi_type(roi_nodes, "Polygon")
    ellipses <- .extract_roi_type(roi_nodes, "Ellipse")
    masks <- .extract_roi_type(roi_nodes, "Mask")

    mask_points <- data.frame()
    if (isTRUE(decode_masks) && nrow(masks) > 0) {
        required_cols <- c("node_text", "Width", "Height", "X", "Y")
        missing_cols <- setdiff(required_cols, colnames(masks))
        if (length(missing_cols) > 0) {
            stop(
                paste0("Mask annotations are missing required columns: ", paste(missing_cols, collapse = ", ")),
                call. = FALSE
            )
        }
        keep_cols <- setdiff(colnames(masks), required_cols)
        mask_points <- decode_ome_masks(
            masks[, c(required_cols, keep_cols), drop = FALSE],
            bit_order = bit_order,
            output = "points"
        )
    }

    list(
        summary = if (isTRUE(include_summary)) .summarize_ome_nodes(ome_doc) else NULL,
        roi = roi_tbl,
        labels = labels,
        polygons = polygons,
        ellipses = ellipses,
        masks = masks,
        mask_points = mask_points
    )
}


#' Decode OME mask annotations
#'
#' Decodes base64-encoded OME \code{Mask} annotation payloads into either
#' binary matrices or sparse point coordinates.
#'
#' @param mask_df Data frame containing at least \code{node_text}, \code{Width},
#'   \code{Height}, \code{X}, and \code{Y}.
#' @param bit_order Bit order to use when interpreting packed bits.
#' @param output Either \code{"points"} or \code{"matrix"}.
#'
#' @returns
#' When \code{output = "matrix"}, returns \code{mask_df} with an added
#' \code{mask_matrix} list-column.
#'
#' When \code{output = "points"}, returns a data.frame with point coordinates
#' and any metadata columns carried through from \code{mask_df}.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   node_text = "gA==",
#'   Width = 2,
#'   Height = 2,
#'   X = 10,
#'   Y = 20,
#'   stringsAsFactors = FALSE
#' )
#' decode_ome_masks(df, output = "points")
decode_ome_masks <- function(mask_df,
                             bit_order = c("msb", "lsb"),
                             output = c("points", "matrix")) {
    bit_order <- match.arg(bit_order)
    output <- match.arg(output)

    required_cols <- c("node_text", "Width", "Height", "X", "Y")
    missing_cols <- setdiff(required_cols, colnames(mask_df))
    if (length(missing_cols) > 0) {
        stop(
            paste0("mask_df is missing required columns: ", paste(missing_cols, collapse = ", ")),
            call. = FALSE
        )
    }

    if (identical(output, "matrix")) {
        out <- mask_df
        out$mask_matrix <- lapply(seq_len(nrow(mask_df)), function(i) {
            .decode_ome_mask(mask_df[i, , drop = FALSE], bit_order = bit_order)
        })
        return(out)
    }

    keep_cols <- setdiff(colnames(mask_df), required_cols)
    point_res <- lapply(seq_len(nrow(mask_df)), function(i) {
        row_df <- mask_df[i, , drop = FALSE]
        pts <- .mask_to_df(row_df, bit_order = bit_order)
        if (nrow(pts) == 0) {
            return(NULL)
        }

        row_meta <- row_df[rep(1, nrow(pts)), keep_cols, drop = FALSE]
        cbind(row_meta, pts)
    })

    dplyr::bind_rows(point_res)
}


.read_tiff_ome_xml <- function(tiff_path) {
    txml <- RBioFormats::read.omexml(tiff_path)
    ome_xml <- paste(txml, collapse = "")

    xml2::read_xml(
        charToRaw(enc2utf8(ome_xml)),
        encoding = "UTF-8",
        options = c("RECOVER", "HUGE", "NOBLANKS")
    )
}


.fetch_ome_nodes <- function(doc, node_types, output = c("nodes", "data.frame")) {
    if (length(node_types) == 0 || anyNA(node_types)) {
        stop("node_types must contain at least one non-missing type name", call. = FALSE)
    }

    output <- match.arg(output)
    node_types <- unique(as.character(node_types))
    xpath_terms <- paste0("local-name()='", node_types, "'")
    xpath_query <- paste0(".//*[", paste(xpath_terms, collapse = " or "), "]")
    nodes <- xml2::xml_find_all(doc, xpath_query)

    if (identical(output, "nodes")) {
        return(nodes)
    }

    tibble::tibble(
        node_type = sub("^.*:", "", xml2::xml_name(nodes)),
        node_text = xml2::xml_text(nodes, trim = TRUE),
        attrs = lapply(nodes, xml2::xml_attrs)
    ) %>%
        tidyr::unnest_wider(attrs, names_repair = "universal")
}


.extract_roi_type <- function(roi_nodes, node_type) {
    if (length(roi_nodes) == 0) {
        return(data.frame())
    }

    roi_ids <- xml2::xml_attr(roi_nodes, "ID")
    res <- lapply(seq_along(roi_nodes), function(i) {
        node_tbl <- .fetch_ome_nodes(roi_nodes[[i]], node_type, output = "data.frame")
        if (nrow(node_tbl) == 0) {
            return(NULL)
        }
        node_tbl$ROI_ID <- roi_ids[[i]]
        node_tbl
    })

    dplyr::bind_rows(res)
}


.summarize_ome_nodes <- function(doc) {
    all_nodes <- xml2::xml_find_all(doc, ".//*")
    all_names <- sub("^.*:", "", xml2::xml_name(all_nodes))
    total_nodes <- length(all_nodes)

    target_types <- c(
        "OME", "Image", "Pixels", "Channel", "TiffData",
        "Plane", "StructuredAnnotations", "MapAnnotation",
        "ROI", "Union", "Shape", "Polygon", "Rectangle",
        "Ellipse", "Line", "Polyline", "Label", "Mask"
    )
    all_types <- sort(unique(c(target_types, all_names)))

    unexpected_types <- setdiff(unique(all_names), target_types)
    if (length(unexpected_types) > 0) {
        unexpected_counts <- sort(table(all_names[all_names %in% unexpected_types]), decreasing = TRUE)
        unexpected_msg <- paste0(
            names(unexpected_counts), " (", as.integer(unexpected_counts), ")",
            collapse = ", "
        )
        warning(
            paste0("Unexpected OME-XML node types detected: ", unexpected_msg),
            call. = FALSE
        )
    }

    tibble::tibble(node_type = all_types) %>%
        dplyr::mutate(
            is_standard = node_type %in% target_types,
            n = vapply(
                node_type,
                FUN = function(tp) {
                    sum(all_names == tp)
                },
                FUN.VALUE = integer(1)
            ),
            pct_all_nodes = if (total_nodes > 0) round(100 * n / total_nodes, 3) else 0
        ) %>%
        dplyr::arrange(dplyr::desc(n), dplyr::desc(is_standard), node_type)
}


.decode_ome_mask <- function(mask_row, bit_order = c("msb", "lsb")) {
    bit_order <- match.arg(bit_order)

    raw_mask <- base64enc::base64decode(mask_row$node_text[[1]])

    width <- as.integer(mask_row$Width[[1]])
    height <- as.integer(mask_row$Height[[1]])
    n_pixels <- width * height

    bits <- as.integer(rawToBits(raw_mask))

    # rawToBits returns least-significant-bit first within each byte.
    if (identical(bit_order, "msb")) {
        bits <- matrix(bits, ncol = 8, byrow = TRUE)
        bits <- as.vector(t(bits[, 8:1, drop = FALSE]))
    }

    bits <- bits[seq_len(n_pixels)]

    matrix(bits, nrow = height, ncol = width, byrow = TRUE)
}


.mask_to_df <- function(mask_row, bit_order = c("msb", "lsb")) {
    bit_order <- match.arg(bit_order)
    mask_mat <- .decode_ome_mask(mask_row, bit_order = bit_order)

    on_idx <- which(mask_mat == 1L, arr.ind = TRUE)
    if (nrow(on_idx) == 0) {
        return(data.frame())
    }

    data.frame(
        x = as.numeric(mask_row$X[[1]]) + on_idx[, "col"] - 1,
        y = as.numeric(mask_row$Y[[1]]) + on_idx[, "row"] - 1
    )
}