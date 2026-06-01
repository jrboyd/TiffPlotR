
library(RBioFormats)
# Sys.getenv("LD_LIBRARY_PATH")
# Sys.setenv(LD_LIBRARY_PATHS ="/usr/lib/R/lib:/usr/lib/x86_64-linux-gnu:/usr/lib/jvm/java-11-openjdk-amd64/lib/server")
# Sys.getenv("LD_LIBRARY_PATH")
# Sys.setenv(RETICULATE_PYTHON = "/usr/local/bin/python3")
# system("echo $LD_LIBRARY_PATH")
library(tidyverse)
tiff_files = dir("/mnt/c/project_data/UVMMIC-RA-13113/FinalScansAndROIs/", pattern = "tiff$", full.names = TRUE)
names(tiff_files) = basename(tiff_files)
tiff_files = as.list(tiff_files)

tf = tiff_files$`31'24.ome.tiff`

tiff_meta = RBioFormats::read.metadata(tf)

tiff_meta %>% class

tiff_meta@.Data[[1]]$coreMetadata
tiff_meta@.Data[[8]]$coreMetadata

RBioFormats::coreMetadata(tiff_meta)
RBioFormats::globalMetadata(tiff_meta)
RBioFormats::seriesMetadata(tiff_meta)


txml = RBioFormats::read.omexml(tf)
txml %>% class
length(txml)
nchar(txml)

library(xml2)
?xml2::read_xml

# OME-XML can be very large; parse with libxml2 "HUGE" mode enabled.
ome_xml = paste(txml, collapse = "")
ome_doc = xml2::read_xml(
	charToRaw(enc2utf8(ome_xml)),
	encoding = "UTF-8",
	options = c("RECOVER", "HUGE", "NOBLANKS")
)

get_ome_nodes = function(doc, node_types, return = c("nodes", "tibble")) {
	# stopifnot(inherits(doc, "xml_document"))
	if (length(node_types) == 0 || anyNA(node_types)) {
		stop("node_types must contain at least one non-missing type name", call. = FALSE)
	}

	return = match.arg(return)
	node_types = unique(as.character(node_types))
	xpath_terms = paste0("local-name()='", node_types, "'")
	xpath_query = paste0(".//*[", paste(xpath_terms, collapse = " or "), "]")
	nodes = xml2::xml_find_all(doc, xpath_query)

	if (identical(return, "nodes")) {
		return(nodes)
	}

	tibble::tibble(
		node_type = sub("^.*:", "", xml2::xml_name(nodes)),
		node_text = xml2::xml_text(nodes, trim = TRUE),
		attrs = lapply(nodes, xml2::xml_attrs)
	) %>%
		tidyr::unnest_wider(attrs, names_repair = "universal")
}

# Namespace-agnostic query for Polygon annotations.
polygon_nodes = get_ome_nodes(ome_doc, "Polygon", return = "nodes")
length(polygon_nodes)

polygon_tbl = tibble(
	id = xml2::xml_attr(polygon_nodes, "ID"),
	points = xml2::xml_attr(polygon_nodes, "Points"),
	text = xml2::xml_attr(polygon_nodes, "Text"),
	the_z = suppressWarnings(as.integer(xml2::xml_attr(polygon_nodes, "TheZ"))),
	the_t = suppressWarnings(as.integer(xml2::xml_attr(polygon_nodes, "TheT"))),
	stroke_color = xml2::xml_attr(polygon_nodes, "StrokeColor"),
	fill_color = xml2::xml_attr(polygon_nodes, "FillColor")
)

polygon_tbl

summarize_ome_nodes = function(doc) {
	stopifnot(inherits(doc, "xml_document"))

	all_nodes = xml2::xml_find_all(doc, ".//*")
	all_names = sub("^.*:", "", xml2::xml_name(all_nodes))
	total_nodes = length(all_nodes)

	target_types = c(
		"OME", "Image", "Pixels", "Channel", "TiffData",
		"Plane", "StructuredAnnotations", "MapAnnotation",
		"ROI", "Union", "Shape", "Polygon", "Rectangle",
		"Ellipse", "Line", "Polyline", "Label", "Mask"
	)
	all_types = sort(unique(c(target_types, all_names)))

	unexpected_types = setdiff(unique(all_names), target_types)
	if (length(unexpected_types) > 0) {
		unexpected_counts = sort(table(all_names[all_names %in% unexpected_types]), decreasing = TRUE)
		unexpected_msg = paste0(
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

ome_summary_tbl = summarize_ome_nodes(ome_doc)
ome_summary_tbl %>% filter(n > 0) %>% pull(node_type)


roi_nodes = get_ome_nodes(ome_doc, "ROI", return = "nodes")
roi_ids = xml2::xml_attr(roi_nodes, "ID")
roi_nodes %>% class
roi_nodes %>% as.list
roi_nodes[1:3]
roi_nodes %>% get_ome_nodes("ID")
poly_res = lapply(roi_nodes, function(n){
    get_ome_nodes(n, "Polygon", return = "tibble")
})
names(poly_res) = roi_ids
has_poly = sapply(poly_res, nrow) == 1
poly_df = poly_res[has_poly] %>% bind_rows(.id = "ROI_ID")

poly_res[[3]]
poly_res[[4]]
lengths(poly_res)
library(RBioFormats)
library(tidyverse)
tiff_files = dir("/mnt/c/project_data/UVMMIC-RA-13113/FinalScansAndROIs/", pattern = "tiff$", full.names = TRUE)
names(tiff_files) = basename(tiff_files)
tiff_files = as.list(tiff_files)

tf = tiff_files$`31'24.ome.tiff`

tiff_meta = RBioFormats::read.metadata(tf)

tiff_meta %>% class

tiff_meta@.Data[[1]]$coreMetadata
tiff_meta@.Data[[8]]$coreMetadata

RBioFormats::coreMetadata(tiff_meta)
RBioFormats::globalMetadata(tiff_meta)
RBioFormats::seriesMetadata(tiff_meta)


txml = RBioFormats::read.omexml(tf)
txml %>% class
length(txml)
nchar(txml)

library(xml2)
?xml2::read_xml

# OME-XML can be very large; parse with libxml2 "HUGE" mode enabled.
ome_xml = paste(txml, collapse = "")
ome_doc = xml2::read_xml(
    charToRaw(enc2utf8(ome_xml)),
    encoding = "UTF-8",
    options = c("RECOVER", "HUGE", "NOBLANKS")
)

get_ome_nodes = function(doc, node_types, return = c("nodes", "tibble")) {
    # stopifnot(inherits(doc, "xml_document"))
    if (length(node_types) == 0 || anyNA(node_types)) {
        stop("node_types must contain at least one non-missing type name", call. = FALSE)
    }

    return = match.arg(return)
    node_types = unique(as.character(node_types))
    xpath_terms = paste0("local-name()='", node_types, "'")
    xpath_query = paste0(".//*[", paste(xpath_terms, collapse = " or "), "]")
    nodes = xml2::xml_find_all(doc, xpath_query)

    if (identical(return, "nodes")) {
        return(nodes)
    }

    tibble::tibble(
        node_type = sub("^.*:", "", xml2::xml_name(nodes)),
        node_text = xml2::xml_text(nodes, trim = TRUE),
        attrs = lapply(nodes, xml2::xml_attrs)
    ) %>%
        tidyr::unnest_wider(attrs, names_repair = "universal")
}

# Namespace-agnostic query for Polygon annotations.
polygon_nodes = get_ome_nodes(ome_doc, "Polygon", return = "nodes")
length(polygon_nodes)

polygon_tbl = tibble(
    id = xml2::xml_attr(polygon_nodes, "ID"),
    points = xml2::xml_attr(polygon_nodes, "Points"),
    text = xml2::xml_attr(polygon_nodes, "Text"),
    the_z = suppressWarnings(as.integer(xml2::xml_attr(polygon_nodes, "TheZ"))),
    the_t = suppressWarnings(as.integer(xml2::xml_attr(polygon_nodes, "TheT"))),
    stroke_color = xml2::xml_attr(polygon_nodes, "StrokeColor"),
    fill_color = xml2::xml_attr(polygon_nodes, "FillColor")
)

polygon_tbl

summarize_ome_nodes = function(doc) {
    stopifnot(inherits(doc, "xml_document"))

    all_nodes = xml2::xml_find_all(doc, ".//*")
    all_names = sub("^.*:", "", xml2::xml_name(all_nodes))
    total_nodes = length(all_nodes)

    target_types = c(
        "OME", "Image", "Pixels", "Channel", "TiffData",
        "Plane", "StructuredAnnotations", "MapAnnotation",
        "ROI", "Union", "Shape", "Polygon", "Rectangle",
        "Ellipse", "Line", "Polyline", "Label", "Mask"
    )
    all_types = sort(unique(c(target_types, all_names)))

    unexpected_types = setdiff(unique(all_names), target_types)
    if (length(unexpected_types) > 0) {
        unexpected_counts = sort(table(all_names[all_names %in% unexpected_types]), decreasing = TRUE)
        unexpected_msg = paste0(
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

ome_summary_tbl = summarize_ome_nodes(ome_doc)
ome_summary_tbl %>% filter(n > 0) %>% pull(node_type)


roi_nodes = get_ome_nodes(ome_doc, "ROI", return = "nodes")
roi_ids = xml2::xml_attr(roi_nodes, "ID")
roi_nodes %>% class
roi_nodes %>% as.list
roi_nodes[1:3]
roi_nodes %>% get_ome_nodes("ID")
poly_res = lapply(roi_nodes, function(n){
    get_ome_nodes(n, "Polygon", return = "tibble")
})


names(poly_res) = roi_ids
has_poly = sapply(poly_res, nrow) == 1
poly_df = poly_res[has_poly] %>% bind_rows(.id = "ROI_ID") %>% select(-ID)

poly_res[[3]]
poly_res[[4]]
lengths(poly_res)
ellipse_res = sapply(roi_nodes, function(n){
    get_ome_nodes(n, "Ellipse", return = "tibble")
})
names(ellipse_res) = roi_ids
has_ellipse = sapply(ellipse_res, nrow) == 1
ellipse_df = ellipse_res[has_ellipse] %>% bind_rows(.id = "ROI_ID") %>% select(-ID)

get_ome_nodes(roi_nodes, "Label", return = "tibble") %>% select(ID, Text, X, Y)


label_res = lapply(roi_nodes, function(n){
    get_ome_nodes(n, "Label", return = "tibble") %>% select(Text, X, Y)
})
names(label_res) = roi_ids
label_df = bind_rows(label_res, .id = "ROI_ID")
label_df = label_df %>% select(ROI_ID, ROI = Text, X_label = X, Y_label = Y)
label_df = label_df %>% mutate(X_label = as.numeric(X_label), Y_label = as.numeric(Y_label))


ellipse_df = merge(label_df, ellipse_df, by = "ROI_ID")
poly_df = merge(label_df, poly_df, by = "ROI_ID")

pnts = poly_df$Points
lapply(strsplit(pnts, " "), function(xy_str){
    strsplit(xy_str, split = ",") %>% lapply()
})
poly_df %>% select(-Points)
xy_df = poly_df %>% group_by(ROI_ID) %>% reframe(xy_str = strsplit(Points, " ") %>% unlist) %>% group_by(ROI_ID) %>% mutate(point_idx = seq_along(xy_str))
xy_df = xy_df %>% separate(xy_str, ",", into = c("x", "y")) %>% mutate(x = as.numeric(x), y = as.numeric(y))
xy_df$x %>% class

library(ggforce)

p_poly = ggplot() +
    geom_polygon(data =xy_df, aes(x = x, y = y, group = ROI_ID)) +
    coord_fixed() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

?geom_ellipse
ellipse_df %>% head
to_num = c("X", "Y", "RadiusX", "RadiusY")
for(tn in to_num){
    ellipse_df[[tn]] = as.numeric(ellipse_df[[tn]])
}
library(ggforce)
ggplot() +
    # geom_ellipse(aes())
    geom_circle(data = ellipse_df, aes(x0 = X, y0 = Y, r = RadiusX))


p_poly = p_poly + geom_circle(data = ellipse_df, aes(x0 = X, y0 = Y, r = RadiusX))

p_poly.zoom = p_poly + coord_fixed(xlim = c(1.75e4, 2.25e4), ylim = c(4.5e4, 5.35e4))

p_poly.zoom +
    geom_label(data = poly_df, aes(x = X_label, y = Y_label, label = ROI, hjust = .5, vjust = .5)) +
    geom_label(data = ellipse_df, aes(x = X_label, y = Y_label, label = ROI, hjust = .5, vjust = .5))

poly_df

ellipse_df
poly_df



has_poly | has_ellipse

get_ome_nodes(roi_nodes, "Polygon", return = "tibble")
poly_full = get_ome_nodes(roi_nodes, "Polygon")
xml2::xml_structure(roi_nodes)

mask_df = get_ome_nodes(roi_nodes[1], "Mask", return = "tibble")
mask_df = get_ome_nodes(roi_nodes, "Mask", return = "tibble")
mask_df$node_text %>% nchar %>% sqrt
mask_df$Height
mask_df$Width
mask_df$X
mask_df$Y
mask_df$Y

mask_b64 = mask_df$node_text
if (!require(base64enc)) {
    install.packages("base64enc")
    library(base64enc)
}
decode_ome_mask <- function(mask_row, bit_order = c("msb", "lsb")) {
    bit_order <- match.arg(bit_order)

    raw_mask <- base64enc::base64decode(mask_row$node_text[[1]])

    width <- as.integer(mask_row$Width[[1]])
    height <- as.integer(mask_row$Height[[1]])
    n_pixels <- width * height

    bits <- as.integer(rawToBits(raw_mask))

    # rawToBits returns least-significant-bit first within each byte.
    # If the overlay looks wrong, retry with bit_order = "msb".
    if (identical(bit_order, "msb")) {
        bits <- matrix(bits, ncol = 8, byrow = TRUE)
        bits <- as.vector(t(bits[, 8:1, drop = FALSE]))
    }

    bits <- bits[seq_len(n_pixels)]

    matrix(bits, nrow = height, ncol = width, byrow = TRUE)
}

mask_to_df <- function(mask_row, bit_order = c("msb", "lsb")) {
    bit_order <- match.arg(bit_order)
    mask_mat <- decode_ome_mask(mask_row, bit_order = bit_order)

    on_idx <- which(mask_mat == 1L, arr.ind = TRUE)
    if (nrow(on_idx) == 0) {
        return(data.frame())
    }

    data.frame(
        x = as.numeric(mask_row$X[[1]]) + on_idx[, "col"] - 1,
        y = as.numeric(mask_row$Y[[1]]) + on_idx[, "row"] - 1
    )
}

decode_ome_masks <- function(mask_df, bit_order = c("msb", "lsb"), return = c("points", "matrix")) {
    bit_order <- match.arg(bit_order)
    return <- match.arg(return)

    required_cols <- c("node_text", "Width", "Height", "X", "Y")
    missing_cols <- setdiff(required_cols, colnames(mask_df))
    if (length(missing_cols) > 0) {
        stop(
            paste0("mask_df is missing required columns: ", paste(missing_cols, collapse = ", ")),
            call. = FALSE
        )
    }

    if (identical(return, "matrix")) {
        out <- mask_df
        out$mask_matrix <- lapply(seq_len(nrow(mask_df)), function(i) {
            decode_ome_mask(mask_df[i, , drop = FALSE], bit_order = bit_order)
        })
        return(out)
    }

    keep_cols <- colnames(mask_df)
    keep_cols <- setdiff(keep_cols, required_cols)
    point_res <- pbmcapply::pbmclapply(seq_len(nrow(mask_df)), function(i) {
        row_df <- mask_df[i, , drop = FALSE]
        pts <- mask_to_df(row_df, bit_order = bit_order)
        if (nrow(pts) == 0) {
            return(NULL)
        }

        row_meta <- row_df[rep(1, nrow(pts)), keep_cols, drop = FALSE]
        cbind(row_meta, pts)
    })

    dplyr::bind_rows(point_res)
}


undebug(decode_ome_masks)
options(mc.cores = 1)
mask_df %>% head
mask_pts = decode_ome_masks(mask_df %>% select(node_text, Text, Width, Height, X, Y))
mask_pts %>% colnames
mask_pts %>% head
mask_pts %>% dim

ggplot() +
    geom_raster(data = mask_pts, aes(x = x, y = y, fill = Text))

mask_pts <- mask_to_df(mask_df[1, ], bit_order = "msb")
mask_pts2 <- mask_to_df(mask_df[2, ], bit_order = "msb")
mask_pts %>% head

ggplot(mask_pts, aes(x = x, y = y)) +
    geom_raster()

ggplot() +
    geom_tile(data = mask_pts, aes(x = x, y = y), fill = "blue") +
    geom_tile(data = mask_pts2, aes(x = x, y = y), fill = "red")



p_poly+
    geom_tile(data = mask_pts, aes(x = x, y = y), fill = "blue") +
    geom_tile(data = mask_pts2, aes(x = x, y = y), fill = "red") +
    coord_cartesian(xlim = range(mask_pts$x), ylim = range(mask_pts$y))

range(mask_pts$x) %>% scales::rescale()
scale_lim = function(lim, factor){
    lim_d = diff(lim)
    half_span = lim_d * factor / 2
    mean(lim) + c(-half_span, half_span)
}


p_poly+
    geom_point(data = mask_pts, aes(x = x, y = y), color = "blue", size = .3) +
    geom_point(data = mask_pts2, aes(x = x, y = y), color = "red", size = .3) +
    coord_cartesian(xlim = range(mask_pts$x), ylim = range(mask_pts$y))

p_poly+
    geom_point(data = mask_pts, aes(x = x, y = y), color = "blue", size = .3) +
    geom_point(data = mask_pts2, aes(x = x, y = y), color = "red", size = .3) +
    coord_cartesian(xlim = scale_lim(range(mask_pts$x), .2), ylim = scale_lim(range(mask_pts$y), .2))



ggplot(mask_pts, aes(x = x, y = y)) +
    geom_point(size = 0.1) +
    scale_y_reverse() +
    coord_fixed()

571*571

cbind(mask_df$node_text)

mask_df$Text

get_ome_nodes(roi_nodes, "Mask") %>% xml_structure()

tmp2 = get_ome_nodes(ome_doc, "ROI", return = "nodes")[1] %>% as.character()
tmp2 %>% nchar
n1 = get_ome_nodes(ome_doc, "ROI", return = "nodes")[4]
n1 %>% xml_structure()
xml_attr(n1, "Text")
xml_attr(n1, "ROI")
get_ome_nodes(n1, "Text", return = "nodes")
get_ome_nodes(n1, "Ellipse", return = "nodes")
get_ome_nodes(n1, "Ellipse", return = "tibble")
get_ome_nodes(n1, "Label", return = "tibble")

n2 = get_ome_nodes(ome_doc, "Polygon", return = "nodes")[1]
xml2::xml_parent(n2)
n2 %>% xml_parent() %>% xml_parent()
get_ome_nodes(n1, "Mask", return = "nodes")
tmp = get_ome_nodes(n1, "Mask", return = "tibble")
tmp$node_text %>% nchar

library(TiffPlotR)
read_tiff_meta_data(tf)
fetchTiffData(tf)

ellipse_res = sapply(roi_nodes, function(n){
    get_ome_nodes(n, "Ellipse", return = "tibble")
})
names(ellipse_res) = roi_ids
has_ellipse = sapply(ellipse_res, nrow) == 1
ellipse_df = ellipse_res[has_ellipse] %>% bind_rows(.id = "ROI_ID") %>% select(-ID)

get_ome_nodes(roi_nodes, "Label", return = "tibble") %>% select(ID, Text, X, Y)
label_res = lapply(roi_nodes, function(n){
    get_ome_nodes(n, "Label", return = "tibble") %>% select(Text, X, Y)
})
names(label_res) = roi_ids
label_df = bind_rows(label_res, .id = "ROI_ID")

merge(label_df, ellipse_df, by = "ROI_ID")
merge(label_df, poly_df, by = "ROI_ID")

ellipse_df
poly_df

has_poly | has_ellipse

get_ome_nodes(roi_nodes, "Polygon", return = "tibble")

tmp2 = get_ome_nodes(ome_doc, "ROI", return = "nodes")[1] %>% as.character()
tmp2 %>% nchar
n1 = get_ome_nodes(ome_doc, "ROI", return = "nodes")[4]
n1 %>% xml_structure()
xml_attr(n1, "Text")
xml_attr(n1, "ROI")
get_ome_nodes(n1, "Text", return = "nodes")
get_ome_nodes(n1, "Ellipse", return = "nodes")
get_ome_nodes(n1, "Ellipse", return = "tibble")
get_ome_nodes(n1, "Label", return = "tibble")

n2 = get_ome_nodes(ome_doc, "Polygon", return = "nodes")[1]
xml2::xml_parent(n2)
n2 %>% xml_parent() %>% xml_parent()
get_ome_nodes(n1, "Mask", return = "nodes")
tmp = get_ome_nodes(n1, "Mask", return = "tibble")
tmp$node_text %>% nchar

library(TiffPlotR)
read_tiff_meta_data(tf)
fetchTiffData(tf)
