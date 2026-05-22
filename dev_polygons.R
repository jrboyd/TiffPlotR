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
xml2::xml_parents(n2)

get_ome_nodes(n1, "Mask", return = "nodes")
tmp = get_ome_nodes(n1, "Mask", return = "tibble")
tmp$node_text %>% nchar

library(TiffPlotR)
read_tiff_meta_data(tf)
fetchTiffData(tf)
