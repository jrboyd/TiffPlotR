library(magrittr)
library(TiffPlotR)

tf = "inst/extdata/301_CellPellet_NegCTL_Scan1_PC3Guise.ome.tiff"
fetchTiffAnnotations(tf)
ome_doc <- TiffPlotR:::.read_tiff_ome_xml(tf)
ome_chans = TiffPlotR:::.fetch_ome_nodes(ome_doc, "Channel")
df = TiffPlotR:::.fetch_ome_nodes(ome_doc, "Channel", output = "data.frame")
df
TiffPlotR:::to_rgba_hex(df$Color)

TiffPlotR:::.fetch_ome_nodes(ome_doc, "PhysicalSizeX", output = "data.frame")
TiffPlotR:::.fetch_ome_nodes(ome_doc, "PhysicalSizeXUnit", output = "data.frame")
TiffPlotR:::.fetch_ome_nodes(ome_doc, "ImageDescription", output = "data.frame")
TiffPlotR:::.fetch_ome_nodes(ome_doc, "ImageDescription")

xml2::xml_structure(ome_doc)

# ome_doc <- TiffPlotR:::.read_tiff_ome_xml(tiff_path)
pixel_info = TiffPlotR:::.fetch_ome_nodes(ome_doc, "Pixels", output = "data.frame")

stopifnot(pixel_info$PhysicalSizeX == pixel_info$PhysicalSizeY)
unit_per_pixel = pixel_info$PhysicalSizeX %>% as.numeric
unit_name = pixel_info$PhysicalSizeXUnit


pixel_info$PhysicalSizeX

td = fetchTiffData(tf)
tdz = fetchTiffData(tf, rect = TiffRect(2000, 3000, 2500, 3500))




# add_um_scale_bar(td, pixel_info$PhysicalSizeX %>% as.numeric, bar_fraction = .2)
# debug(TiffPlotR:::.scale_bar_convert_to_um)
# debug(add_um_scale_bar)
add_um_scale_bar(td)
add_um_scale_bar(tdz)


add_um_scale_bar(td, label_offset_fraction = -.03)
add_um_scale_bar(tdz, label_offset_fraction = -.03)

add_um_scale_bar(td, position = "topright")
add_um_scale_bar(tdz, position = "topright")


add_um_scale_bar(tdz, label_offset_fraction = .01)
add_um_scale_bar(tdz, label_offset_fraction = -.02)
add_um_scale_bar(td, pad_fraction = .01, label_offset_fraction = -.05)
add_um_scale_bar(td, pad_fraction = .01, position = "bottomright")
add_um_scale_bar(td, position = "topleft")
add_um_scale_bar(td, pad_fraction = .01, label_offset_fraction = .01, position = "topright")

add_um_scale_bar(td, pad_fraction = .01, label_offset_fraction = .1, position = "topright")
