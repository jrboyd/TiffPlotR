library(tidyverse)
library(TiffPlotR)
library(GeomxTools)

align_dir = "/mnt/c/project_data/UVMMIC-RA-13113/alignment/"
if(!dir.exists(align_dir)){
    align_dir = "D:/project_data/alignment"

}

tf_files = dir(align_dir, pattern = "tiff$", full.names = TRUE)
tf = tf_files[1]
x_res = TiffPlotR:::.read_tiff_ome_xml(tf)
ann = TiffPlotR:::fetchTiffAnnotations(tf)
ann$summary %>% filter(n == 4)

TiffPlotR:::.fetch_ome_nodes(x_res, "Pixels", output = "data.frame")
TiffPlotR:::.fetch_ome_nodes(x_res, "Pixels")
chan_info1 = TiffPlotR:::.fetch_ome_nodes(x_res, "Channel", output = "data.frame")
TiffPlotR:::.fetch_ome_nodes(x_res, "Channel") %>% xml2::xml_parent()
TiffPlotR:::.fetch_ome_nodes(x_res, "TiffData")
TiffPlotR:::.fetch_ome_nodes(x_res, "ChannelInfo", output = "data.frame")
TiffPlotR:::.fetch_ome_nodes(x_res, "ChannelInfo") %>% TiffPlotR:::.fetch_ome_nodes("ChannelInfo")

cinfo = TiffPlotR:::.fetch_ome_nodes(x_res, "ChannelInfo")
cinfo %>% xml2::xml_children()
TiffPlotR:::.fetch_ome_nodes(x_res, "ChannelInfo")[1] %>% TiffPlotR:::.fetch_ome_nodes(node_types = c("Name", "Dye", "DisplayName"), output = "data.frame")

TiffPlotR:::.fetch_ome_nodes(x_res, "BiologicalTarget")
TiffPlotR:::.fetch_ome_nodes(x_res, "Dye")
TiffPlotR:::.fetch_ome_nodes(x_res, "Maxintensity")
TiffPlotR:::.fetch_ome_nodes(x_res, "Minintensity")

TiffPlotR:::.fetch_ome_nodes(x_res, "CommentAnnotation")
TiffPlotR:::.fetch_ome_nodes(x_res, "BiologicalClass")

sanno = TiffPlotR:::.fetch_ome_nodes(x_res, "BiologicalClass") %>% xml2::xml_parent()  %>% xml2::xml_parent()   %>% xml2::xml_parent()   %>% xml2::xml_parent()
xml2::xml_structure(sanno)

chan_children = lapply(TiffPlotR:::.fetch_ome_nodes(x_res, "ChannelInfo"), xml2::xml_children)
x = chan_children[[1]]
x
chan_info = lapply(chan_children, function(x){
    data.frame(name = xml2::xml_name(x), value = xml2::xml_text(x))
}) %>% bind_rows(.id = "id")
chan_info = chan_info %>% pivot_wider()



