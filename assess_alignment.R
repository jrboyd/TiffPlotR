library(tidyverse)
library(TiffPlotR)
library(GeomxTools)

align_dir = "/mnt/c/project_data/UVMMIC-RA-13113/alignment/"
if(!dir.exists(align_dir)){
    align_dir = "D:/project_data/alignment"

}

geo_dat = readRDS("../../projects_core/02172026_reem_geomx/02_geomx_prep_31-24_v3/00_project_31-24_geomx_data.Rds")
geo_meta = sData(geo_dat)
geo_meta$Roi
geo_meta$Plate_ID %>% table
geo_meta$Scan.Name %>% table
geo_meta$`Scan Date` %>% table

geo_scan_map_df = geo_meta %>% select(SampleID, Roi, `Scan Date`, Scan.Name, Segment)

roi_by_scan =  split(geo_meta$Roi, geo_meta$`Scan Date`)
roi_by_scan$`2025-11-19T13:44:08` %>% as.numeric %>% range
roi_by_scan$`2025-11-18T15:39:30` %>% as.numeric %>% range

rscope_aligned_masks = dir('cache_mask_results_aligned/', full.names = TRUE)
geomx_masks = dir("../../projects_core/02172026_reem_geomx/cache_mask_results/", full.names = TRUE)
geomx_masks = geomx_masks[grepl("TomatoRed", geomx_masks)]

geomx_file_df = tibble(file = geomx_masks)
geo_img = readRDS(geomx_file_df$file[1])
geo_tiff_name = geo_img@tiff_path %>% basename
geo_tf = file.path(align_dir, geo_tiff_name)
# TiffPlotR::fetchTiffAnnotations()
tf_xml = TiffPlotR:::.read_tiff_ome_xml(geo_tf)
xml2::xml_structure(tf_xml)

all_tiff_files = dir(align_dir, pattern = "Red.+tiff$", full.names = TRUE)
names(all_tiff_files) = basename(all_tiff_files)
tiff_scan_info = lapply(all_tiff_files, function(tf){
    tf_xml = TiffPlotR:::.read_tiff_ome_xml(tf)
    TiffPlotR:::.fetch_ome_nodes(tf_xml, "Image", output = "data.frame")
})



tiff_scan_info = bind_rows(tiff_scan_info, .id = "file")
#fix name ' encoding
tiff_scan_info$Scan.Name = tiff_scan_info$Name %>% sub("&apos;", "'", .)

stopifnot(tiff_scan_info$Scan.Name %in% geo_scan_map_df$Scan.Name)


TiffPlotR:::.fetch_ome_nodes(tf_xml, "Plate", output = "data.frame")
TiffPlotR:::.fetch_ome_nodes(tf_xml, "Image", output = "data.frame")

rscope_file_df = tibble(file = rscope_aligned_masks)
rsc_img = readRDS(rscope_file_df$file[1])


geomx_file_df = geomx_file_df %>%
    mutate(group = basename(file) %>% sub("\\..+", "", .)) %>%
    mutate(ROI_ID = sub(".+tiff\\.", "", file) %>% sub(".Rds", "", .) %>% sub("\uf03a", "_", .))

geomx_file_df$group %>% table

rscope_file_df = rscope_file_df %>%
    # mutate(group = basename(file) %>% sub("\\..+", "", .)) %>%
    mutate(ROI_ID_full = sub(".+tiff\\.", "", file) %>% sub(".Rds", "", .) %>% sub("\uf03a", "_", .)) %>%
    mutate(group_num = ROI_ID_full %>% sub("_.+", "", .)) %>%
    mutate(ROI_ID = sub(".+_R", "R", ROI_ID_full))

rscope_file_df$group_num %>% table

#based on counts this is the correct assignment
# TomatoRed_PanCk_CD45_3124  is missing 8 though
group_num2group = c(
    "1" = "TomatoRed_CD45_PanCK_3124",
    "2" = "TomatoRed_PanCk_CD45_3124"
)

rscope_file_df$group = group_num2group[rscope_file_df$group_num]

image_file_df = merge(rscope_file_df, geomx_file_df, by = c("ROI_ID", "group")) %>%
    dplyr::rename(rscope_file = file.x, geomx_file = file.y)

.find_tiff = function(f){
    out_f = file.path(align_dir, basename(f))
    stopifnot(file.exists(out_f))
    out_f
}

#### setup ####

geomx_chan_names = c(
    "Syto13",
    "PanCK",
    "CD45",
    "TdTomato"
)

rscope_chan_names = c(
    "Syto13",
    "PanCK",
    "GLRX",
    "TomatoRed"
)

theme_set(theme(panel.grid = element_blank(), panel.background = element_rect()))
my_theme = theme(panel.grid = element_blank(), panel.background = element_rect())


library(patchwork)
# img_rscope$plots$masked + theme(panel.background = element_rect())

.prep_fetch_list = function(img, chan_names){
    tf = .find_tiff(img$tiff_path)
    r = img$rect
    r@meta = data.frame()
    list(tiff_path = tf, rect = r, chan_names = chan_names)
}

.plot_geomx_and_rscope_img = function(img_g, img_r){
    p_g = plot(img_g) + guides(fill = "none")
    p_g = p_g + labs(title = "GeoMx")
    p_r = plot(img_r) + guides(fill = "none")
    p_r = p_r + scale_x_reverse() + scale_y_continuous() + theme(panel.background = element_rect(), panel.grid = element_blank()) + labs(title = "RNAscope")
    wrap_plots(p_g, p_r, nrow = 1)
}

.fetch_single = function(tf, chan_names, zoom_rect = NULL, anno_rect = NULL){
    img = fetchTiffData(tf, channel_names = chan_names, rect = zoom_rect)
    if(!is.null(anno_rect)){
        img = img %>%
            shape_annotate(anno_rect)

    }
    img
}

.fetch_single_list_wide= function(list_args){
    .fetch_single(list_args$tiff_path, list_args$chan_names, zoom_rect = list_args$rect %>% shape_resize_mult(5), anno_rect = list_args$rect)
}


my_fetchWide = function(list_g, list_r){
    img_g = .fetch_single_list_wide(list_g)
    img_r = .fetch_single_list_wide(list_r)
    .plot_geomx_and_rscope_img(img_g, img_r)
}

#### fetch wide views around ROI ####
out_dir = "alignment_assessment_plots"
dir.create(out_dir)

i = 1
for(i in seq_along(image_file_df$geomx_file)){
    this_roi = image_file_df$ROI_ID[i]
    this_group = image_file_df$group[i]
    root_name = paste0("wide_images_", this_group, "_", this_roi)
    out_file = file.path(out_dir, paste0(root_name, ".png"))

    message(out_file)
    if(file.exists(out_file)){
        message("skip")
        next
    }

    img_geomx = readRDS(image_file_df$geomx_file[i])
    img_rscope = readRDS(image_file_df$rscope_file[i])

    fl_geomx = .prep_fetch_list(img_geomx, geomx_chan_names)
    fl_rscope = .prep_fetch_list(img_rscope, rscope_chan_names)

    pw_res = my_fetchWide(fl_geomx, fl_rscope)
    pw_res = pw_res + plot_annotation(title = this_roi, subtitle = this_group)

    ggsave(out_file, pw_res, width = 8, height = 5)
}

library(future)
plan(multisession, workers = 12) # Adjust workers based on your CPU cores
plan()


i = 1
needed_globals = c("image_file_df", ".prep_fetch_list", "my_fetchWide", "out_dir")
# future.apply::future_lapply(
# seq_along(image_file_df$geomx_file),
# future.globals = needed_globals,
# function(i){

for(i in seq_along(image_file_df$geomx_file)){
    this_roi = image_file_df$ROI_ID[i]
    this_group = image_file_df$group[i]
    root_name = paste0("mask_images_", this_group, "_", this_roi)
    out_file = file.path(out_dir, paste0(root_name, ".png"))

    message(out_file)
    if(file.exists(out_file)){
        message("skip")
        next
    }

    img_geomx = readRDS(image_file_df$geomx_file[i])
    img_rscope = readRDS(image_file_df$rscope_file[i])

    img_geomx@data %>% group_by(channel) %>% summarize(max(norm_value))
    q_geomx = img_geomx@data %>% group_by(channel) %>% summarize(cap = quantile(value, .98))
    q_rscope = img_rscope@data %>% group_by(channel) %>% summarize(cap = quantile(value, .98))

    dat_geomx = merge(img_geomx@data, q_geomx, by = "channel")
    dat_geomx$group = "GeoMx"
    dat_rscope = merge(img_rscope@data, q_rscope, by = "channel")
    dat_rscope$group = "RNAscope"

    .norm_vales = function(dat){
        dat = dat %>% mutate(norm_value = value / cap)
        dat = dat %>% mutate(norm_value = ifelse(norm_value > 1, 1, norm_value))
        dat
    }

    dat_geomx = .norm_vales(dat_geomx)
    dat_rscope = .norm_vales(dat_rscope)

    p_geomx = ggplot(dat_geomx, aes(x = i, y = j, fill = norm_value)) +
        geom_raster() +
        facet_grid(channel~Text+group) +
        scale_fill_viridis_c(option = "magma") +
        theme(panel.background = element_rect(fill = "gray50")) +
        coord_fixed() +
        scale_y_reverse() + scale_x_continuous() +
        guides(fill = "none")

    p_rscope = ggplot(dat_rscope, aes(x = i, y = j, fill = norm_value)) +
        geom_raster() +
        facet_grid(channel~Text+group) +
        scale_fill_viridis_c(option = "magma") +
        theme(panel.background = element_rect(fill = "gray50")) +
        coord_fixed() +
        scale_x_reverse() + scale_y_continuous() +
        guides(fill = "none")

    # pw_res = .plot_geomx_and_rscope_img(img_geomx, img_rscope)
    p_geomx = p_geomx + labs(title = "GeoMx")
    p_rscope = p_rscope + labs(title = "RNAscope")
    pw_res = p_geomx + p_rscope
    pw_res = pw_res + plot_annotation(title = this_roi, subtitle = this_group)

    ggsave(out_file, pw_res, width = 11, height = 9)
}
#)


