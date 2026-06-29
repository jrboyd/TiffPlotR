library(tidyverse)
library(TiffPlotR)
library(GeomxTools)

#### setup data paths ####

rscope_alignment_cache = "cache_ROI_results_aligned"

# geomx_mask_cache = "../../projects_core/02172026_reem_geomx/cache_mask_results/"
geomx_mask_cache = "cache_mask_results.full_ROI"

geomx_data_rds = "../../projects_core/02172026_reem_geomx/02_geomx_prep_31-24_v3/00_project_31-24_geomx_data.Rds"

align_dir = "/mnt/c/project_data/UVMMIC-RA-13113/alignment/"
if(!dir.exists(align_dir)){
    align_dir = "D:/project_data/alignment"

}

#### load data ####

geo_dat = readRDS(geomx_data_rds)
geo_meta = sData(geo_dat)
geo_meta$Roi
geo_meta$Plate_ID %>% table
geo_meta$Scan.Name %>% table
geo_meta$`Scan Date` %>% table

geo_scan_map_df = geo_meta %>% select(SampleID, Roi, `Scan Date`, Scan.Name, Segment)

roi_by_scan =  split(geo_meta$Roi, geo_meta$`Scan Date`)
roi_by_scan$`2025-11-19T13:44:08` %>% as.numeric %>% range
roi_by_scan$`2025-11-18T15:39:30` %>% as.numeric %>% range

rscope_aligned_masks = dir(rscope_alignment_cache, full.names = TRUE)
geomx_masks = dir(geomx_mask_cache, full.names = TRUE)
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

#### match GeoMx tiffs to ROI group number ####

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

# old wrong order
# rscope_chan_names = c(
#     "Syto13",
#     "PanCK",
#     "GLRX",
#     "TomatoRed"
# )

rscope_chan_names = c(
    "Syto13",
    "PanCK",
    "TomatoRed",
    "GLRX"
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

.fetch_single = function(tf, chan_names, zoom_rect = NULL, anno_rect = NULL, resolution = NULL, precalc_max = NULL){
    img = fetchTiffData(tf, channel_names = chan_names, rect = zoom_rect, resolution = resolution, precalc_max = precalc_max)
    if(!is.null(anno_rect)){
        img = img %>%
            shape_annotate(anno_rect)

    }
    img
}

.fetch_single_list_wide= function(list_args, resolution = NULL){
    .fetch_single(list_args$tiff_path, list_args$chan_names, zoom_rect = list_args$rect %>% shape_resize_mult(5), anno_rect = list_args$rect, resolution = resolution, precalc_max = list_args$precalc_max)
}


my_fetchWide = function(list_g, list_r, resolution = NULL){
    img_g = .fetch_single_list_wide(list_g, resolution = resolution)
    img_r = .fetch_single_list_wide(list_r, resolution = resolution)
    .plot_geomx_and_rscope_img(img_g, img_r)
}

#### fetch wide views around ROI ####
out_dir = "alignment_assessment_plots_full_ROI"
dir.create(out_dir)
dir(out_dir)

if(FALSE){
    #junk code to verify compatibility of resolution 4 precalc_max with other resolutions
    #ROI with high geomx TdTomato and high RNAscope GLRX
    precalc_roi = "ROI_27"
    precalc_group = "TomatoRed_CD45_PanCK_3124"

    precalc_img_df = subset(image_file_df, ROI_ID == precalc_roi & group == precalc_group)
    img_geomx = readRDS(precalc_img_df$geomx_file[1])
    img_rscope = readRDS(precalc_img_df$rscope_file[1])

    cowplot::plot_grid(

        img_geomx$plots$masked + facet_grid(channel~.),
        img_rscope$plots$masked + scale_y_continuous() + scale_x_reverse()
    )

    fl_geomx = .prep_fetch_list(img_geomx, geomx_chan_names)
    fl_rscope = .prep_fetch_list(img_rscope, rscope_chan_names)

    pw_res = my_fetchWide(fl_geomx, fl_rscope)
    pw_res = pw_res + plot_annotation(title = this_roi, subtitle = this_group)

    img_g = .fetch_single_list_wide(fl_geomx)
    img_r = .fetch_single_list_wide(fl_rscope)

    precalc_max_res4 = list(
        "geomx" = img_g@precalc_max,
        "rscope" = img_r@precalc_max
    )
    saveRDS(precalc_max_res4, "precalc_max_res4.Rds")

    pc_g = img_g@precalc_max
    pc_g$channel = geomx_chan_names[pc_g$channel]

    pc_g.new = img_g@data %>% group_by(channel) %>% summarize(min_value = quantile(value, .25), max_value = quantile(value, .995))

    ggplot(img_g@data) +
        scale_x_log10() +
        geom_histogram(aes(x = value, fill = channel)) +
        geom_vline(data = pc_g, aes(xintercept = max_value)) +
        geom_vline(data = pc_g.new, aes(xintercept = max_value), color = "red") +
        geom_vline(data = pc_g.new, aes(xintercept = min_value), color = "blue") +
        facet_wrap(channel~., scales = "free_x", ncol = 1)

    pc_r = img_r@precalc_max
    pc_r$channel = rscope_chan_names[pc_r$channel]

    pc_r.new = img_r@data %>% group_by(channel) %>% summarize(min_value = quantile(value, .25), max_value = quantile(value, .995))
    # pc_r.new = img_r@data %>% group_by(channel) %>% summarize(min_value = quantile(value, .85), max_value = quantile(value, .999))


    ggplot(img_r@data, aes(x = value, fill = channel)) +
        scale_x_log10() +
        geom_histogram() +
        geom_vline(data = pc_r, aes(xintercept = max_value)) +
        geom_vline(data = pc_r.new, aes(xintercept = max_value), color = "red") +
        geom_vline(data = pc_r.new, aes(xintercept = min_value), color = "blue") +
        facet_wrap(channel~., scales = "free_x", ncol = 1)

    precalc_minmax = list(
        "geomx" = pc_g.new,
        "rscope" = pc_r.new
    )
    precalc_minmax = lapply(precalc_minmax, function(x){
        x$channel = seq(4)
        x
    })
    saveRDS(precalc_max_res4, "precalc_minmax.Rds")


    fl_geomx$precalc_max = precalc_minmax$geomx
    fl_rscope$precalc_max = precalc_minmax$rscope

    img_g.mm = .fetch_single_list_wide(fl_geomx, resolution = 4)
    img_r.mm = .fetch_single_list_wide(fl_rscope, resolution = 4)

    fl_geomx$precalc_max = precalc_max_res4$geomx
    fl_rscope$precalc_max = precalc_max_res4$rscope

    img_g.mr4 = .fetch_single_list_wide(fl_geomx, resolution = 4)
    img_r.mr4 = .fetch_single_list_wide(fl_rscope, resolution = 4)

    cowplot::plot_grid(
        img_g.mm$plots$normalized.annotated,
        img_g.mr4$plots$normalized.annotated
    )

    cowplot::plot_grid(
        img_r.mm$plots$normalized.annotated,
        img_r.mr4$plots$normalized.annotated
    )

    #same precalc looks fine for resolution 1
    img_g = .fetch_single_list_wide(fl_geomx, resolution = 1)
    img_r = .fetch_single_list_wide(fl_rscope, resolution = 1)

    pc_g = img_g@precalc_max
    pc_g$channel = geomx_chan_names[pc_g$channel]

    ggplot(img_g@data) +
        scale_x_log10() +
        geom_histogram(aes(x = value, fill = channel)) +
        geom_vline(data = pc_g, aes(xintercept = max_value)) +
        facet_grid(channel~., scales = "free_x")

    pc_r = img_r@precalc_max
    pc_r$channel = rscope_chan_names[pc_r$channel]


    ggplot(img_r@data, aes(x = value, fill = channel)) +
        scale_x_log10() +
        geom_histogram() +
        geom_vline(data = pc_r, aes(xintercept = max_value)) +
        facet_grid(channel~., scales = "free_x")
}else{
    precalc_max_res4 = readRDS("precalc_max_res4.Rds")
    # precalc_max_res4 = readRDS("precalc_minmax.Rds")
}



# globals = ls(pattern = "\\.", all.names = TRUE)
# globals = c(globals,
#             c("image_file_df", "out_dir", ".prep_fetch_list", "precalc_max_res4", "my_fetchWide", "align_dir", "geomx_chan_names", "rscope_chan_names")
#             )

globals = ls(all.names = TRUE)

# library(future)
# plan(sequential)
# plan(multisession, workers = 12) # Adjust workers based on your CPU cores

i = 1
# future.apply::future_lapply(seq_along(image_file_df$geomx_file), future.globals = ls(all.names = TRUE), function(i){

#### wide images ####
for(i in seq_along(image_file_df$geomx_file)){
    this_roi = image_file_df$ROI_ID[i]
    this_group = image_file_df$group[i]
    root_name = paste0("wide_images_", this_group, "_", this_roi)
    out_file = file.path(out_dir, paste0(root_name, ".png"))

    message(out_file)
    # if(file.exists(out_file)){
    #     message("skip")
    #     next
    # }

    img_geomx = readRDS(image_file_df$geomx_file[i])
    img_rscope = readRDS(image_file_df$rscope_file[i])

    fl_geomx = .prep_fetch_list(img_geomx, geomx_chan_names)
    fl_rscope = .prep_fetch_list(img_rscope, rscope_chan_names)

    fl_geomx$precalc_max = precalc_max_res4$geomx
    fl_rscope$precalc_max = precalc_max_res4$rscope

    pw_res = my_fetchWide(fl_geomx, fl_rscope, resolution = 4)
    pw_res = pw_res + plot_annotation(title = this_roi, subtitle = this_group)

    ggsave(out_file, pw_res, width = 8, height = 5)
}
# )

# library(future)
# plan(multisession, workers = 12) # Adjust workers based on your CPU cores
# plan()


i = 1
# needed_globals = c("image_file_df", ".prep_fetch_list", "my_fetchWide", "out_dir")
# future.apply::future_lapply(
# seq_along(image_file_df$geomx_file),
# future.globals = needed_globals,
# function(i){

#### ROI images ####
for(i in seq_along(image_file_df$geomx_file)){
    this_roi = image_file_df$ROI_ID[i]
    this_group = image_file_df$group[i]
    root_name = paste0("ROI_images_", this_group, "_", this_roi)
    out_file = file.path(out_dir, paste0(root_name, ".png"))

    message(out_file)
    if(file.exists(out_file)){
        message("skip")
        next
    }

    img_geomx = readRDS(image_file_df$geomx_file[i])
    img_rscope = readRDS(image_file_df$rscope_file[i])

    # img_geomx@data %>% group_by(channel) %>% summarize(max(norm_value))
    # q_geomx = img_geomx@data %>% group_by(channel) %>% summarize(cap = quantile(value, .98))
    # q_rscope = img_rscope@data %>% group_by(channel) %>% summarize(cap = quantile(value, .98))

    #use precalc


    pc_geomx = precalc_max_res4$geomx
    pc_geomx$channel = geomx_chan_names[pc_geomx$channel]
    dat_geomx = merge(img_geomx@data, pc_geomx, by = "channel")
    dat_geomx$group = "GeoMx"

    pc_rscope = precalc_max_res4$rscope
    pc_rscope$channel = rscope_chan_names[pc_rscope$channel]
    dat_rscope = merge(img_rscope@data, pc_rscope, by = "channel")
    dat_rscope$group = "RNAscope"

    .norm_vales = function(dat){
        dat = dat %>% mutate(norm_value = value / max_value)
        dat = dat %>% mutate(norm_value = ifelse(norm_value > 1, 1, norm_value))
        dat
    }

    dat_geomx = .norm_vales(dat_geomx)
    dat_rscope = .norm_vales(dat_rscope)

    # ggplot(dat_geomx, aes(x = i, y = j, fill = norm_value)) +
    #     geom_raster() +
    #     facet_grid(channel~Text+group) +
    #     # facet_grid(channel~group) +
    #     scale_fill_viridis_c(option = "magma") +
    #     theme(panel.background = element_rect(fill = "gray50")) +
    #     coord_fixed() +
    #     scale_y_reverse() + scale_x_continuous() +
    #     guides(fill = "none")

    p_geomx = ggplot(dat_geomx, aes(x = i, y = j, fill = norm_value)) +
        geom_raster() +
        # facet_grid(channel~Text+group) +
        facet_grid(channel~group) +
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

    ggsave(out_file, pw_res, width = 6.2, height = 9.2)
}
#)


