#GLRX specifically -
# need to get masks/rects from two basic GeoMx tiffs and retrieve from GLRX tiff
# hope the alignment is correct

library(TiffPlotR)
library(dplyr)
library(tidyr)
library(ggplot2)

# ----------------------------------------------------------------------------
# Example TIFF selection
# ----------------------------------------------------------------------------
tiff_files <- dir(
    "/mnt/c/project_data/UVMMIC-RA-13113/FinalScansAndROIs/",
    pattern = "tiff$",
    full.names = TRUE
)
names(tiff_files) <- basename(tiff_files)
tiff_files <- as.list(tiff_files)


# tf = "/mnt/c/project_data/UVMMIC-RA-13113/GLRX RNAscope/Slide 411_ Exp 31'24.ome.tiff"
# tf = "/mnt/c/project_data/UVMMIC-RA-13113/GLRX RNAscope images/Slide 411_ Exp 31'24.ome.tiff"
# tf = "/mnt/c/project_data/UVMMIC-RA-13113/GLRX RNAscope images/Slide 410_Exp29'24.ome.tiff"
# tf = "/mnt/c/project_data/UVMMIC-RA-13113/LXlI4oj06AJKJfOF3Ug9/TomatoRed_CD45_PanCK_3124.ome_.tiff"


# these 2 might be the same file
# tf = "/mnt/c/project_data/UVMMIC-RA-13113/FinalScansAndROIs/31'24.ome.tiff"
tf.gmx1 = "/mnt/c/project_data/UVMMIC-RA-13113/LXlI4oj06AJKJfOF3Ug9/TomatoRed_PanCk_CD45_3124.ome_.tiff"
tf.gmx2 = "/mnt/c/project_data/UVMMIC-RA-13113/LXlI4oj06AJKJfOF3Ug9/TomatoRed_CD45_PanCK_3124.ome_.tiff"

ann.gmx1 <- fetchTiffAnnotations(
    tiff_path = tf.gmx1,
    decode_masks = TRUE,
    include_summary = TRUE
)
ann.gmx2 <- fetchTiffAnnotations(
    tiff_path = tf.gmx2,
    decode_masks = TRUE,
    include_summary = TRUE
)
x = ann.gmx1$polygons$Points[1]
extract_poly_points = function(x){
    strsplit(strsplit(x, " ")[[1]], ",") %>% unlist %>% as.numeric %>% matrix(byrow = TRUE, ncol = 2) %>% as.data.frame %>% rename(x = V1, y = V2)
}

poly_pts.gmx1 = ann.gmx1$polygons %>% group_by(ROI_ID) %>% reframe(extract_poly_points(Points)) %>% group_by(ROI_ID) %>% summarise(x = mean(x), y = mean(y))
ellipse_pts.gmx1 = ann.gmx1$ellipses %>% mutate(x = as.numeric(X), y = as.numeric(Y)) %>% select(ROI_ID, x, y)

poly_pts.gmx1$group = "GMX1"
ellipse_pts.gmx1$group = "GMX1"

poly_pts.gmx2 = ann.gmx2$polygons %>% group_by(ROI_ID) %>% reframe(extract_poly_points(Points)) %>% group_by(ROI_ID) %>% summarise(x = mean(x), y = mean(y))
ellipse_pts.gmx2 = ann.gmx2$ellipses %>% mutate(x = as.numeric(X), y = as.numeric(Y)) %>% select(ROI_ID, x, y)

poly_pts.gmx2$group = "GMX2"
ellipse_pts.gmx2$group = "GMX2"

roi_pts = rbind(
    poly_pts.gmx1,
    ellipse_pts.gmx1,
    poly_pts.gmx2,
    ellipse_pts.gmx2
)

ggplot(roi_pts, aes(x = x, y = y, color = group)) +
    geom_point()

tf = "/mnt/c/project_data/UVMMIC-RA-13113/GLRX RNAscope/Slide411_GLRX_31-24.ome.tiff"
tf.test = "/mnt/c/project_data/UVMMIC-RA-13113/GLRX RNAscope/Slide 410_Exp29'24.ome.tiff"
format(file.size(tf), units = "GB")
file.size(tf) / 1e9

r = TiffRect(2e4, 3e4, 5e3, 15e3)
img.gmx1.z = fetchTiffData(tf.gmx1, rect = r)
img.gmx2.z = fetchTiffData(tf.gmx2, rect = r)
img.gmx1.z
img.gmx2.z

img.gmx1 = fetchTiffData(tf.gmx1)
img.gmx2 = fetchTiffData(tf.gmx2)
img = fetchTiffData(tf)
fetchTiffData(tf, selected_channels = 1)
fetchTiffData(tf.test, selected_channels = 1)

roi_pts$norm_value = 0
p.gmx = img.gmx1@plots$normalized +
    scale_fill_gradientn(colors = c("black", "white")) +
    geom_point(data = roi_pts, aes(x = x, y = y, color = group), size = .3)

p.gmx2 = img.gmx2@plots$normalized +
    scale_fill_gradientn(colors = c("black", "white")) +
    geom_point(data = roi_pts, aes(x = x, y = y, color = group), size = .3)

p.glrx = img@plots$normalized +
    scale_fill_gradientn(colors = c("black", "white")) +
    geom_point(data = roi_pts, aes(x = x, y = y, color = group), size = .3)

library(patchwork)
img.gmx1@plots$normalized +
img.gmx2@plots$normalized +
img@plots$normalized


p.glrx.r = img@plots$normalized + scale_x_reverse() + scale_y_continuous()
p.glrx.r@data$i %>% range
p.glrx.r@data$j %>% range
roi_pts.r = roi_pts
roi_pts.r$x = -1*(roi_pts.r$x - max(p.glrx.r@data$i))
roi_pts.r$y = -1*(roi_pts.r$y - max(p.glrx.r@data$j))
p.glrx.r = p.glrx.r + scale_fill_gradientn(colors = c("black", "white")) +
    geom_point(data = roi_pts.r, aes(x = x, y = y, color = group), size = .3)

p.gmx = p.gmx + labs(title = "GemMx annotation on GeoMx slide")
p.glrx.r = p.glrx.r + labs(title = "GemMx annotation on GLRX RNAscope slide")

p.gmx + p.gmx2

p.gmx + p.glrx.r

fetchTiffData(tf)
read_tiff_meta_data(tf)
RBioFormats::read.metadata(tf)
RBioFormats::read.omexml(tf)

TiffPlotR:::.read_tiff_ome_xml(tf.test) %>%
    TiffPlotR:::.fetch_ome_nodes(., "Channel", output = "data.frame")

ome_doc <- TiffPlotR:::.read_tiff_ome_xml(tf)
TiffPlotR:::.fetch_ome_nodes(ome_doc, "ROI") %>% xml2::xml_structure()
roi_in_ome = TiffPlotR:::.fetch_ome_nodes(ome_doc, "ROI", output = "data.frame")$ID
roi_in_ome
ome_chans = TiffPlotR:::.fetch_ome_nodes(ome_doc, "Channel", output = "data.frame")
ome_chans$Fluor
pixel_info = TiffPlotR:::.fetch_ome_nodes(ome_doc, "Pixels", output = "data.frame")
pixel_info

# fetchTiffData(tiff_files$`31'24.ome.tiff`)
# fetchTiffData(tiff_files$`29'24.ome.tiff`)

# tf <- tiff_files[["31'24.ome.tiff"]]
tf <- tiff_files$`13'22.ome.tiff`
# for(tf in tiff_files[1:2]){

    # full_img = fetchTiffData(tf)

    # ----------------------------------------------------------------------------
    # Read OME annotations via package API
    # ----------------------------------------------------------------------------
    ann <- fetchTiffAnnotations(
        tiff_path = tf,
        decode_masks = TRUE,
        include_summary = TRUE
    )

    ann$summary %>% filter(n < 40)

    ome_doc <- TiffPlotR:::.read_tiff_ome_xml(tf)
    ome_chans = TiffPlotR:::.fetch_ome_nodes(ome_doc, "Channel")
    pixel_info = TiffPlotR:::.fetch_ome_nodes(ome_doc, "Pixels", output = "data.frame")
    pixel_info

    geomx_chan_names = c(
        "Syto13",
        "PanCK",
        "CD45",
        "TdTomato"
    )

    rnascope_chan_names = c(
        "Syto13",

    )

    ann$ellipses %>% nrow
    ann$polygons %>% nrow
    ann$mask_points

    mask_rects = ann$mask_points %>% group_by(ROI_ID) %>% summarize(imin = min(i), imax = max(i), jmin = min(j), jmax = max(j))
    mask_rects = TiffRect(mask_rects$imin, mask_rects$imax, mask_rects$jmin, mask_rects$jmax, name = mask_rects$ROI_ID)
    mask_rects[1]
    mask_rects[2]

    i = 1
    parallel::detectCores()
    # ?pbmcapply::pbmcmapply()
    # options(mc.cores = 8)

    # install.packages("future")
    # install.packages("future.apply")
    library(future)

    # Set up a multisession plan for parallel processing
    # 20 will crash, i guess memory
    plan(sequential)
    plan(multisession, workers = 8) # Adjust workers based on your CPU cores
    plan()

    # Define a function to simulate a time-consuming task
    # slow_function <- function(x) {
    #     Sys.sleep(2) # Simulate a delay
    #     return(x^2)
    # }

    # inputs = 1:8

    # Use future_lapply for parallel processing
    # results <- future.apply::future_lapply(inputs, slow_function)


    # all_msk = pbmcapply::pbmclapply(seq(nrow(mask_rects@coords)), function(i){
    # all_msk = pbapply::pblapply(seq(nrow(mask_rects@coords)), function(i){
    dir.create("cache_mask_results")
    mask_points =  ann$mask_points
    write.table(paste("starting:", date()), "progress.txt", col.names = FALSE, row.names = FALSE)
    all_msk = future.apply::future_lapply(seq(nrow(mask_rects@coords)), future.globals = c("tf", "mask_rects", "mask_points", "chan_names", "fetchTiffDataMasked"), function(i){
        message(i)
        write.table(paste(i, "start"), "progress.txt", append = TRUE, col.names = FALSE, row.names = FALSE)
        fetch_rect = mask_rects[i]
        cache_file = file.path("cache_mask_results", paste0(basename(tf), ".", fetch_rect$name, ".Rds"))
        if(!file.exists(cache_file)){
            tryCatch({
                msk = fetchTiffDataMasked(tf, mask_points = mask_points, rect = fetch_rect, resolution = 1, channel_names = chan_names)
                saveRDS(msk, cache_file)
            }, error = function(e){
                write.table(paste(i, "ERROR"), "progress.txt", append = TRUE, col.names = FALSE, row.names = FALSE)
            })

        }
        write.table(paste(i, "end"), "progress.txt", append = TRUE, col.names = FALSE, row.names = FALSE)
        cache_file
    })
    write.table(paste("finished:", date()), "progress.txt", col.names = FALSE, row.names = FALSE, append = TRUE)

    plan(sequential)
}

mask_rects$name
names(all_msk) =


    msk@data

# ----------------------------------------------------------------------------
# Build a base image plot using package TIFF reader
# ----------------------------------------------------------------------------
# img <- fetchTiffData(tf)
view_rect = readRDS("tmp_view_rect.Rds")
img <- fetchTiffData(tf, rect = view_rect)
base_plot <- img@plots[[img@activePlot]] + coord_fixed()


undebug(fetchTiffDataMasked)
msk = fetchTiffDataMasked(tf, mask_points = ann$mask_points)

debug(TiffPlotR:::.ome_fill_color_to_hex)
msk2 = fetchTiffDataMasked(tf, mask_points = ann$mask_points, rect = view_rect)



# signal_df = readRDS("tmp_signal_df.Rds")
signal_df = img@data
mask_points = readRDS("tmp_mask_points.Rds")
mask_points = mask_points %>% rename(i = x, j = y)

mask_points %>% head
view_rect$xmin
view_rect$ymin
view_rect$xmax

mask_points = mask_points  %>% filter(i > view_rect$xmin & i < view_rect$xmax & j > view_rect$ymin & j < view_rect$ymax)

signal_df %>% head
p_img = ggplot(signal_df, aes(x = i, y = j, fill = norm_value)) +
    geom_raster() +
    facet_wrap(~channel) +
    scale_fill_gradientn(colours = c("black", "white"))

mask_points %>% head
mask_p = mask_points %>% group_by(Text, ROI_ID) %>% summarise(x = mean(i), y = mean(j))
mask_p$norm_value = 0



p_raw_mask = p_img + geom_point(data = mask_p, aes(x = x, y = y, color = Text))

p_raw_mask


# for lower resolutions, many mask points can match a single pixel
# need to convert mask points to pixel resolution
delta_i = signal_df$i %>% unique %>% sort %>% diff %>% table %>% sort %>% rev %>% head(n=1) %>% names %>% as.numeric
delta_j = signal_df$j %>% unique %>% sort %>% diff %>% table %>% sort %>% rev %>% head(n=1) %>% names %>% as.numeric

remain_i = signal_df$i %% delta_i %>% table %>% sort %>% rev %>% head(n = 1) %>% names %>% as.numeric
remain_j = signal_df$j %% delta_j %>% table %>% sort %>% rev %>% head(n = 1) %>% names %>% as.numeric

# join will be done on i_bin and j_bin
signal_df = signal_df %>%
    mutate(i_bin = round((i-remain_i) / delta_i)) %>%
    mutate(j_bin = round((j-remain_j) / delta_j))




# join will be done on i_bin and j_bin
mask_points = mask_points %>%
    mutate(i_bin = round((i-remain_i) / delta_i)) %>%
    mutate(j_bin = round((j-remain_j) / delta_j))

# select most common mask assignment per ROI_ID in pixel
mask_pixels = mask_points %>% group_by(Text, ROI_ID, i_bin, j_bin) %>% summarise(N = length(Text)) %>% group_by(ROI_ID, i_bin, j_bin) %>% mutate(fraction = N / sum(N)) %>% filter(fraction > .5)
mask_pixels = mask_pixels %>% mutate(i = i_bin*delta_i+remain_i, j = j_bin*delta_j+remain_j)

mask_pixels$norm_value = 0


sel_points = mask_points %>% filter(ROI_ID == "ROI:0")

xrng = sel_points$i %>% range
xrng = xrng + c(-200, 200)
yrng = sel_points$j %>% range
yrng = yrng + c(-200, 200)

mask_points$norm_value = 0
sel_points = mask_points %>% filter(i > min(xrng) & i < max(xrng) & j > min(yrng) & j < max(yrng))

# view_rect = TiffRect(min(xrng), max(xrng), min(yrng), max(yrng))
# saveRDS(view_rect, "tmp_view_rect.Rds")


p_img + geom_point(data = sel_points, aes(x = i, y = j, color = Text), size = .1, alpha = .2) +
    coord_cartesian(xlim = xrng, ylim = yrng)

p_img
p_img + geom_point(data = mask_pixels, aes(x = i, y = j, color = Text), size = .1) +
    coord_cartesian(xlim = xrng, ylim = yrng)



signal_df
mask_merge = mask_pixels %>% ungroup %>% select(Text, ROI_ID, i, j)
signal_df$i %>% class
mask_merge$i %>% class
signal_df$i %>% unique %>% sort
mask_merge$i %>% unique %>% sort

signal_df.masked = merge(signal_df, mask_merge, by = c("i", "j"))
signal_df.masked %>% head
ggplot(signal_df.masked, aes(x = i, y = j, fill = norm_value)) +
    geom_raster() +
    facet_grid(channel~Text) +
    scale_fill_viridis_c()

theme_set(theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))

ggplot(signal_df.masked, aes(x = Text, y = value)) +
    geom_boxplot() +
    facet_wrap(~channel, scales = "free_y")


signal_df.masked %>% head
xy_df = signal_df.masked %>% select(-norm_value) %>% pivot_wider(names_from = "channel", values_from = "value")
ggplot(xy_df, aes(x = `3`, y = `4`)) +
    geom_point() +
    facet_wrap(~Text)


ggplot(signal_df.masked, aes(x = Text, y = value)) +
    geom_violin() +
    facet_wrap(~channel, scales = "free_y")



mask_points
signal_df$j %>% diff %>% unique
mask_points


# ----------------------------------------------------------------------------
# Add annotations using shape_layer (ggplot2 '+' workflow)
# ----------------------------------------------------------------------------
p_annotated <- base_plot

if (!is.null(poly_shape)) {
    p_annotated <- p_annotated +
        shape_layer(poly_shape, color = "#2C7FB8", fill = NA, alpha = 0.8, linewidth = 0.5)
}

if (!is.null(ellipse_shape)) {
    p_annotated <- p_annotated +
        shape_layer(ellipse_shape, color = "#D95F0E", fill = NA, alpha = 0.8, linewidth = 0.5)
}

if (nrow(label_tbl) > 0) {
    p_annotated <- p_annotated +
        geom_label(
            data = label_tbl,
            aes(x = X_label, y = Y_label, label = ROI),
            inherit.aes = FALSE,
            size = 2.5,
            label.size = 0.1,
            alpha = 0.8
        )
}

if (nrow(ann$mask_points) > 0) {
    p_annotated <- p_annotated +
        geom_point(
            data = ann$mask_points,
            aes(x = x, y = y),
            inherit.aes = FALSE,
            color = "#31A354",
            size = 0.15,
            alpha = 0.6
        )
}

print(p_annotated)

# ----------------------------------------------------------------------------
# Optional: still works with direct shape_annotate calls
# ----------------------------------------------------------------------------
if (!is.null(poly_shape)) {
    p_poly_only <- shape_annotate(base_plot, poly_shape, color = "#2C7FB8", fill = NA, alpha = 0.7)
    print(p_poly_only)
}
