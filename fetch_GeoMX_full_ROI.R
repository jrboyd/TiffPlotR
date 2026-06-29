#GLRX specifically -
# need to get masks/rects from two basic GeoMx tiffs and retrieve from GLRX tiff
# hope the alignment is correct

# RNAscope channel order was incorrect, GLRX is definitely 4, TdTomato is 3

# AOI masks are poorly aligned in serial section.
# fetch full ROI and resegment

library(TiffPlotR)
library(dplyr)
library(tidyr)
library(ggplot2)

# ----------------------------------------------------------------------------
# Example TIFF selection
# ----------------------------------------------------------------------------
align_dir = "/mnt/c/project_data/UVMMIC-RA-13113/alignment/"
if(!dir.exists(align_dir)){
    align_dir = "D:/project_data/alignment"
    dir(align_dir)
}

tiff_files <- dir(
    align_dir,
    pattern = "tiff$",
    full.names = TRUE
)
names(tiff_files) <- basename(tiff_files)
tiff_files <- as.list(tiff_files)

alignment_data = readRDS("aligned_mask_points.Rds")

# fetchTiffData(tiff_files$`31'24.ome.tiff`)
# fetchTiffData(tiff_files$`29'24.ome.tiff`)

# tf <- tiff_files[["31'24.ome.tiff"]]
tf <- tiff_files$`Slide411_GLRX_31-24.ome.tiff`
# for(tf in tiff_files[1:2]){

geomx_chan_names = c(
    "Syto13",
    "PanCK",
    "CD45",
    "TdTomato"
)

#old bad order
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

ann = list()


#### verify that points from polygons are accurate ####
# need to get points contained in each ROI shape
test_poly = alignment_data$all_poly$`Slide411_GLRX_31-24`
# roi_tp = test_poly$name %>% unique %>% sample(2)
roi_tp = c("1_ROI:43", "1_ROI:3")
test_poly@coords = test_poly@coords %>% filter(name %in% roi_tp)

test_pts = shape_contained_points(test_poly)
test_pts %>% head
test_pts %>% nrow

p_test_points = ggplot(test_pts %>% slice_sample(n = 2e4), aes(x = x, y = y, color = name)) +
    geom_point()

p_test_points = p_test_points %>% shape_annotate(test_poly)
p_test_points

p_test_points + coord_cartesian(xlim = c(2000, 3e3), ylim = c(58e3,56e3))

#### verify that points from ellipses are accurate ####
test_poly = alignment_data$all_ellipse$`Slide411_GLRX_31-24`
roi_tp = test_poly$name %>% unique %>% sample(2)
roi_tp = c("2_ROI:17", "2_ROI:5")
test_poly@coords = test_poly@coords %>% filter(name %in% roi_tp)

test_pts = shape_contained_points(test_poly)
test_pts %>% head
test_pts %>% nrow

p_test_points = ggplot(test_pts %>% slice_sample(n = 2e4), aes(x = x, y = y, color = name)) +
    geom_point()

p_test_points = p_test_points %>% shape_annotate(test_poly)
p_test_points

p_test_points + coord_cartesian(xlim = c(18750, 19500), ylim = c(5.25e4, 5.4e4))

alignment_data$all_poly
alignment_data$all_ellipse
# alignment_data$all_rects

#### assemble all ROI points ####

poly_pts = shape_contained_points(alignment_data$all_poly$`Slide411_GLRX_31-24`)
elli_pts = shape_contained_points(alignment_data$all_ellipse$`Slide411_GLRX_31-24`)

ann$mask_points = rbind(poly_pts, elli_pts) %>% tibble
ann$mask_points %>% head


# is x/y i or j?
ann$mask_points$y %>% max
alignment_data$all_mask_points$j %>% max
#y is j

ann$mask_points$x %>% max
alignment_data$all_mask_points$i %>% max
#x is i

ann$mask_points = ann$mask_points %>% rename(i = x, j = y, ROI_ID = name)


fold_inc = nrow(ann$mask_points) / nrow(alignment_data$all_mask_points)
message(fold_inc %>% round(2), " fold more points in ROI vs AOI")

#### fetch signal at mask ####
# same script as before for retrieving signal at mask points from here
mask_rects = ann$mask_points %>% group_by(ROI_ID) %>% summarize(imin = min(i), imax = max(i), jmin = min(j), jmax = max(j))
mask_rects = TiffRect(mask_rects$imin, mask_rects$imax, mask_rects$jmin, mask_rects$jmax, name = mask_rects$ROI_ID)
mask_rects[1]
mask_rects[2]

i = 1
# install.packages("future")
# install.packages("future.apply")
library(future)

# Set up a multisession plan for parallel processing
# 20 will crash, i guess memory
# 8 worked with 64 GB mem
plan(sequential)
plan(multisession, workers = 12) # Adjust workers based on your CPU cores
plan()

#set missing globals
cache_dir = "cache_ROI_results_aligned"
dir.create(cache_dir)

mask_points =  ann$mask_points
# Text attribute is currently required
mask_points$Text = "fullROI"

#### assess precalc max ####
if(FALSE){
    p_max = msk$precalc_max
    p_max$channel_number = p_max$channel
    p_max$channel = chan_names[p_max$channel_number]
    p_max$value = p_max$max_value

    new_p_max = p_max
    new_p_max$max_value = 3*new_p_max$max_value
    new_p_max$channel = new_p_max$channel_number

    msk2 = fetchTiffDataMasked(tf, mask_points = mask_points, rect = fetch_rect, resolution = 1, channel_names = chan_names, precalc_max = new_p_max)
    msk2$plots$masked +
        coord_fixed(xlim = c(10900, 11200), ylim = c(49700, 50000))

    ggplot(msk$data, aes(x = value+1, fill = channel)) +
        geom_histogram() +
        scale_x_log10() +
        geom_vline(data = p_max, aes(xintercept = max_value*10)) +
        facet_grid(channel~., scales = "free_x")
}

chan_names = rscope_chan_names


plan(multisession, workers = 10) # Adjust workers based on your CPU cores
# plan()
write.table(paste("starting:", date()), "progress.txt", col.names = FALSE, row.names = FALSE)
all_msk = future.apply::future_lapply(seq(nrow(mask_rects@coords)), future.globals = c("tf", "mask_rects", "mask_points", "chan_names", "fetchTiffDataMasked", "cache_dir"), function(i){
    message(i)
    write.table(paste(i, "start"), "progress.txt", append = TRUE, col.names = FALSE, row.names = FALSE)
    fetch_rect = mask_rects[i]
    cache_file = file.path(cache_dir, paste0(basename(tf), ".", fetch_rect$name, ".Rds"))
    cache_file = sub(":", "_", cache_file)

    mask_points.sel =  subset(mask_points, ROI_ID == fetch_rect$name)

    if(!file.exists(cache_file)){
        tryCatch({
            write.table(paste(i, "try run"), "progress.txt", append = TRUE, col.names = FALSE, row.names = FALSE)
            msk = fetchTiffDataMasked(tf, mask_points = mask_points.sel, rect = fetch_rect, resolution = 1, channel_names = chan_names)
            saveRDS(msk, cache_file)
        }, error = function(e){
            write.table(paste(i, "ERROR"), "progress.txt", append = TRUE, col.names = FALSE, row.names = FALSE)
        })

    }else{
        write.table(paste(i, "use cache"), "progress.txt", append = TRUE, col.names = FALSE, row.names = FALSE)
    }
    write.table(paste(i, "end"), "progress.txt", append = TRUE, col.names = FALSE, row.names = FALSE)
    cache_file
})
write.table(paste("finished:", date()), "progress.txt", col.names = FALSE, row.names = FALSE, append = TRUE)

plan(sequential)

