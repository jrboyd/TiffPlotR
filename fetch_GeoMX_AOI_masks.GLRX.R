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

rscope_chan_names = c(
    "Syto13",
    "PanCK",
    "GLRX",
    "TomatoRed"
)

ann = list()
ann$mask_points = alignment_data$all_mask_points

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
cache_dir = "cache_mask_results_aligned"
dir.create(cache_dir)

mask_points =  ann$mask_points
chan_names = rscope_chan_names
write.table(paste("starting:", date()), "progress.txt", col.names = FALSE, row.names = FALSE)
all_msk = future.apply::future_lapply(seq(nrow(mask_rects@coords)), future.globals = c("tf", "mask_rects", "mask_points", "chan_names", "fetchTiffDataMasked", "cache_dir"), function(i){
    message(i)
    write.table(paste(i, "start"), "progress.txt", append = TRUE, col.names = FALSE, row.names = FALSE)
    fetch_rect = mask_rects[i]
    cache_file = file.path(cache_dir, paste0(basename(tf), ".", fetch_rect$name, ".Rds"))
    cache_file = sub(":", "_", cache_file)
    if(!file.exists(cache_file)){
        tryCatch({
            write.table(paste(i, "try run"), "progress.txt", append = TRUE, col.names = FALSE, row.names = FALSE)
            msk = fetchTiffDataMasked(tf, mask_points = mask_points, rect = fetch_rect, resolution = 1, channel_names = chan_names)
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

