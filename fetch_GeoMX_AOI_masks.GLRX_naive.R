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




# tf = "/mnt/c/project_data/UVMMIC-RA-13113/GLRX RNAscope/Slide 411_ Exp 31'24.ome.tiff"
# tf = "/mnt/c/project_data/UVMMIC-RA-13113/GLRX RNAscope images/Slide 411_ Exp 31'24.ome.tiff"
# tf = "/mnt/c/project_data/UVMMIC-RA-13113/GLRX RNAscope images/Slide 410_Exp29'24.ome.tiff"
# tf = "/mnt/c/project_data/UVMMIC-RA-13113/LXlI4oj06AJKJfOF3Ug9/TomatoRed_CD45_PanCK_3124.ome_.tiff"


# these 2 might be the same file
# tf = "/mnt/c/project_data/UVMMIC-RA-13113/FinalScansAndROIs/31'24.ome.tiff"
tf.gmx1 = tiff_files$TomatoRed_PanCk_CD45_3124.ome_.tiff
tf.gmx2 = tiff_files$TomatoRed_CD45_PanCK_3124.ome_.tiff

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

tf = tiff_files$`Slide411_GLRX_31-24.ome.tiff`
tf.test = tiff_files$`Slide411_GLRX_31-24.ome.tiff`
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
theme_set(theme(panel.background = element_blank(), panel.grid = element_blank()))
p.gmx = p.gmx + labs(title = tf.gmx1 %>% basename)
p.gmx2 = p.gmx2 + labs(title = tf.gmx2 %>% basename)
p.glrx.r = p.glrx.r + labs(title = tf %>% basename)
pw = p.gmx + p.gmx2 + p.glrx.r

pw + patchwork::plot_annotation(title = "no alignment", theme = theme(title = element_text(size = 15)))
ggsave("alignment_plots/images_no_alignment.pdf", width = 12, height = 8)
