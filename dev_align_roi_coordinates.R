library(TiffPlotR)
library(tidyverse)
align_dir = "/mnt/c/project_data/UVMMIC-RA-13113/alignment/"
if(!dir.exists(align_dir)){
    align_dir = "D:/project_data/alignment"
    dir(align_dir)
}
zip_files = dir(align_dir, pattern = "zip$", full.names = TRUE)
names(zip_files) = basename(zip_files) %>% sub(".ome.+", "", .)  %>% sub("_TMA_rect.+", "", .)
zip_files = as.list(zip_files)

tiff_files = dir(align_dir, pattern = "tiff$", full.names = TRUE)
names(tiff_files) = basename(tiff_files) %>% sub(".ome.+", "", .)  %>% sub("_TMA_rect.+", "", .)
tiff_files = as.list(tiff_files)


stopifnot(setequal(names(zip_files), names(tiff_files)))

all_rois = lapply(zip_files, readImageJRois)
x = all_rois$`Slide411_GLRX_31-24`

#roi were defined at resolution 2 - expand to full resolution coordinates
exp_factor = 2
all_rects = lapply(all_rois, function(x){
    r = x$rects
    r@coords$xmin = exp_factor*r@coords$xmin
    r@coords$xmax = exp_factor*r@coords$xmax
    r@coords$ymin = exp_factor*r@coords$ymin
    r@coords$ymax = exp_factor*r@coords$ymax
    rnk_val = r@coords$xmin + 10*r@coords$ymin
    r@coords$name = paste0("roi_", order(rnk_val))
    r
})

#reverse slide411
r = all_rects$`Slide411_GLRX_31-24`
r@coords$name = rev(r@coords$name)
all_rects$`Slide411_GLRX_31-24` = r


all_imgd.no_rect = list()
for(name in names(tiff_files)){
    message(name)
    imgd = fetchTiffData.rgb(tiff_files[[name]])
    all_imgd.no_rect[[name]] = imgd
}



all_imgd = list()
for(name in names(all_imgd.no_rect)){
    message(name)
    imgd = all_imgd.no_rect[[name]]
    r = all_rects[[name]]
    xy_df = r@coords %>% mutate(x = (xmin + xmax) / 2, y = (ymin + ymax)/2, num = name %>% sub(".+_", "", .) %>% as.numeric())
    xy_df = xy_df %>% arrange(num)
    imgd = imgd %>% shape_annotate(all_rects[[name]]) %>% shape_name_annotate(all_rects[[name]], color = "green")
    imgd@plots$rgb.annotated = imgd@plots$rgb.annotated + annotate("path", x= xy_df$x, y = xy_df$y, color = "red")
    all_imgd[[name]] = imgd
}

theme_set(theme(panel.background = element_blank(), panel.grid = element_blank()))
#verify correct order
cowplot::plot_grid(nrow = 1,
                   plot(all_imgd$`Slide411_GLRX_31-24`) + guides(fill = "none", color = "none") + labs(title = "Slide411_GLRX_31-24"),
                   plot(all_imgd$TomatoRed_CD45_PanCK_3124) + guides(fill = "none", color = "none") + labs(title = "TomatoRed_CD45_PanCK_3124-24"),
                   plot(all_imgd$TomatoRed_PanCk_CD45_3124) + guides(fill = "none", color = "none") + labs(title = "TomatoRed_PanCk_CD45_3124-24")
)
out_dir = "alignment_plots"
ggsave(file.path(out_dir, "images_confirm_TMA_order.pdf"), width = 12, height = 8)

ref_coords = all_rects$TomatoRed_PanCk_CD45_3124@coords
target_coords = all_rects$`Slide411_GLRX_31-24`@coords



convert_coordinates = function(ref_rects, target_rects, invert_x = FALSE, invert_y = FALSE){
    ref_coords = ref_rects@coords
    target_coords = target_rects@coords
    #in this case, x and y are both flipped
    if(invert_x){
        ref_coords = ref_coords %>% group_by(name) %>% rename(xmin = xmax, xmax = xmin)
    }
    if(invert_y){
        ref_coords = ref_coords %>% group_by(name) %>% rename(ymin = ymax, ymax = ymin)
    }
    ref_coords = ref_coords %>% group_by(name) %>% select(ref_xmin = xmin, ref_xmax = xmax, ref_ymin = ymin, ref_ymax = ymax)


    target_coords = target_coords %>% group_by(name) %>% mutate(target_xmin = xmin, target_xmax = xmax, target_ymin = ymin, target_ymax = ymax, .keep = "none")
    m_coords = merge(ref_coords, target_coords, by = "name")
    pred_coords = rbind(
        m_coords %>% select(name, ref_x = ref_xmin, target_x = target_xmin, ref_y = ref_ymin, target_y = target_ymin),
        m_coords %>% select(name, ref_x = ref_xmin, target_x = target_xmin, ref_y = ref_ymax, target_y = target_ymax),
        m_coords %>% select(name, ref_x = ref_xmax, target_x = target_xmax, ref_y = ref_ymin, target_y = target_ymin),
        m_coords %>% select(name, ref_x = ref_xmax, target_x = target_xmax, ref_y = ref_ymax, target_y = target_ymax)
    )

    model_x <- lm(target_x ~ ref_x + ref_y, data = pred_coords)
    model_x2 = lm(target_x ~ ref_x + ref_y + ref_x*ref_y, data = pred_coords)
    model_x3 = lm(target_x ~ ref_x + ref_y + ref_x*ref_y + ref_x^2 + ref_y^2, data = pred_coords)

    model_y <- lm(target_y ~ ref_x + ref_y, data = pred_coords)
    model_y2 = lm(target_y ~ ref_x + ref_y + ref_x*ref_y, data = pred_coords)
    model_y3 = lm(target_y ~ ref_x + ref_y + ref_x*ref_y + ref_x^2 + ref_y^2, data = pred_coords)


    pred_coords$pred_x <- predict(model_x, newdata = pred_coords)
    pred_coords$pred_x2 <- predict(model_x2, newdata = pred_coords)
    pred_coords$pred_x3 <- predict(model_x3, newdata = pred_coords)
    pred_coords$pred_y <- predict(model_y, newdata = pred_coords)
    pred_coords$pred_y2 <- predict(model_y2, newdata = pred_coords)
    pred_coords$pred_y3 <- predict(model_y3, newdata = pred_coords)

    pred_coords


    pred_coords %>% head
    plot_coords = rbind(
        pred_coords %>% select(name, target_x, target_y, x = ref_x, y = ref_y) %>% mutate(group = "ref"),
        pred_coords %>% select(name, target_x, target_y, x = pred_x, y = pred_y) %>% mutate(group = "pred1"),
        pred_coords %>% select(name, target_x, target_y, x = pred_x2, y = pred_y2) %>% mutate(group = "pred2"),
        pred_coords %>% select(name, target_x, target_y, x = pred_x3, y = pred_y3) %>% mutate(group = "pred3")
    )


    library(ggpmisc)

    px = ggplot(plot_coords, aes(x = target_x, y = x, color = group, group = group)) +
        geom_point() +
        stat_poly_line() +
        stat_poly_eq(rr.digits = 10)

    px

    py = ggplot(plot_coords, aes(x = target_y, y = y, color = group, group = group)) +
        geom_point() +
        stat_poly_line() +
        stat_poly_eq(rr.digits = 10)

    py

    return(list(model_x = model_x2, model_y = model_y2, plot_x = px,  plot_y = py))
}

res1 = convert_coordinates(all_rects$TomatoRed_PanCk_CD45_3124, all_rects$`Slide411_GLRX_31-24`, invert_x = TRUE, invert_y = TRUE)
res1$plot_x
res2 = convert_coordinates(all_rects$TomatoRed_CD45_PanCK_3124, all_rects$`Slide411_GLRX_31-24`, invert_x = TRUE, invert_y = TRUE)
res2$plot_x
res3 = convert_coordinates(all_rects$TomatoRed_CD45_PanCK_3124, all_rects$`TomatoRed_PanCk_CD45_3124`)
res3$plot_x

# saveRDS(res1, "convert.TomatoRed_PanCk_CD45_3124.to.Slide411_GLRX_31-24.Rds")
# saveRDS(res2, "convert.TomatoRed_CD45_PanCK_3124.to.Slide411_GLRX_31-24.Rds")
# saveRDS(res3, "convert.TomatoRed_CD45_PanCK_3124.to.TomatoRed_PanCk_CD45_3124.Rds")

converters = list(
    `TomatoRed_PanCk_CD45_3124` = list(`Slide411_GLRX_31-24` = res1),
    `TomatoRed_CD45_PanCK_3124` = list(`Slide411_GLRX_31-24` = res2, `TomatoRed_PanCk_CD45_3124` = res3)
)

#  x in Slide411 = predict(res1$model_x, x in TomatoRed_PanCk_CD45_3124)
#  y in Slide411 = predict(res1$model_y, y in TomatoRed_PanCk_CD45_3124)


# as a test, import ROI from GeomX to GLRX
all_ann = lapply(tiff_files, fetchTiffAnnotations)
all_ann$TomatoRed_PanCk_CD45_3124$polygons
all_ann$TomatoRed_PanCk_CD45_3124$ellipses

createPolygonsFromAnnotations = function(ann){
    if(is.data.frame(ann)){
        ann = list(polygons = ann)
    }
    stopifnot(is.list(ann))
    stopifnot("polygons" %in% names(ann))
    poly_df = ann$polygons
    if(nrow(poly_df) == 0) return(NULL)
    poly_df = poly_df %>% group_by(ROI_ID) %>% reframe(.extract_poly_points(Points))
    # TiffPolygon(split(poly_df$x, poly_df$ROI_ID), split(poly_df$y, poly_df$ROI_ID))
    # debug(TiffPolygon)
    TiffPolygon(poly_df$x, poly_df$y, poly_df$ROI_ID)
}
.extract_poly_points = function(x){
    strsplit(strsplit(x, " ")[[1]], ",") %>% unlist %>% as.numeric %>% matrix(byrow = TRUE, ncol = 2) %>% as.data.frame %>% rename(x = V1, y = V2)
}

createEllipsesFromAnnotations = function(ann){
    if(is.data.frame(ann)){
        ann = list(polygons = ann)
    }
    stopifnot(is.list(ann))
    stopifnot("ellipses" %in% names(ann))
    poly_df = ann$ellipses
    if(nrow(poly_df) == 0) return(NULL)
    TiffEllipse(poly_df$X %>% as.numeric, poly_df$Y %>% as.numeric, radius_x = poly_df$RadiusX %>% as.numeric, name = poly_df$ROI_ID)
}

ann = all_ann$TomatoRed_CD45_PanCK_3124

all_poly = lapply(all_ann, createPolygonsFromAnnotations)
all_ellipse = lapply(all_ann, createEllipsesFromAnnotations)

ref_name = "TomatoRed_PanCk_CD45_3124"
target_name = "Slide411_GLRX_31-24"

ref_img_anno = all_imgd.no_rect[[ref_name]] %>%
    shape_annotate(all_poly[[ref_name]]) %>%
    shape_annotate(all_ellipse[[ref_name]])

ref_img_anno = (ref_img_anno %>% plot) +
    guides(fill = "none", color = "none") +
    labs(title = ref_name)
ref_img_anno
ggsave(file.path(out_dir, paste0("image_self_annotated_", ref_name, ".pdf")),
       width = 4, height = 8)

model_x = converters[[ref_name]][[target_name]]$model_x
model_y = converters[[ref_name]][[target_name]]$model_y

#### apply models to polygons ####
.predict_polygons = function(ref_name, target_name){
    stopifnot(exists("converters"))
    stopifnot(exists("all_poly"))
    model_x = converters[[ref_name]][[target_name]]$model_x
    model_y = converters[[ref_name]][[target_name]]$model_y

    poly_df = all_poly[[ref_name]]@coords %>% group_by(name) %>% reframe(x = unlist(x), y = unlist(y))
    poly_df = poly_df %>% rename(ref_x = x, ref_y = y)
    poly_df$x = predict(model_x, newdata = poly_df)
    poly_df$y = predict(model_y, newdata = poly_df)

    TiffPolygon(poly_df$x, poly_df$y, poly_df$name)
}

# poly_df = all_poly[[ref_name]]@coords %>% group_by(name) %>% reframe(x = unlist(x), y = unlist(y))
# poly_df = poly_df %>% rename(ref_x = x, ref_y = y)
# poly_df$x = predict(model_x, newdata = poly_df)
# poly_df$y = predict(model_y, newdata = poly_df)
#
# all_poly[[target_name]] = TiffPolygon(poly_df$x, poly_df$y, poly_df$name)

s1 = .predict_polygons("TomatoRed_CD45_PanCK_3124", target_name)
s2 = .predict_polygons("TomatoRed_PanCk_CD45_3124", target_name)
s1@coords$name = paste0("1_", s1@coords$name)
s2@coords$name = paste0("2_", s2@coords$name)

all_poly[[target_name]] = c(s1, s2)


#### apply models to ellipses ####
.approx_ellipse = function(x, y, rx, ry = rx, n = 12, just_plot = FALSE, first_plot = FALSE){
    stopifnot(length(x) == 1)
    stopifnot(length(y) == 1)
    stopifnot(length(rx) == 1)
    stopifnot(length(ry) == 1)
    stopifnot(length(n) == 1)
    # Set parameters for the ellipse
    h <- x              # x-coordinate of the center
    k <- y              # y-coordinate of the center
    a <- rx              # Length of the semi-major axis
    b <- ry              # Length of the semi-minor axis
    # n <- 50             # Number of points to generate

    # Generate n points along the parametric circle/ellipse
    theta <- seq(0, 2 * pi, length.out = n)
    x <- h + a * cos(theta)
    y <- k + b * sin(theta)

    if(just_plot){
        # Plot the approximated ellipse
        if(first_plot){
            plot(x, y, type = "l", col = "blue", asp = 1,
                 main = "Approximated Ellipse", xlab = "X", ylab = "Y")
        }else{
            lines(x, y, col = "blue")
        }
        points(x, y, col = "red", pch = 19)
    }else{
        list(x, y)
    }
}
.approx_ellipse(1, 0, 5, just_plot = TRUE, first_plot = TRUE)
.approx_ellipse(5, -2, 2, n = 50, just_plot = TRUE)
.approx_ellipse(1, 0, 3, n = 10, just_plot = TRUE)

.predict_ellipse = function(ref_name, target_name){
    stopifnot(exists("converters"))
    stopifnot(exists("all_ellipse"))
    model_x = converters[[ref_name]][[target_name]]$model_x
    model_y = converters[[ref_name]][[target_name]]$model_y

    ellipse_df = all_ellipse[[ref_name]]@coords %>% group_by(name) %>% reframe({res = .approx_ellipse(x0, y0, radius_x, n = 21); data.frame(x = res[[1]], y = res[[2]])})
    ellipse_df = ellipse_df %>% rename(ref_x = x, ref_y = y)
    ellipse_df$x = predict(model_x, newdata = ellipse_df)
    ellipse_df$y = predict(model_y, newdata = ellipse_df)

    TiffPolygon(ellipse_df$x, ellipse_df$y, ellipse_df$name)
}


e1 = .predict_ellipse("TomatoRed_CD45_PanCK_3124", target_name)
e2 = .predict_ellipse("TomatoRed_PanCk_CD45_3124", target_name)
e1@coords$name = paste0("1_", e1@coords$name)
e2@coords$name = paste0("2_", e2@coords$name)
names(all_ellipse)
all_ellipse[[target_name]] = c(
    e1,
    e2
)


# ellipse_df = all_ellipse[[ref_name]]@coords %>% group_by(name) %>% reframe({res = .approx_ellipse(x0, y0, radius_x, n = 21); data.frame(x = res[[1]], y = res[[2]])})
# ellipse_df = ellipse_df %>% rename(ref_x = x, ref_y = y)
# ellipse_df$x = predict(model_x, newdata = ellipse_df)
# ellipse_df$y = predict(model_y, newdata = ellipse_df)

# all_ellipse[[target_name]] = TiffPolygon(split(ellipse_df$x, ellipse_df$name), split(ellipse_df$y, ellipse_df$name))

#### apply model to TMA rects ####
ref_r = all_rects[[ref_name]]@coords
ref_poly = ref_r %>% group_by(name) %>% reframe(ref_x = c(xmin, xmin, xmax, xmax), ref_y = c(ymin, ymax, ymax, ymin))
ref_poly$x = predict(model_x, newdata = ref_poly)
ref_poly$y = predict(model_y, newdata = ref_poly)

ref_poly = TiffPolygon(split(ref_poly$x, ref_poly$name), split(ref_poly$y, ref_poly$name))

target_img_anno = all_imgd.no_rect[[target_name]] %>%
    # shape_annotate(ref_poly) %>%
    shape_annotate(all_ellipse[[target_name]]) %>%
    shape_annotate(all_poly[[target_name]])

ggsave(file.path(out_dir, paste0("image_self_annotated_", ref_name, ".pdf")),
       width = 4, height = 8)

ref_names = names(all_imgd.no_rect)[2:3]
ref_plots = lapply(ref_names, function(ref_name){
    ref_img_anno = all_imgd.no_rect[[ref_name]] %>%
        shape_annotate(all_poly[[ref_name]]) %>%
        shape_annotate(all_ellipse[[ref_name]])
    ref_plot = ref_img_anno %>% plot
    ref_plot = ref_plot +
        guides(fill = "none", color = "none") +
        labs(title = ref_name)

})

target_plot = all_imgd.no_rect[[target_name]] %>% plot  %>%
    shape_annotate(all_poly[[target_name]]) %>%
    shape_annotate(all_ellipse[[target_name]])
target_plot = target_plot + scale_x_reverse() + scale_y_continuous()
target_plot = target_plot +
    guides(fill = "none", color = "none") +
    labs(title = target_name, subtitle = paste0("annotation aligned from:\n  ", paste0(ref_names, collapse = "\n  ")))


# cowplot::plot_grid(ref_plots[[1]], ref_plots[[2]], target_plot, nrow = 1)
library(patchwork)
ref_plots[[1]] + ref_plots[[2]] + target_plot

ggsave(file.path(out_dir, paste0("image_alignment_annotated_", target_name, ".pdf")),
       width = 12, height = 8)


# according to:
converters$TomatoRed_CD45_PanCK_3124$TomatoRed_PanCk_CD45_3124$model_x
converters$TomatoRed_CD45_PanCK_3124$TomatoRed_PanCk_CD45_3124$model_y
# geomx are shifted 430 pixels x and 14 pixels y
sapply(all_ellipse$`Slide411_GLRX_31-24`@coords$x, function(x){diff(range(x))})
sapply(all_poly$`Slide411_GLRX_31-24`@coords$x, function(x){diff(range(x))})
sapply(all_ellipse$`Slide411_GLRX_31-24`@coords$y, function(x){diff(range(x))})
sapply(all_poly$`Slide411_GLRX_31-24`@coords$y, function(x){diff(range(x))})
# ROI are 400-1000 pixels


.predict_mask = function(ref_name, target_name){
    stopifnot(exists("converters"))
    stopifnot(exists("all_ann"))
    model_x = converters[[ref_name]][[target_name]]$model_x
    model_y = converters[[ref_name]][[target_name]]$model_y

    mask_points = TiffPlotR::decode_ome_masks(all_ann[[ref_name]]$masks) %>% select(ROI_ID, FillColor, ref_x = i, ref_y = j, Text)

    mask_points$x = predict(model_x, newdata = mask_points)
    mask_points$y = predict(model_y, newdata = mask_points)
    mask_points %>% rename(i = x, j = y)
}



s1 = .predict_mask("TomatoRed_CD45_PanCK_3124", target_name)
s2 = .predict_mask("TomatoRed_PanCk_CD45_3124", target_name)

s1$ROI_ID = paste0("1_", s1$ROI_ID)
s2$ROI_ID = paste0("2_", s2$ROI_ID)

all_mask_points = rbind(s1, s2)
all_mask_points %>% head


# target_data = fetchTiffData(tiff_files$`Slide411_GLRX_31-24`, rect = TiffRect(15e3, 25e3, 3e3, 13e3), channel_names = rscope_chan_names)

# old bad order
# rscope_chan_names = c("Syto13", "PanCK", "GLRX", "TomatoRed")

rscope_chan_names = c("Syto13", "PanCK", "TomatoRed", "GLRX")
view_rect = TiffRect(18e3, 25e3, 7.5e3, 14e3)
qmax = .985
p_mask_global = target_plot %>% shape_annotate(view_rect, color = "red") %>% plot
p_mask_global = p_mask_global + scale_x_continuous() + scale_y_reverse()


target_data = fetchTiffData(
    tiff_files$`Slide411_GLRX_31-24`,
    rect = view_rect,
    channel_names = rscope_chan_names, quantile_norm = qmax)
p_mask_roi = target_data %>%
    shape_annotate(all_ellipse[[target_name]]) %>%
    shape_annotate(all_poly[[target_name]]) %>%
    plot
p_mask_roi = p_mask_roi + guides(fill = "none")



target_masked = fetchTiffDataMasked(
    tiff_files$`Slide411_GLRX_31-24`,
    mask_points = all_mask_points,
    rect = view_rect,
    channel_names = rscope_chan_names, quantile_norm = qmax)

#this didn't look better
# target_masked.res1 = fetchTiffDataMasked(
#     tiff_files$`Slide411_GLRX_31-24`,
#     resolution = 1,
#     mask_points = all_mask_points,
#     rect = view_rect,
#     channel_names = rscope_chan_names, quantile_norm = qmax)
#
# ggplot(target_masked.res1@data, aes(x = Text, y = norm_value)) +
#     geom_boxplot() +
#     facet_grid(channel~., scales = "free_y") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

res1_means = target_masked@data %>% tibble %>% group_by(channel, Text, ROI_ID) %>% summarise(norm_value = mean(norm_value), value = mean(value))

ggplot(res1_means, aes(x = Text, y = value, group = ROI_ID)) +
    ggbeeswarm::geom_beeswarm() +
    facet_grid(channel~., scales = "free_y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    geom_path()

target_masked %>% class
p_mask_assigned = target_masked@plots$masked %>%
    shape_annotate(all_ellipse[[target_name]]) %>%
    shape_annotate(all_poly[[target_name]]) %>%
    plot
p_mask_assigned = p_mask_assigned + guides(fill = "none")
target_masked$data %>% head
p_mask_box = ggplot(target_masked@data, aes(x = Text, y = norm_value)) +
    geom_boxplot() +
    facet_grid(channel~., scales = "free_y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
p_mask_assigned = p_mask_assigned + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
pc1 = (p_mask_global /p_mask_roi)
pw = wrap_plots(pc1, p_mask_assigned, p_mask_box, nrow = 1)
pw + plot_layout(width = c(.8, 1, .3))

ggsave(file.path(out_dir, paste0("image_alignment_masked_", target_name, ".pdf")),
       width = 16, height = 10)


saveRDS(list(all_mask_points = all_mask_points, all_rects = all_rects, all_ellipse = all_ellipse, all_poly = all_poly, converters = converters),
        "aligned_mask_points.Rds")
