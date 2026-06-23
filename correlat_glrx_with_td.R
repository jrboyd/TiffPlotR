library(tidyverse)
library(GeomxTools)

geo_dat = readRDS("../../projects_core/02172026_reem_geomx/02_geomx_prep_31-24_v3/00_project_31-24_geomx_data.Rds")
geo_meta = sData(geo_dat)
geo_meta$
geo_meta %>% filter(GLRX == "pos") %>% select(Roi, Slide.Name)

all_rds = dir("cache_mask_results_aligned/", full.names = TRUE)
names(all_rds) = basename(all_rds) %>% sub(".+tiff.", "", .) %>% sub(".Rds", "", .)
f = all_rds[1]

all_data = lapply(all_rds, function(f){
    img = readRDS(f)
    img$data
})

df = bind_rows(all_data)
df %>% head
dfw = df %>% select(channel, value, ROI_ID, Text, i, j) %>% pivot_wider(names_from = "channel", values_from = "value")
dfw %>% head


cor

dfw$Syto13 %>% hist(., breaks = 500)
syto_cutoff = 300
syto_high = 2.7e3
lines(rep(syto_cutoff, 2), c(0, 8e5), col= "red")
lines(rep(syto_high, 2), c(0, 8e5), col= "green")

k = dfw$Syto13 > syto_cutoff
sum(k)/length(k)
dfw = dfw %>% mutate(low_Syto = Syto13 <= syto_cutoff)
dfw$syto_group = "middle"
dfw$syto_group = cut(dfw$Syto13, breaks = c(-Inf, syto_cutoff, syto_high, Inf))
dfw = dfw %>% mutate(low_Syto = Syto13 <= syto_cutoff)

dfw$GLRX %>% log10 %>% hist
dfw$Syto13 %>% log10 %>% hist
dfw$TomatoRed %>% log10 %>% hist
dfw$PanCK %>% log10 %>% hist

dfw.plot = dfw[sample(nrow(dfw), 10e3),]

dfw.plot %>% pull(TomatoRed) %>% log10() %>% hist(., breaks = 50)
xbreaks = c(200, 500, 1.3e3)

lines(x = log10(rep(xbreaks, each = 2)), y = rep(c(0, 600), length(xbreaks)), col = "red")

dfw.plot$TomatoRed_group = cut(dfw.plot$TomatoRed, c(-Inf, xbreaks, Inf))

ggplot(dfw.plot %>% filter(syto_group == "(300,2.7e+03]"), aes(x = TomatoRed_group, y = GLRX, color = syto_group)) +
    geom_boxplot()

ggplot(dfw.plot, aes(x = GLRX, y = TomatoRed, color = syto_group)) +
    geom_point(alpha  = .1, pch = 16)


dfw.plot %>% filter(TomatoRed > 400) %>% filter(TomatoRed < 2e3) %>% pull(TomatoRed) %>% hist(breaks = 200)
dfw.mid = dfw.plot %>% filter(syto_group == "(300,2.7e+03]")
dfw.mid = dfw.mid %>% mutate(syto_bin = cut(Syto13, breaks = 10))
ggplot(dfw.mid %>% filter(TomatoRed < 2e3), aes(x = GLRX, y = TomatoRed)) +
    geom_point(alpha = .1, shape = 16) +
    facet_wrap(~syto_bin)

ggplot(dfw.mid %>% filter(TomatoRed < 2e3), aes(x = GLRX, y = TomatoRed)) +
    geom_point(alpha = .1, shape = 16) +
    facet_wrap(~syto_bin)


ggplot(dfw.mid %>% filter(TomatoRed < 2e3), aes(x = GLRX/Syto13, y = TomatoRed/Syto13)) +
    geom_point(alpha = .1, shape = 16)

head(df)
df.roi = df %>% group_by(ROI_ID, channel) %>% summarise(mean_value = mean(value), median_value = median(value))
# df.roi_plot = df.roi %>% pivot_wider(names_from = "channel", values_from = c("mean_value", "median_value"))
df.roi_plot = df.roi %>% select(-median_value) %>% pivot_wider(names_from = "channel", values_from = c("mean_value"))
df.tmp = df.roi_plot[, -1] %>% as.data.frame

rownames(df.tmp) = df.roi_plot$ROI_ID
df.tmp$`GLRX / Syto13` = df.tmp$GLRX / df.tmp$Syto13
df.tmp$`Tomato / Syto13` = df.tmp$TomatoRed / df.tmp$Syto13

plot(df.tmp[, 1:4])
plot(df.tmp)
plot(df.tmp %>% filter(Syto13 > 1e3))

ggplot(df.roi_plot, aes(x = mean_value_GLRX , y = mean_value_TomatoRed)) +
    geom_point()

# ROI_41
# TomatoRed_CD45_PanCK_3124
mask_data = readRDS("aligned_mask_points.Rds")
poly_ish = mask_data$all_poly$`Slide411_GLRX_31-24`@coords %>% filter(grepl("41", name))
poly_ish = mask_data$all_poly$`Slide411_GLRX_31-24`@coords %>% filter(grepl("39", name))
poly_ish = poly_ish %>% group_by(name) %>% reframe(x = unlist(x), y = unlist(y))
poly_rect = poly_ish %>% group_by(name) %>% summarise(xmin = min(x), xmax = max(x), ymin = min(y), ymax = max(y))
view_rects = TiffRect(poly_rect$xmin, poly_rect$xmax, poly_rect$ymin, poly_rect$ymax, poly_rect$name)
mask_data$all_rects$`Slide411_GLRX_31-24`

# ----------------------------------------------------------------------------
align_dir = "/mnt/c/project_data/UVMMIC-RA-13113/alignment/"
if(!dir.exists(align_dir)){
    align_dir = "D:/project_data/alignment"
    dir(align_dir)
}



tf = dir(align_dir, pattern = "Slide.+tiff", full.names = TRUE)
fetchTiffData.rgb(tf, rect = view_rects[1] %>% shape_resize_mult(2), blue_channel = 1, red_channel = 4, green_channel = 3)
fetchTiffData.rgb(tf, rect = view_rects[1] %>% shape_resize_mult(2), blue_channel = 1, red_channel = 4, green_channel = 3, quantile_norm = .99999)
fetchTiffData(tf, rect = view_rects[1] %>% shape_resize_mult(2), quantile_norm = .99999)
fetchTiffData.rgb(tf, rect = view_rects[2] %>% shape_resize_mult(2), blue_channel = 1, red_channel = 4, green_channel = 3)
fetchTiffData.rgb(tf, rect = TiffRect(12500, 13000, 28000, 28500), blue_channel = 1, red_channel = 4, green_channel = 3, channel_names = rscope_chan_names)
fetchTiffData(tf, rect = TiffRect(12500, 13000, 28000, 28500))
fetchTiffData(tf, rect = view_rects[2])
