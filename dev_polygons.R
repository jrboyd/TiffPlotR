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

tf <- tiff_files[["31'24.ome.tiff"]]



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
df = TiffPlotR:::.fetch_ome_nodes(ome_doc, "Channel", output = "data.frame")
View(df)
xml2::xml_structure(ome_chans)

ann$summary
ann$summary %>% filter(n > 0) %>% select(node_type, n) %>% print(n = Inf)

# ----------------------------------------------------------------------------
# Helper: parse OME Polygon Points "x1,y1 x2,y2 ..." into numeric vectors
# ----------------------------------------------------------------------------
parse_polygon_points <- function(points_chr) {
  pts <- strsplit(points_chr, " ", fixed = TRUE)[[1]]
  xy <- do.call(rbind, strsplit(pts, ",", fixed = TRUE))
  list(
    x = as.numeric(xy[, 1]),
    y = as.numeric(xy[, 2])
  )
}

# ----------------------------------------------------------------------------
# Build shape objects from annotation tables
# ----------------------------------------------------------------------------
label_tbl <- ann$labels %>%
  transmute(
    ROI_ID,
    ROI = dplyr::coalesce(Text, node_text),
    X_label = suppressWarnings(as.numeric(X)),
    Y_label = suppressWarnings(as.numeric(Y))
  )

polygon_tbl <- ann$polygons %>%
  mutate(Points = as.character(Points)) %>%
  filter(!is.na(Points), nzchar(Points))

poly_shape <- NULL
if (nrow(polygon_tbl) > 0) {
  parsed <- lapply(polygon_tbl$Points, parse_polygon_points)
  poly_shape <- TiffPolygon(
    x = lapply(parsed, `[[`, "x"),
    y = lapply(parsed, `[[`, "y"),
    name = dplyr::coalesce(polygon_tbl$Text, polygon_tbl$ROI_ID)
  )
}

ellipse_tbl <- ann$ellipses %>%
  mutate(
    X = suppressWarnings(as.numeric(X)),
    Y = suppressWarnings(as.numeric(Y)),
    RadiusX = suppressWarnings(as.numeric(RadiusX)),
    RadiusY = suppressWarnings(as.numeric(RadiusY))
  ) %>%
  filter(!is.na(X), !is.na(Y), !is.na(RadiusX), !is.na(RadiusY))

ellipse_shape <- NULL
if (nrow(ellipse_tbl) > 0) {
  ellipse_shape <- TiffEllipse(
    x0 = ellipse_tbl$X,
    y0 = ellipse_tbl$Y,
    radius_x = ellipse_tbl$RadiusX,
    radius_y = ellipse_tbl$RadiusY,
    name = dplyr::coalesce(ellipse_tbl$Text, ellipse_tbl$ROI_ID)
  )
}

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
