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

ann$masks
ann$mask_points
dir()

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
img <- fetchTiffData(tf)
img$
base_plot <- img@plots[[img@activePlot]] + coord_fixed()

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
