tf = exampleTiff()

img_meta_df = read_tiff_meta_data(tf)
sel_size = img_meta_df %>% filter(sizeX == min(sizeX))

img_data = fetchTiffData(tf)
img_data@data

fetchTiffArray(tf)

img_data = RBioFormats::read.image(
    tf,
    normalize = FALSE,
    series = 1,
    resolution = resolution,
    subset = list(
        X = seq(x_start*x_ratio, (x_start + x_width)*x_ratio),
        Y = seq(y_start*y_ratio, (y_start + y_width)* y_ratio)
    ))

#


