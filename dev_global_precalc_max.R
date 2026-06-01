library(TiffPlotR)
library(tidyverse)
tf = exampleTiff()

img_meta_df = read_tiff_meta_data(tf)
sel_size = img_meta_df %>% filter(sizeX == min(sizeX))

img_data = fetchTiffData(tf)
fetchTiffData.rgb(tf)
img_data@data

qmax = .999

arr = fetchTiffArray(tf, max_pixels = 1)
dim(arr)
?quantile
dim(arr)[1:2] %>% prod()
apply(arr, 3, quantile, probs = c(.5, .9, .95, .99, .999, .9999, .99999, 1))
#starting at lowest resolution
# for each channel
# retrieve signal near previous resolution high signal regions OR for entire image if lowest resolution
# identify n high signal regions
# calculate cutoff
# increment next lowest resolution
arr@metadata$coreMetadata
arr@metadata$globalMetadata
arr@metadata$seriesMetadata

img_data = RBioFormats::read.image(
    tf,
    normalize = FALSE,
    series = 1,
    resolution = resolution,
    subset = list(
        X = seq(x_start*x_ratio, (x_start + x_width)*x_ratio),
        Y = seq(y_start*y_ratio, (y_start + y_width)* y_ratio)
    ))


hist(arr[,,1])
hist(arr[,,2])

x = arr[,,1]
hist_res = lapply(seq(dim(arr)[3]), function(i){
    x = arr[,,i]

    rng = range(x)
    brks = seq(rng[1], rng[2], length.out = 50)

    zero_count = sum(x == 0)
    zero_pct = round(100*zero_count/length(x), 2)

    binned = cut(x[x>0], breaks = brks, include.lowest = TRUE)
    tab_res = table(binned)
    tib = tibble(count = tab_res, bin_range = names(tab_res))
    tib$bin_range = factor(tib$bin_range, levels = levels(binned))
    tib = tib %>%
        mutate(xmin = brks[as.numeric(bin_range)])  %>%
        mutate(xmax = brks[as.numeric(bin_range)+1])
    tib$channel = i
    tib
})
hist_res = hist_res %>% bind_rows()

ggplot(hist_res, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = count)) +
    geom_rect() +
    facet_wrap(~channel, scales = "free") +
    labs(subtitle = paste(zero_pct, "% zeroes removed"))


my_arr_hist = function(x){
    arr_dim = dim(x)
    stopifnot(length(arr_dim) == 3)
    layout(matrix())
    a

}

hist(arr[,,3], breaks = 500)
#


