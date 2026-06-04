tiff_path = "inst/extdata/301_CellPellet_NegCTL_Scan1_PC3Guise.ome.tiff"
tiff_path = "c://Users/boydj/project_data/EBV_image_files/RNAScopeRound1/D-EB-13.ome.tiff"

time = system.time({
    qtile_df = collect_channel_quantiles_all_resolutions(tiff_path)
})
debug(collect_channel_quantiles_all_resolutions)
time = system.time({
    qtab <- collect_channel_quantiles_all_resolutions(
        tiff_path,
        max_full_fetch = 5e6
    )
})

library(tidyverse)
p_q = ggplot(qtile_df, aes(x = prob, y = value)) +
    geom_path() +
    facet_grid(channel~resolution, scales = "free_y")


p_q + coord_cartesian(xlim = c(.98, 1))

quantiles_to_precalc_max

precalc_max_all_resolutions()
pm <- precalc_max_all_resolutions(tiff_path)

# Reuse for any future regions/resolutions/channels
obj1 <- fetchTiffData(tiff_path, rect = TiffRect(100, 800, 100, 800), precalc_max = pm)
obj2 <- fetchTiffData(tiff_path, rect = TiffRect(900, 1600, 900, 1600), resolution = 4, precalc_max = pm)
obj3 <- fetchTiffData.rgb(tiff_path, rect = TiffRect(500, 1400, 500, 1400), precalc_max = pm)




c(
    seq(0, 100)/100,
    .99 + seq(1, 9)/1000
) %>% sort


