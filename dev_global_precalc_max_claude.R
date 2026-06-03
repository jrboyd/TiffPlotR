tiff_path = "inst/extdata/301_CellPellet_NegCTL_Scan1_PC3Guise.ome.tiff"

pm <- precalc_max_all_resolutions(tiff_path)

# Reuse for any future regions/resolutions/channels
obj1 <- fetchTiffData(tiff_path, rect = TiffRect(100, 800, 100, 800), precalc_max = pm)
obj2 <- fetchTiffData(tiff_path, rect = TiffRect(900, 1600, 900, 1600), resolution = 4, precalc_max = pm)
obj3 <- fetchTiffData.rgb(tiff_path, rect = TiffRect(500, 1400, 500, 1400), precalc_max = pm)


c(
    seq(0, 100)/100,
    .99 + seq(1, 9)/1000
) %>% sort


