library(testthat)

rect <- TiffRect(1, 1024, 1, 1024)
ome_names <- c("DAPI", "Opal 520", "Opal 570", "Opal 620", "Opal 690", "Autofluorescence")
cn <- c("DAPI", paste("channel", LETTERS[1:5]))

test_that("read_tiff_channels reads OME names, RGBA colors and other attributes", {
  ch <- read_tiff_channels(exampleTiff())
  expect_equal(ch$channel, 1:6)
  expect_equal(ch$name, ome_names)
  # OME Color is RGBA: 65535 = 0x0000FFFF is opaque blue
  expect_equal(ch$color[1:5], c("#0000FF", "#00FF00", "#FFFF00", "#FF8000", "#FF0000"))
  expect_equal(ch$ID[1], "Channel:0:0")
  expect_type(ch$SamplesPerPixel, "integer")
})

test_that(".ome_color_to_hex handles signed values and missing colors", {
  expect_equal(.ome_color_to_hex(c("-16776961", NA, "")), c("#FF0000", NA, NA))
})

test_that(".channel_table validates names", {
  tf <- exampleTiff()
  # "auto" (the default) uses the OME names, which this TIFF has for every channel
  expect_equal(.channel_table(tf)$channel_name, ome_names)
  expect_equal(.channel_table(tf, NULL)$channel_name, ome_names)
  expect_equal(.channel_table(tf, "number")$channel_name, as.character(1:6))
  expect_equal(.channel_table(tf, "ome")$channel_name, ome_names)
  expect_equal(.channel_table(tf, cn)$ome_name, ome_names)
  expect_error(.channel_table(tf, cn[1:5]), "Not enough")
  expect_warning(.channel_table(tf, c(cn, "extra")), "Too many")
  expect_error(.channel_table(tf, c("a", "a", cn[3:6])), "unique")
  expect_error(.channel_table(tf, c(NA, cn[2:6])), "missing")
})

test_that(".resolve_channels takes numbers, display names and OME names", {
  tab <- .channel_table(exampleTiff(), cn)
  expect_equal(.resolve_channels(c(3, 1), tab), c(3L, 1L))
  expect_equal(.resolve_channels(c("channel B", "Opal 690"), tab), c(3L, 5L))
  expect_equal(.resolve_channels(factor("DAPI"), tab), 1L)
  expect_null(.resolve_channels(NULL, tab))
  expect_error(.resolve_channels("nope", tab, arg = "selected_channels"), "Unknown selected_channels: nope")
  expect_error(.resolve_channels(7, tab), "Unknown")
  expect_error(.resolve_channels(c(1, 1), tab), "only be given once")
  expect_equal(.resolve_channels(c(1, 1), tab, allow_duplicates = TRUE), c(1L, 1L))
})

test_that("fetchTiffData keeps channel numeric and adds channel_name in plot order", {
  img <- fetchTiffData(exampleTiff(), rect = rect, resolution = 4, channel_names = cn,
                       selected_channels = c("channel E", "DAPI"))
  d <- img@data
  expect_type(d$channel, "integer")
  expect_equal(sort(unique(d$channel)), c(1L, 6L))
  expect_equal(levels(d$channel_name), c("channel E", "DAPI"))
  expect_true(all(d$channel_name[d$channel == 6] == "channel E"))
  expect_equal(img@channels$channel_name, cn)
  expect_equal(img@channels$ome_name, ome_names)
  expect_equal(img$channels, img@channels)

  # numeric selection without names, and OME names
  img2 <- fetchTiffData(exampleTiff(), rect = rect, resolution = 4, selected_channels = c(5, 1),
                        channel_names = "number")
  expect_equal(levels(img2@data$channel_name), c("5", "1"))
  img2 <- fetchTiffData(exampleTiff(), rect = rect, resolution = 4, selected_channels = c(5, 1))
  expect_equal(levels(img2@data$channel_name), ome_names[c(5, 1)])
  expect_equal(img2@plot_parameters$channel_names, "auto")
  img3 <- fetchTiffData(exampleTiff(), rect = rect, resolution = 4, channel_names = "ome",
                        selected_channels = "Opal 570")
  expect_equal(levels(img3@data$channel_name), "Opal 570")
  expect_equal(unique(img3@data$channel), 3L)
})

test_that("precalc_max can be keyed by name, including OME names", {
  pm <- data.frame(channel = c("DAPI", "Opal 520"), min_value = 0, max_value = c(5, 1))
  img <- fetchTiffData(exampleTiff(), rect = rect, resolution = 4, precalc_max = pm,
                       selected_channels = 1:2)
  expect_equal(img@precalc_max$channel, c(1, 2))
  expect_error(fetchTiffData(exampleTiff(), rect = rect, resolution = 4,
                             precalc_max = data.frame(channel = "nope", min_value = 0, max_value = 1)),
               "Unknown precalc_max channels")
})

test_that("bad channel names fail before the image is read", {
  expect_error(fetchTiffData(exampleTiff(), rect = rect, selected_channels = "nope"),
               "Unknown selected_channels")
})

test_that("rgb channels can be given by name", {
  img <- fetchTiffData.rgb(exampleTiff(), rect = rect, resolution = 4, channel_names = cn,
                           red_channel = "channel E", green_channel = "Opal 570", blue_channel = "DAPI")
  expect_equal(img@plots$rgb$meta$tiffplotr_composite$channels, c(6, 3, 1))
  leg <- img@plots$rgb$layers[[2]]$data
  expect_equal(leg$channel_name, c("channel E", "channel B", "DAPI"))
  expect_type(img@data$channel, "integer")
  expect_equal(img@channels$channel_name, cn)
})

test_that("colorized channel_colors resolve OME names without channel_names", {
  img <- fetchTiffData.colorized(exampleTiff(), rect = rect, resolution = 4,
                                 channel_colors = list(DAPI = "blue", "Opal 570" = "yellow"))
  cmap <- img@plots$colorized$meta$tiffplotr_composite$channel_map
  expect_equal(cmap$channel, c(1L, 3L))
  # labels are the display names: the OME names under the default "auto"
  expect_equal(cmap$channel_name, c("DAPI", "Opal 570"))
  expect_error(fetchTiffData.colorized(exampleTiff(), rect = rect, resolution = 4,
                                       channel_colors = list(nope = "blue")),
               "Unknown channel_colors names")
})

test_that("constructors default @channels to numbered channels from data", {
  df <- data.frame(i = 1, j = 1, channel = c(3L, 1L, 3L), value = 1, norm_value = 1)
  obj <- TiffPlotData(df, plots = list(p = ggplot()))
  expect_equal(obj@channels$channel, c(1L, 3L))
  expect_equal(obj@channels$channel_name, c("1", "3"))
  expect_true(all(is.na(obj@channels$ome_name)))
  expect_equal(TiffPlotDataMasked(df, plots = list(p = ggplot()))@channels, obj@channels)

  # no numeric channel column: nothing to build from
  expect_equal(nrow(TiffPlotData(data.frame(x = 1), plots = list(p = ggplot()))@channels), 0)
  expect_equal(nrow(TiffPlotData(data.frame(channel = "a"), plots = list(p = ggplot()))@channels), 0)
  # an explicit table is kept
  tab <- data.frame(channel = 1L, channel_name = "DAPI")
  expect_equal(TiffPlotData(df, plots = list(p = ggplot()), channels = tab)@channels, tab)
})

test_that("renormalize works on a hand-built object with default @channels", {
  df <- data.frame(i = 1:4, j = 1, channel = rep(1:2, 2), value = c(0, 5, 10, 20), norm_value = 0)
  obj <- TiffPlotData(df, plots = list(p = ggplot(df)),
                      precalc_max = data.frame(channel = 1:2, min_value = 0, max_value = 10))
  obj2 <- renormalize(obj, channels = "2", max_value = 20)
  expect_equal(obj2@data$norm_value[obj2@data$channel == 2], c(0.25, 1))
})

test_that(".auto_channel_names uses unique OME names, else channel N", {
  expect_equal(.auto_channel_names(c("DAPI", NA, ""), 1:3), c("DAPI", "channel 2", "channel 3"))
  # duplicated OME names can't identify a channel
  expect_equal(.auto_channel_names(c("A", "A", "B"), 1:3), c("channel 1", "channel 2", "B"))
  # an OME name that collides with a fallback name: number everything
  expect_equal(.auto_channel_names(c("channel 2", NA), 1:2), c("channel 1", "channel 2"))
})
