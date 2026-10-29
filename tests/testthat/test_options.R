library(testthat)
library(ggplot2)

rect <- TiffRect(900, 2300, 1400, 2800)
fetch1 <- function(...) suppressMessages(fetchTiffData(exampleTiff(), rect, selected_channels = 1, ...))
no_options <- list(TPR_MAX_PIXELS = NULL, TPR_CHANNEL_NAMES = NULL, TPR_THEME = NULL,
                   TPR_SCALE_BAR_STYLE = NULL, TPR_VERBOSE = NULL, TPR_UM_AXES = NULL,
                   TPR_UM_SCALE_BAR = NULL, TPR_SCALE_COLORS = NULL)

test_that("fetches record the settings they used", {
  withr::local_options(no_options)
  img <- fetch1(resolution = 4)
  pp <- img@plot_parameters
  expect_setequal(names(pp), c("scale_colors", "channel_names", "max_pixels", "um_axes", "scale_bar",
                               "scale_bar_style", "plot_theme"))
  expect_equal(pp$channel_names, "auto")
  expect_equal(pp$max_pixels, 800)
  expect_equal(pp$scale_bar_style, list())
  expect_null(pp$plot_theme)
})

test_that("TPR_MAX_PIXELS sets the automatic resolution", {
  withr::local_options(no_options)
  expect_equal(fetch1()@resolution, 2)       # 1400 px wide at full resolution, 700 at level 2
  withr::local_options(TPR_MAX_PIXELS = 200)
  img <- fetch1()
  expect_equal(img@resolution, 4)
  expect_equal(img@plot_parameters$max_pixels, 200)
  expect_equal(fetch1(max_pixels = 800)@resolution, 2)
  withr::local_options(TPR_MAX_PIXELS = -1)
  expect_error(fetch1(), "Invalid TPR_MAX_PIXELS option")
})

test_that("TPR_CHANNEL_NAMES sets the channel naming", {
  withr::local_options(no_options)
  expect_equal(levels(fetch1(resolution = 4)@data$channel_name), "DAPI")
  withr::local_options(TPR_CHANNEL_NAMES = "number")
  img <- fetch1(resolution = 4)
  expect_equal(levels(img@data$channel_name), "1")
  expect_equal(img@plot_parameters$channel_names, "number")
  withr::local_options(TPR_CHANNEL_NAMES = 1:6)
  expect_error(fetch1(resolution = 4), "Invalid TPR_CHANNEL_NAMES option")
})

test_that("TPR_THEME adds a theme to fetched and replotted plots", {
  withr::local_options(no_options)
  blank <- theme(panel.grid = element_blank())
  withr::local_options(TPR_THEME = blank)
  img <- fetch1(resolution = 4)
  expect_s3_class(img@plots[[1]]$theme$panel.grid, "element_blank")
  expect_equal(img@plot_parameters$plot_theme, blank)

  # replots use the recorded theme; an explicit one wins
  withr::local_options(TPR_THEME = NULL)
  expect_s3_class(plotTiffData(img)$theme$panel.grid, "element_blank")
  red_grid <- theme(panel.grid = element_line(colour = "red"))
  expect_equal(plotTiffData(img, plot_theme = red_grid)$theme$panel.grid$colour, "red")
  expect_equal(plotTiffData(img, plot_theme = theme(legend.position = "none"))$theme$legend.position, "none")

  # a recorded "no theme" is kept even if the option is set later
  img0 <- fetch1(resolution = 4)
  withr::local_options(TPR_THEME = blank)
  expect_null(plotTiffData(img0)$theme$panel.grid)
  expect_error(fetch1(resolution = 4, plot_theme = "minimal"), "plot_theme must be a ggplot2 theme")
})

test_that("TPR_SCALE_BAR_STYLE styles the scale bar", {
  withr::local_options(no_options)
  withr::local_options(TPR_SCALE_BAR_STYLE = list(line_color = "black", position = "topright"))
  img <- fetch1(resolution = 4, scale_bar = TRUE)
  bar <- img@plots[[1]]$layers[[2]]
  expect_equal(bar$aes_params$colour, "black")
  expect_equal(img@plot_parameters$scale_bar_style$position, "topright")
  # the style is kept with the object even with no bar drawn
  expect_equal(fetch1(resolution = 4)@plot_parameters$scale_bar_style$line_color, "black")

  expect_error(fetch1(resolution = 4, scale_bar_style = list(colour = "red")), "unknown entries: colour")
  expect_error(fetch1(resolution = 4, scale_bar_style = list("red")), "named list")
  withr::local_options(TPR_SCALE_BAR_STYLE = list(xlim = 1))
  expect_error(fetch1(resolution = 4), "Invalid TPR_SCALE_BAR_STYLE option")
})

test_that("TPR_VERBOSE silences fetch messages", {
  withr::local_options(no_options)
  expect_message(fetchTiffData(exampleTiff(), rect, resolution = 4, selected_channels = 1), "plotted image")
  expect_silent(fetchTiffData(exampleTiff(), rect, resolution = 4, selected_channels = 1, verbose = FALSE))
  withr::local_options(TPR_VERBOSE = FALSE)
  expect_silent(fetchTiffData(exampleTiff(), rect, selected_channels = 1))
  expect_silent(fetchTiffData.rgb(exampleTiff(), rect, resolution = 4, red_channel = 5, green_channel = 6,
                                  blue_channel = 1))
})
