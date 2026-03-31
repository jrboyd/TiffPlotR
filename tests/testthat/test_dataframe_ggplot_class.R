library(testthat)
library(ggplot2)

test_that("TiffPlotData class can be instantiated with metadata slots", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  p1 <- ggplot(df, aes(x = x, y = y)) + geom_point()
  r <- TiffRect(0, 10, 0, 10)
  img_info <- data.frame(sizeX=100, sizeY=100, sizeC=3, resolutionLevel=1)

  obj <- new("TiffPlotData",
             data = df,
             plots = list(scatter = p1),
             activePlot = "scatter",
             tiff_path = "foo.tiff",
             resolution = 2,
             precalc_max = data.frame(channel=1, min_value=0, max_value=1),
             rect = r,
             img_info = img_info)

  expect_s4_class(obj, "TiffPlotData")
  expect_identical(obj@data, df)
  expect_length(obj@plots, 1)
  expect_equal(obj@activePlot, "scatter")
  expect_equal(obj@tiff_path, "foo.tiff")
  expect_equal(obj@resolution, 2)
  expect_equal(obj@precalc_max$channel, 1)
  expect_s4_class(obj@rect, "TiffRect")
  expect_equal(obj@img_info$sizeX, 100)
})

test_that("TiffPlotData accepts multi-row TiffRect metadata", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  p1 <- ggplot(df, aes(x = x, y = y)) + geom_point()
  r <- TiffRect(c(0, 20), c(10, 30), c(0, 40), c(10, 50), name = c("roi1", "roi2"))

  obj <- new("TiffPlotData",
             data = df,
             plots = list(scatter = p1),
             activePlot = "scatter",
             tiff_path = "foo.tiff",
             resolution = 2,
             precalc_max = data.frame(channel=1, min_value=0, max_value=1),
             rect = r,
             img_info = data.frame())

  expect_s4_class(obj@rect, "TiffRect")
  expect_equal(nrow(obj@rect@coords), 2)
  expect_equal(obj@rect@coords$name, c("roi1", "roi2"))
})




# constructor helper tests

test_that("TiffPlotData() helper wraps new() and provides defaults", {
  df <- data.frame(x = 1:3, y = 4:6)
  p <- ggplot(df, aes(x,y)) + geom_point()
  obj <- TiffPlotData(df, list(one = p), activePlot = "one")
  expect_s4_class(obj, "TiffPlotData")
  expect_equal(obj@activePlot, "one")
  expect_equal(obj@tiff_path, NA_character_)
  expect_true(is.data.frame(obj@precalc_max) && nrow(obj@precalc_max)==0)
  expect_s4_class(obj@rect, "TiffRect")
  expect_true(is.data.frame(obj@img_info) && nrow(obj@img_info)==0)

  # default activePlot uses first plot name
  obj2 <- TiffPlotData(df, list(foo = p, bar = p))
  expect_equal(obj2@activePlot, "foo")
})

test_that("TiffPlotData requires activePlot to be in plots names", {
  df <- data.frame(x = 1:5, y = 1:5)
  p1 <- ggplot(df, aes(x = x, y = y)) + geom_point()

  expect_error(
    new("TiffPlotData",
        data = df,
        plots = list(scatter = p1),
        activePlot = "nonexistent",
        tiff_path = "x", resolution = 1, precalc_max = data.frame(), rect = TiffRect(0,1,0,1)),
    "must be a name in the plots list"
  )
})

test_that("TiffPlotData requires activePlot to be a single character", {
  df <- data.frame(x = 1:5, y = 1:5)
  p1 <- ggplot(df, aes(x = x, y = y)) + geom_point()

  expect_error(
    new("TiffPlotData",
        data = df,
        plots = list(scatter = p1),
        activePlot = c("scatter", "line"),
        tiff_path = "x", resolution = 1, precalc_max = data.frame(), rect = TiffRect(0,1,0,1)),
    "must be a single character string"
  )
})

test_that("show method displays object information", {
  df <- data.frame(x = 1:5, y = 1:5)
  p1 <- ggplot(df, aes(x = x, y = y)) + geom_point()
  p2 <- ggplot(df, aes(x = x, y = y)) + geom_line()
  r <- TiffRect(0,1,0,1)

  obj <- new("TiffPlotData",
             data = df,
             plots = list(scatter = p1, line = p2),
             activePlot = "scatter",
             tiff_path = "foo", resolution = 1, precalc_max = data.frame(), rect = r)

  expect_output(show(obj), "TiffPlotData object")
  expect_output(show(obj), "Number of plots: 2")
  expect_output(show(obj), "Active plot: scatter")
  expect_output(show(obj), "scatter, line")
  expect_output(show(obj), "TIFF path: foo")
  expect_output(show(obj), "Resolution: 1")
})

create_empty_dobj = function(){
  df <- data.frame(x = 1:5, y = rnorm(5))
  p1 <- ggplot(df, aes(x = x, y = y)) + geom_point()
  p2 <- ggplot(df, aes(x = x, y = y)) + geom_line()
  p3 <- ggplot(df, aes(x = x, y = y, group = x)) + geom_boxplot()

  obj = TiffPlotData(data = df, plots = list(scatter = p1, line = p2, box = p3), activePlot = "box")
  obj
}

test_that("plot method displays the active plot", {
  obj = create_empty_dobj()

  # Capture the plot output
  expect_silent(plot(obj))
})

test_that("Multiple plots can be stored in the list", {
  obj = create_empty_dobj()
  expect_length(obj@plots, 3)
  expect_equal(names(obj@plots), c("scatter", "line", "box"))
  expect_equal(obj@activePlot, "box")
})

test_that("$ operator accesses slots", {
  df <- data.frame(x = 1:5, y = rnorm(5))
  p1 <- ggplot(df, aes(x = x, y = y)) + geom_point()
  r <- TiffRect(0,1,0,1)

  obj <- new("TiffPlotData",
             data = df,
             plots = list(scatter = p1),
             activePlot = "scatter",
             tiff_path = "foo", resolution = 1, precalc_max = data.frame(), rect = r)

  expect_identical(obj$data, df)
  expect_length(obj$plots, 1)
  expect_equal(obj$activePlot, "scatter")
  expect_equal(obj$tiff_path, "foo")
  expect_equal(obj$resolution, 1)
  expect_s4_class(obj$rect, "TiffRect")
  expect_true(is.data.frame(obj$img_info))
})

test_that("$ operator accesses data.frame columns", {
  df <- data.frame(x = 1:5, y = rnorm(5))
  p1 <- ggplot(df, aes(x = x, y = y)) + geom_point()
  r <- TiffRect(0,1,0,1)
  obj <- new("TiffPlotData",
             data = df,
             plots = list(scatter = p1),
             activePlot = "scatter",
             tiff_path = "foo", resolution = 1, precalc_max = data.frame(), rect = r)

  expect_equal(obj$x, 1:5)
  expect_equal(obj$y, df$y)
})

test_that("$<- operator sets slots", {
  df <- data.frame(x = 1:5, y = rnorm(5))
  p1 <- ggplot(df, aes(x = x, y = y)) + geom_point()
  p2 <- ggplot(df, aes(x = x, y = y)) + geom_line()
  r <- TiffRect(0,1,0,1)

  obj <- new("TiffPlotData",
             data = df,
             plots = list(scatter = p1),
             activePlot = "scatter",
             tiff_path = "foo", resolution = 1, precalc_max = data.frame(), rect = r)

  # Change the data
  new_df <- data.frame(x = 1:10, y = rnorm(10))
  obj$data <- new_df
  expect_identical(obj$data, new_df)

  # Change the plots
  obj$plots <- list(scatter = p1, line = p2)
  expect_length(obj$plots, 2)

  # Change the active plot
  obj$activePlot <- "line"
  expect_equal(obj$activePlot, "line")

  # Change metadata slots
  obj$tiff_path <- "bar"
  expect_equal(obj$tiff_path, "bar")
  obj$resolution <- 5
  expect_equal(obj$resolution, 5)
  new_pm <- data.frame(channel=2,min_value=0,max_value=2)
  obj$precalc_max <- new_pm
  expect_equal(obj$precalc_max$channel, 2)
  new_r <- TiffRect(1,2,3,4)
  obj$rect <- new_r
  expect_equal(obj$rect@coords$xmin[[1]], 1)
  new_r_multi <- TiffRect(c(1, 5), c(2, 6), c(3, 7), c(4, 8), name = c("r1", "r2"))
  obj$rect <- new_r_multi
  expect_equal(nrow(obj$rect@coords), 2)
  expect_equal(obj$rect@coords$name, c("r1", "r2"))
  new_info <- data.frame(sizeX=10,sizeY=10,sizeC=1,resolutionLevel=1)
  obj$img_info <- new_info
  expect_equal(obj$img_info$sizeY, 10)
})

test_that("$<- operator sets data.frame columns", {
  df <- data.frame(x = 1:5, y = rnorm(5))
  p1 <- ggplot(df, aes(x = x, y = y)) + geom_point()
  r <- TiffRect(0,1,0,1)
  obj <- new("TiffPlotData",
             data = df,
             plots = list(scatter = p1),
             activePlot = "scatter",
             tiff_path = "foo", resolution = 1, precalc_max = data.frame(), rect = r)

  # Add a new column
  obj$z <- 5:1
  expect_equal(obj$z, 5:1)
  expect_true("z" %in% names(obj$data))

  # Modify an existing column
  obj$x <- 10:14
  expect_equal(obj$x, 10:14)
})

test_that("$ operator raises error for non-existent names", {
  df <- data.frame(x = 1:5, y = rnorm(5))
  p1 <- ggplot(df, aes(x = x, y = y)) + geom_point()
  r <- TiffRect(0,1,0,1)
  obj <- new("TiffPlotData",
             data = df,
             plots = list(scatter = p1),
             activePlot = "scatter",
             tiff_path = "foo", resolution = 1, precalc_max = data.frame(), rect = r)

  expect_error(obj$nonexistent, "not found")
})

# if example tiff is available the plotting helpers should populate img_info
if (requireNamespace("RBioFormats", quietly = TRUE)) {
  test_that("plot_tiff_image returns img_info slot", {
    tiff_path <- exampleTiff()
    obj <- TiffPlotR:::.fetch_tiff_data(tiff_path, x_start=0, x_width=1, y_start=0, y_width=1,
                            resolution=1, max_pixels=10)
    expect_s4_class(obj, "TiffPlotData")
    expect_true(nrow(obj@img_info) > 0)
  })
}

