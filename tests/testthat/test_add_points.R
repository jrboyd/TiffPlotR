library(testthat)

.legend_plot <- function() {
  ggplot2::ggplot() +
    ggplot2::geom_point(data = data.frame(k = factor(c("A", "B"))), ggplot2::aes(colour = k),
                        x = NA, y = NA, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = c(A = "red", B = "blue"))
}

test_that("add_points leaves sibling copies' colour guide untouched", {
  skip_if_not_installed("ggnewscale")
  p <- .legend_plot()
  p1 <- p
  before <- names(p1$guides$guides)
  q <- add_points(p, data.frame(x = 1:3, y = 1:3, g = c("a", "b", "a")), colour_by = "g")
  expect_identical(names(p1$guides$guides), before)
  expect_identical(names(p$guides$guides), before)
  expect_s3_class(q, "ggplot")
  expect_silent(ggplot2::ggplot_build(q))
  expect_equal(sum(vapply(q$scales$scales, function(s) "colour_ggnewscale_1" %in% s$aesthetics || "colour" %in% s$aesthetics, logical(1))), 2)
})

test_that("add_points on a colorized image keeps a buildable plot", {
  skip_if_not_installed("ggnewscale")
  rect <- TiffRect(0, 100, 0, 100)
  img <- fetchTiffData.colorized(exampleTiff(), rect = rect,
                                 channel_colors = list("red", "green", "blue"))
  p <- img@plots[[1]]
  q <- add_points(p, data.frame(x = c(10, 20), y = c(10, 20), gene = c("A", "B")), colour_by = "gene")
  expect_silent(ggplot2::ggplot_build(p))
  expect_silent(ggplot2::ggplot_build(q))
})

test_that("add_points without an existing colour scale skips ggnewscale", {
  q <- add_points(ggplot2::ggplot(), data.frame(x = 1:3, y = 1:3, g = c("a", "b", "a")), colour_by = "g")
  expect_equal(length(q$scales$scales), 1)
})

test_that("add_points level order, palette override, and plain points", {
  df <- data.frame(px = 1:4, py = 1:4, gene = c("b", "a", "a", "c"))
  q <- add_points(ggplot2::ggplot(), df, x = "px", y = "py", colour_by = "gene",
                  palette = c(c = "black"))
  sc <- q$scales$get_scales("colour")
  expect_equal(names(sc$palette(3)), c("a", "b", "c"))
  expect_equal(unname(sc$palette(3)["c"]), "black")
  expect_equal(levels(q$layers[[1]]$data$gene), c("a", "b", "c"))

  q2 <- add_points(ggplot2::ggplot(), df, x = "px", y = "py")
  expect_equal(q2$layers[[1]]$aes_params$colour, "white")
  expect_error(add_points(ggplot2::ggplot(), df, x = "nope"), "columns not in df")
  expect_message(add_points(ggplot2::ggplot(), df[0, ]), "no points")
})
