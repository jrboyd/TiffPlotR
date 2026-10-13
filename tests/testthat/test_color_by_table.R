library(testthat)

.two_squares <- function() {
  TiffPolygon(x = c(0, 4, 4, 0, 10, 14, 14, 10), y = c(0, 0, 4, 4, 0, 0, 4, 4),
              name = c(rep("a", 4), rep("b", 4)))
}

test_that("assign_value_colors_by matches on a key column", {
  tab <- data.frame(id = c("b", "a", "z"), type = c("Immune", "Tumor", "Other"))
  res <- assign_value_colors_by(.two_squares(), tab, "type", key = "id")
  m <- shape_meta(res)
  expect_equal(m$value[match(c("a", "b"), m$name)], c("Tumor", "Immune"))
})

test_that("assign_value_colors_by matches on rownames and reports misses", {
  tab <- data.frame(score = c(1, 5), row.names = c("a", "x"))
  expect_message(res <- assign_value_colors_by(.two_squares(), tab, "score", na_color = "pink"),
                 "1/2 shapes not found")
  m <- shape_meta(res)
  expect_equal(m$fill[m$name == "b"], "pink")
})

test_that("assign_value_colors_by validates columns", {
  tab <- data.frame(id = "a", type = "T")
  expect_error(assign_value_colors_by(.two_squares(), tab, "nope", key = "id"), "column 'nope'")
  expect_error(assign_value_colors_by(.two_squares(), tab, "type", key = "nope"), "key 'nope'")
})

test_that(".category_palette auto, named and positional overrides", {
  expect_equal(unname(.category_palette(c("x", "y"))), .set1_colors[1:2])
  expect_equal(length(unique(.category_palette(letters[1:12]))), 12)
  pal <- .category_palette(c("x", "y", "z"), c(z = "black", q = "white"))
  expect_equal(names(pal), c("x", "y", "z"))
  expect_equal(unname(pal), c(.set1_colors[1:2], "black"))
  expect_equal(unname(.category_palette(c("x", "y"), "black")), c("black", .set1_colors[2]))
})

test_that("assign_value_colors defaults are unchanged by the shared palette", {
  res <- assign_value_colors(.two_squares(), c(a = "T", b = "I"))
  expect_equal(attr(shape_meta(res), "color_map")$fill, .set1_colors[1:2])
})
