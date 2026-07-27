#' Colour shapes by a per-shape value categorical or continuous
#'
#' Maps a value per shape row to a fill colour and writes it (plus the raw
#' `value`) to the shape's metadata, so `shape_annotate()` uses it directly.
#' Categorical values (character/factor, or numeric with `continuous = FALSE`)
#' map to discrete palette colours; numeric values map along a colour gradient.
#' `NA` values get `na_color`. Pure base R.
#'
#' @param shape A `TiffShape`.
#' @param values Vector of values, either length `nrow(shape@coords)` in shape
#'   order, or a named vector keyed by shape name (matched by name).
#' @param palette For categorical: discrete colours (recycled with a warning if
#'   too few). For continuous: gradient control points passed to
#'   `colorRampPalette`. `NULL` picks sensible defaults.
#' @param continuous `TRUE`/`FALSE` to force scale type; `NULL` auto-detects
#'   (numeric & non-factor -> continuous).
#' @param na_color Colour for `NA` values.
#' @param fill,border Write colour to `fill` and/or `color` metadata columns.
#' @return `shape` with `@meta` columns `name`, `value`, and `fill`/`color`.
#'   The value->colour mapping is attached as `attr(shape@meta, "color_map")`
#'   (a data.frame) for building legends.
#' @export
#' @examples
#' poly <- TiffPolygon(
#'   x = c(0, 4, 4, 0,  10, 14, 14, 10,  20, 24, 24, 20),
#'   y = c(0, 0, 4, 4,   0,  0,  4,  4,   0,  0,  4,  4),
#'   name = c(rep("a", 4), rep("b", 4), rep("c", 4))
#' )
#' # categorical: values named by shape name
#' by_type <- assign_value_colors(poly, c(a = "Tumor", b = "Immune", c = "Tumor"))
#' shape_meta(by_type)
#' attr(shape_meta(by_type), "color_map")
#' # continuous: numeric values map along a gradient
#' by_val <- assign_value_colors(poly, c(a = 1, b = 5, c = 9))
#' shape_meta(by_val)
assign_value_colors <- function(shape, values, palette = NULL, continuous = NULL,
                                na_color = "grey80", fill = TRUE, border = FALSE) {
  if (!is(shape, "TiffShape")) stop("shape must be a TiffShape", call. = FALSE)
  nm <- shape@coords$name
  n  <- length(nm)

  if (!is.null(names(values))) values <- values[nm]
  if (length(values) != n)
    stop("values must have length nrow(shape) or be a vector named by shape name", call. = FALSE)

  if (is.null(continuous)) continuous <- is.numeric(values) && !is.factor(values)

  color_map <- NULL
  if (continuous) {
    if (is.null(palette)) palette <- c("#2c7bb6", "#ffffbf", "#d7191c")
    ramp <- grDevices::colorRampPalette(palette)(256L)
    v <- as.numeric(values)
    rng <- range(v, na.rm = TRUE)
    span <- diff(rng)
    idx <- if (is.finite(span) && span > 0) round((v - rng[1]) / span * 255) + 1L else rep(1L, n)
    cols <- ramp[idx]
    cols[is.na(v)] <- na_color
    value_out <- v
    color_map <- data.frame(low = rng[1], high = rng[2],
                            col_low = ramp[1], col_high = ramp[256],
                            stringsAsFactors = FALSE)
  } else {
    f <- as.character(values)
    ulev <- sort(unique(f[!is.na(f)]))
    k <- length(ulev)
    if (is.null(palette)) {
      palette <- if (k <= 8L)
        c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
          "#ff7f00", "#ffff33", "#a65628", "#f781bf")[seq_len(max(k, 1L))]
      else grDevices::hcl.colors(k, "Dark 3")
    }
    if (length(palette) < k) {
      warning("palette has fewer colours than categories; recycling", call. = FALSE)
      palette <- rep(palette, length.out = k)
    }
    map <- stats::setNames(palette[seq_len(k)], ulev)
    cols <- unname(map[f])
    cols[is.na(f)] <- na_color
    value_out <- f
    color_map <- data.frame(value = ulev, fill = unname(map[ulev]), stringsAsFactors = FALSE)
  }

  meta <- data.frame(name = nm, value = value_out, stringsAsFactors = FALSE)
  if (fill)   meta$fill  <- cols
  if (border) meta$color <- cols
  attr(meta, "color_map") <- color_map
  shape@meta <- meta
  validObject(shape)
  shape
}
