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
#' by_val <- assign_value_colors(poly, c(a = 1, b = 5, c = 9), border = TRUE, fill = TRUE)
#' library(ggplot2)
#' ggplot() %>% shape_annotate(by_val)
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
      palette <- unname(.category_palette(ulev))
      if (k == 0L) palette <- .set1_colors[1]
    }
    if (length(palette) < k) {
      warning("palette has fewer colours than categories; recycling", call. = FALSE)
      palette <- rep(palette, length.out = k)
    }
    if(is.null(names(palette))){
      map <- stats::setNames(palette[seq_len(k)], ulev)
      
      
    }else{
      #user supplied palette with names
      if(!all(ulev %in% names(palette))){
        message("bad values: ", setdiff(ulev, names(palette)) %>% paste(collapse = ","))
        stop("values outside of supplied palette names detected!")
      }
      map = palette
    }
    
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


#' Colour shapes by a column of a table
#'
#' Looks up each shape's name in a key of `table` and colours the shape by
#' `table[[column]]` via [assign_value_colors()]. Shapes with no matching row
#' get `na_color`, and a message reports how many there were.
#'
#' @param shape A `TiffShape`.
#' @param table A data.frame (or data.table/tibble) with one row per shape id.
#' @param column Name of the column of `table` to colour by.
#' @param key Name of the column of `table` holding shape names. `NULL` uses
#'   `rownames(table)` (e.g. a Seurat object's `@meta.data`).
#' @param ... Passed to [assign_value_colors()] (`palette`, `continuous`,
#'   `na_color`, `fill`, `border`).
#' @return `shape` with colour metadata, as for [assign_value_colors()].
#' @export
#' @examples
#' poly <- TiffPolygon(
#'   x = c(0, 4, 4, 0,  10, 14, 14, 10),
#'   y = c(0, 0, 4, 4,   0,  0,  4,  4),
#'   name = c(rep("a", 4), rep("b", 4))
#' )
#' cells <- data.frame(id = c("a", "b"), type = c("Tumor", "Immune"))
#' shape_meta(assign_value_colors_by(poly, cells, "type", key = "id"))
assign_value_colors_by <- function(shape, table, column, key = NULL, ...) {
  if (!is(shape, "TiffShape")) stop("shape must be a TiffShape", call. = FALSE)
  table <- as.data.frame(table)
  if (!column %in% colnames(table)) stop("column '", column, "' not in table", call. = FALSE)
  if (is.null(key)) {
    ids <- rownames(table)
  } else {
    if (!key %in% colnames(table)) stop("key '", key, "' not in table", call. = FALSE)
    ids <- as.character(table[[key]])
  }
  nm <- shape@coords$name
  hit <- match(nm, ids)
  n_na <- sum(is.na(hit))
  if (n_na) message(n_na, "/", length(nm), " shapes not found in table")
  assign_value_colors(shape, table[[column]][hit], ...)
}
