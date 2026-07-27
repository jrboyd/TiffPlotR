#' Assign colours so spatially adjacent shapes rarely share a colour
#'
#' Builds a proximity graph over shape centroids (two shapes are neighbours when
#' their centroids are within `dist_thresh`) and greedily colours it
#' (Welsh-Powell: highest-degree first) using `palette`. The chosen colours are
#' written to the shape's metadata `fill` column so `shape_annotate()` picks
#' them up. Pure base R (no spatial dependencies).
#'
#' @param shape A `TiffShape` (typically a `TiffPolygon` of cells).
#' @param palette Character vector of colours. Default is an 8-colour set.
#' @param dist_thresh Centroid distance (in shape coordinate units) below which
#'   two shapes are treated as adjacent. Default: `2.5 *` the typical centroid
#'   spacing, `sqrt(bbox_area / n)`.
#' @param fill,border Whether to write the colour to the `fill` and/or `color`
#'   metadata columns.
#' @return `shape` with its `@meta` populated (`name`, and `fill`/`color`).
#' @export
assign_adjacent_colors <- function(shape, palette = NULL, dist_thresh = NULL,
                                    fill = TRUE, border = FALSE) {
  if (!is(shape, "TiffShape")) stop("shape must be a TiffShape", call. = FALSE)
  if (is.null(palette))
    palette <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
                 "#ff7f00", "#ffff33", "#a65628", "#f781bf")
  P <- length(palette)
  nm <- shape@coords$name
  n  <- length(nm)

  bb <- .shape_bbox(shape)
  cx <- (bb$xmin + bb$xmax) / 2
  cy <- (bb$ymin + bb$ymax) / 2

  if (n <= 1L) {
    meta <- data.frame(name = nm, stringsAsFactors = FALSE)
    if (fill)   meta$fill  <- palette[1L]
    if (border) meta$color <- palette[1L]
    shape@meta <- meta; validObject(shape); return(shape)
  }

  if (is.null(dist_thresh)) {
    area <- max(1, (max(cx) - min(cx))) * max(1, (max(cy) - min(cy)))
    dist_thresh <- 2.5 * sqrt(area / n)
  }
  cell <- dist_thresh

  # --- spatial grid buckets -> neighbour list within dist_thresh ---
  gx <- floor((cx - min(cx)) / cell)
  gy <- floor((cy - min(cy)) / cell)
  ncol_g <- max(gx) + 1
  bucket <- gx + gy * ncol_g
  by_bucket <- split(seq_len(n), bucket)
  d2 <- dist_thresh^2

  neighbours <- vector("list", n)
  for (b in names(by_bucket)) {
    idx <- by_bucket[[b]]
    bnum <- as.integer(b)
    bgx <- bnum %% ncol_g; bgy <- bnum %/% ncol_g
    cand <- integer(0)
    for ( dx in -1:1) for (dy in -1:1) {
      key <- as.character((bgx + dx) + (bgy + dy) * ncol_g)
      if (!is.null(by_bucket[[key]])) cand <- c(cand, by_bucket[[key]])
    }
    for (i in idx) {
      dd <- (cx[cand] - cx[i])^2 + (cy[cand] - cy[i])^2
      nb <- cand[dd <= d2 & cand != i]
      neighbours[[i]] <- nb
    }
  }

  # --- greedy Welsh-Powell colouring ---
  deg <- lengths(neighbours)
  ord <- order(deg, decreasing = TRUE)
  col <- integer(n)
  for (v in ord) {
    used <- col[neighbours[[v]]]; used <- used[used > 0]
    avail <- setdiff(seq_len(P), used)
    col[v] <- if (length(avail)) avail[1L] else which.min(tabulate(used, nbins = P))
  }

  # report residual conflicts (adjacent pairs sharing a colour)
  conf <- 0L; tot <- 0L
  for (i in seq_len(n)) {
    nb <- neighbours[[i]][neighbours[[i]] > i]
    tot <- tot + length(nb)
    conf <- conf + sum(col[nb] == col[i])
  }
  message(sprintf("assign_adjacent_colors: %d shapes, %d adjacencies, %d colours, %.3f%% same-colour edges",
                  n, tot, P, if (tot) 100 * conf / tot else 0))

  meta <- data.frame(name = nm, stringsAsFactors = FALSE)
  if (fill)   meta$fill  <- palette[col]
  if (border) meta$color <- palette[col]
  shape@meta <- meta
  validObject(shape)
  shape
}
