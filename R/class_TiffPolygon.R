#' Polygon definition S4 class
#'
#' Stores polygons as list-columns of x/y vertices.
#'
#' @slot coords data.frame with x/y/name columns where x and y are list-columns
#' @export
setClass(
    "TiffPolygon",
    contains = "TiffShape",
    validity = function(object) {
        required_cols <- c("x", "y", "name")
        if (!all(required_cols %in% names(object@coords))) return("coords must include x, y, and name columns")
        if (!is.list(object@coords$x) || !is.list(object@coords$y)) {
            return("x and y must be list-columns of numeric vertex vectors")
        }

        for (i in seq_len(nrow(object@coords))) {
            xi <- object@coords$x[[i]]
            yi <- object@coords$y[[i]]
            if (!is.numeric(xi) || !is.numeric(yi)) return("Each polygon x and y entry must be numeric")
            if (length(xi) != length(yi)) return("Each polygon must have equal x and y vertex counts")
            if (length(xi) < 3) return("Each polygon must have at least 3 vertices")
        }
        TRUE
    }
)


#' Construct a TiffPolygon
#'
#' @param x numeric vector of x vertices or list of numeric vectors
#' @param y numeric vector of y vertices or list of numeric vectors
#' @param name optional name(s). Recycled when length 1.
#' @return a `TiffPolygon` object
#' @export
TiffPolygon <- function(x, y, name = NULL) {
    reduce_names = FALSE
    if(length(x) == length(name)){
        x = split(x, name)
        reduce_names = TRUE
    }
    if(length(y) == length(name)){
        y = split(y, name)
        reduce_names = TRUE
    }

    x_list <- if (is.list(x)) x else list(x)
    y_list <- if (is.list(y)) y else list(y)

    n <- max(length(x_list), length(y_list))
    if (!(length(x_list) %in% c(1L, n)) || !(length(y_list) %in% c(1L, n))) {
        stop("x and y must each have length 1 or match the longest input")
    }

    recycle_list <- function(v) if (length(v) == 1L && n > 1L) rep(v, n) else v
    x_list <- recycle_list(x_list)
    y_list <- recycle_list(y_list)

    stopifnot(setequal(names(x_list), names(y_list)))
    stopifnot(names(x_list) == names(y_list))

    if(reduce_names){
        name = names(x_list)
    }
    if(is.null(name)){
        if(!is.null(names(x_list))){
            name = names(x_list)
        }else{
            name = "polygon"
        }
    }

    name <- as.character(name)
    if (!(length(name) %in% c(1L, n))) stop("name must be length 1 or match polygon count")
    if (length(name) == 1L && n > 1L) name <- rep(name, n)

    coords_df <- data.frame(name = .unique_shape_names(name), stringsAsFactors = FALSE)
    coords_df$x <- x_list
    coords_df$y <- y_list
    new("TiffPolygon", coords = coords_df)
}

#' @rdname TiffPolygon
#' @param object A TiffPolygon object
setMethod("show", "TiffPolygon", function(object) {
    n <- nrow(object@coords)
    cat("TiffPolygon with", n, if (n == 1) "polygon\n" else "polygons\n")
    out <- object@coords
    out$n_points <- vapply(out$x, length, integer(1))
    out$x <- NULL
    out$y <- NULL
    print(out, row.names = FALSE)
})

#' @rdname TiffPolygon
#' @param x A TiffPolygon object
setMethod("names", "TiffPolygon", function(x) {
    c("x", "y", "name", "n_points")
})

#' @rdname TiffPolygon
#' @param name Name of slot to be replaced/retrieved
setMethod("$", "TiffPolygon", function(x, name) {
    switch(
        name,
        x = x@coords$x,
        y = x@coords$y,
        name = x@coords$name,
        n_points = vapply(x@coords$x, length, integer(1))
    )
})

#' @rdname TiffPolygon
#' @param x TiffPolygon object
#' @param i integer, logical, or character index
#' @param j unused
#' @param drop unused
#' @return a `TiffPolygon` containing only the selected rows
#' @export
setMethod("[", signature(x = "TiffPolygon"),
          function(x, i, j, ..., drop = FALSE) {
              new_coords <- x@coords[i, , drop = FALSE]
              new_meta   <- if (nrow(x@meta) > 0L) {
                  x@meta[x@meta$name %in% new_coords$name, , drop = FALSE]
              } else {
                  x@meta
              }
              new("TiffPolygon", coords = new_coords, meta = new_meta)
          })

#' Combine `TiffPolygon` objects
#'
#' @param ... one or more `TiffPolygon` objects
#' @return a single `TiffPolygon` with all rows combined
#' @export
setMethod("c", signature(x = "TiffPolygon"),
          function(x, ...) {
              parts <- c(list(x), list(...))
              if (!all(vapply(parts, is, logical(1), "TiffPolygon"))) {
                  stop("all arguments to c() must be TiffPolygon objects")
              }
              combined <- do.call(rbind, lapply(parts, function(r) r@coords))
              rownames(combined) <- NULL
              old_names <- combined$name
              combined$name <- .unique_shape_names(old_names)
              combined_meta <- .combine_shape_meta(parts, old_names, combined$name)
              new("TiffPolygon", coords = combined, meta = combined_meta)
          })
