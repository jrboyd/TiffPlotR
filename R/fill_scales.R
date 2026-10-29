# Fill layers shared by the image plots. Single-channel plots map a value onto
# a colour gradient; rgb/colorized composites carry precomputed colours in chex.

# viridisLite::magma(6), the palette scale_fill_viridis_c(option = "magma")
# interpolates, so it reproduces plots made before scale_colors existed.
.magma_scale_colors = function(){
  c("#000004", "#3B0F70", "#8C2981", "#DE4968", "#FE9F6D", "#FCFDBF")
}

# Default for every scale_colors argument: the TPR_SCALE_COLORS option when
# set (see ?TiffPlotR), otherwise magma.
.default_scale_colors = function(){
  opt = getOption("TPR_SCALE_COLORS")
  if(is.null(opt)) return(.magma_scale_colors())
  tryCatch(.check_scale_colors(opt), error = function(e){
    stop("Invalid TPR_SCALE_COLORS option: ", conditionMessage(e), call. = FALSE)
  })
}

.check_scale_colors = function(scale_colors){
  if(!is.character(scale_colors) || length(scale_colors) < 2 || anyNA(scale_colors)){
    stop("scale_colors must be a character vector of at least 2 colours.")
  }
  ok = vapply(scale_colors, function(cl) !inherits(try(grDevices::col2rgb(cl), silent = TRUE), "try-error"),
              logical(1))
  if(!all(ok)) stop("scale_colors has invalid colours: ", paste(scale_colors[!ok], collapse = ", "))
  unname(scale_colors)
}

# Raster plus gradient fill for plots whose fill aesthetic is a value.
.fill_gradient_layers = function(scale_colors = .default_scale_colors()){
  list(geom_raster(),
       scale_fill_gradientn(colours = scale_colors))
}

# Raster of precomputed colours (rgb_df$chex) plus an identity fill scale.
.fill_identity_layers = function(rgb_df){
  list(.identity_raster(rgb_df),
       scale_fill_identity())
}

.identity_raster = function(rgb_df){
  geom_raster(data = rgb_df, aes(x = i, y = j, fill = chex))
}
