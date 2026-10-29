# Package options (documented in ?TiffPlotR, R/TiffPlotR-package.R). Each
# getter returns the option's value, validated, or the default when unset.
# TPR_SCALE_COLORS is in R/fill_scales.R.

.default_max_pixels = function(){
  val = getOption("TPR_MAX_PIXELS", 800)
  .check_option(val, "TPR_MAX_PIXELS", .check_max_pixels)
}

.default_channel_names = function(){
  val = getOption("TPR_CHANNEL_NAMES", "auto")
  .check_option(val, "TPR_CHANNEL_NAMES", .check_channel_names)
}

.default_um_axes = function(){
  .check_option(getOption("TPR_UM_AXES", FALSE), "TPR_UM_AXES", .check_flag)
}

.default_scale_bar = function(){
  .check_option(getOption("TPR_UM_SCALE_BAR", FALSE), "TPR_UM_SCALE_BAR", .check_flag)
}

.default_scale_bar_style = function(){
  .check_option(getOption("TPR_SCALE_BAR_STYLE", list()), "TPR_SCALE_BAR_STYLE", .check_scale_bar_style)
}

.default_plot_theme = function(){
  .check_option(getOption("TPR_THEME"), "TPR_THEME", .check_plot_theme)
}

.default_verbose = function(){
  .check_option(getOption("TPR_VERBOSE", TRUE), "TPR_VERBOSE", .check_flag)
}

# Validate an option's value, naming the option in any error.
.check_option = function(val, name, check_FUN){
  tryCatch(check_FUN(val, name), error = function(e){
    stop("Invalid ", name, " option: ", conditionMessage(e), call. = FALSE)
  })
}

.check_flag = function(x, arg){
  if(!is.logical(x) || length(x) != 1 || is.na(x)) stop(arg, " must be TRUE or FALSE.", call. = FALSE)
  x
}

.check_max_pixels = function(x, arg){
  if(!is.numeric(x) || length(x) != 1 || !is.finite(x) || x <= 0) stop(arg, " must be a single positive number.", call. = FALSE)
  x
}

.check_channel_names = function(x, arg){
  if(!is.character(x) || length(x) < 1 || anyNA(x)) {
    stop(arg, " must be \"auto\", \"ome\", \"number\" or a character vector naming every channel.", call. = FALSE)
  }
  x
}

.check_scale_bar_style = function(x, arg){
  if(is.null(x)) x = list()
  if(!is.list(x) || (length(x) > 0 && (is.null(names(x)) || any(!nzchar(names(x)))))){
    stop(arg, " must be a named list of add_um_scale_bar() arguments.", call. = FALSE)
  }
  # the plot, its units and limits come from the image
  allowed = setdiff(names(formals(add_um_scale_bar)), c("p", "unit_per_pixel", "unit_name", "xlim", "ylim"))
  bad = setdiff(names(x), allowed)
  if(length(bad) > 0){
    stop(arg, " has unknown entries: ", paste(bad, collapse = ", "),
         ". Allowed: ", paste(allowed, collapse = ", "), call. = FALSE)
  }
  x
}

.check_plot_theme = function(x, arg){
  if(!is.null(x) && !inherits(x, "theme")) stop(arg, " must be a ggplot2 theme or NULL.", call. = FALSE)
  x
}
