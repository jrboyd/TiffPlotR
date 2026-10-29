#' Read per-channel metadata from a TIFF's OME-XML
#'
#' Returns one row per channel of the first image (series 1, the one
#' \code{fetchTiffData()} reads), with every attribute OME records for it, such
#' as \code{Name}, \code{Fluor} or \code{EmissionWavelength}. Attributes of
#' child elements are included as \code{<Child>_<attribute>}, for example
#' \code{LightSourceSettings_Wavelength}.
#'
#' If the file has no readable OME-XML, or it does not describe the channels,
#' the result still has one row per channel, with \code{name} and \code{color}
#' set to NA.
#'
#' @param tiff_path Path to the TIFF image file.
#'
#' @returns A data.frame with columns:
#'   \item{channel}{Channel number, 1-based.}
#'   \item{name}{OME channel \code{Name}, or NA.}
#'   \item{color}{OME channel \code{Color} as a \code{"#RRGGBB"} string, or NA.}
#'   followed by the remaining OME attributes, converted to numbers where they
#'   look numeric.
#' @export
#'
#' @examples
#' read_tiff_channels(exampleTiff())
read_tiff_channels = function(tiff_path){
  n_channels = read_tiff_meta_data(tiff_path)$sizeC[1]
  ome = tryCatch(.read_ome_channels(.read_tiff_ome_xml(tiff_path)), error = function(e) data.frame())
  if(nrow(ome) != n_channels){
    if(nrow(ome) > 0){
      warning("OME-XML describes ", nrow(ome), " channels but the image has ", n_channels,
              "; ignoring OME channel metadata.")
    }
    ome = data.frame(row.names = seq_len(n_channels))
  }
  name = if(is.null(ome$Name)) NA_character_ else as.character(ome$Name)
  color = if(is.null(ome$Color)) NA_character_ else .ome_color_to_hex(ome$Color)
  extra = ome[, setdiff(colnames(ome), c("Name", "Color")), drop = FALSE]
  extra[] = lapply(extra, utils::type.convert, as.is = TRUE)
  out = data.frame(channel = seq_len(n_channels), name = name, color = color, stringsAsFactors = FALSE)
  cbind(out, extra)
}

# Attributes of the first Image's Channel nodes (and of their child elements),
# one row per channel, as character.
.read_ome_channels = function(ome_doc){
  image = xml2::xml_find_first(ome_doc, ".//*[local-name()='Image']")
  if(inherits(image, "xml_missing")) return(data.frame())
  nodes = xml2::xml_find_all(image, ".//*[local-name()='Channel']")
  rows = lapply(nodes, function(node){
    attrs = as.list(xml2::xml_attrs(node))
    for(child in xml2::xml_children(node)){
      child_attrs = xml2::xml_attrs(child)
      if(length(child_attrs) > 0){
        names(child_attrs) = paste0(xml2::xml_name(child), "_", names(child_attrs))
        attrs = c(attrs, as.list(child_attrs))
      }
    }
    attrs
  })
  if(length(rows) == 0) return(data.frame())
  cols = unique(unlist(lapply(rows, names)))
  out = lapply(cols, function(cl) vapply(rows, function(r) if(is.null(r[[cl]])) NA_character_ else r[[cl]], character(1)))
  names(out) = cols
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

# OME stores Color as a signed 32-bit RGBA integer (red in the high byte; alpha
# is dropped). Not to_rgba_hex(), which reads the bytes as ARGB.
.ome_color_to_hex = function(x){
  val = suppressWarnings(as.numeric(x))
  val = ifelse(val < 0, val + 2^32, val)
  out = rep(NA_character_, length(val))
  ok = !is.na(val)
  v = val[ok]
  out[ok] = grDevices::rgb(floor(v / 256^3) %% 256, floor(v / 256^2) %% 256, floor(v / 256) %% 256,
                           maxColorValue = 255)
  out
}

# The channel dictionary stored in TiffPlotData@channels: every channel in the
# image, with the name used for display and the OME name and color.
#
# channel_names: NULL (names are the channel numbers), "ome" (the OME names),
# or a character vector naming every channel in order.
.channel_table = function(tiff_path, channel_names = NULL){
  ome = read_tiff_channels(tiff_path)
  n = nrow(ome)
  if(is.null(channel_names)){
    display = as.character(ome$channel)
  }else if(identical(channel_names, "ome")){
    if(anyNA(ome$name)) stop("channel_names = \"ome\", but this TIFF's OME-XML does not name every channel.")
    display = ome$name
  }else{
    if(length(channel_names) < n){
      stop("Not enough channel names specified, expected ", n, " channels.")
    }
    if(length(channel_names) > n){
      warning("Too many channel names specified, only found ", n, " channels.")
    }
    display = as.character(channel_names[seq_len(n)])
  }
  if(anyNA(display) || any(!nzchar(display))) stop("channel_names must not contain missing or empty names.")
  if(anyDuplicated(display)){
    stop("channel_names must be unique. Duplicates: ", paste(unique(display[duplicated(display)]), collapse = ", "))
  }
  data.frame(channel = ome$channel, channel_name = display, ome_name = ome$name, color = ome$color,
             stringsAsFactors = FALSE)
}

# The channel table for an object built without a TIFF: channels named by
# their numbers, taken from data$channel. Empty when data has no numeric
# channel column.
.default_channel_table = function(data){
  ch = data$channel
  if(!is.numeric(ch) || length(ch) == 0) return(data.frame())
  ch = sort(unique(ch[!is.na(ch)]))
  data.frame(channel = ch, channel_name = as.character(ch), ome_name = NA_character_, color = NA_character_,
             stringsAsFactors = FALSE)
}

# Resolve channels given by number or name to channel numbers. Names match the
# display channel_name first, then the OME name (when that is unambiguous).
.resolve_channels = function(x, channel_table, arg = "channels", allow_duplicates = FALSE){
  if(is.null(x)) return(NULL)
  if(is.numeric(x)){
    k = match(x, channel_table$channel)
  }else if(is.character(x) || is.factor(x)){
    x = as.character(x)
    k = match(x, channel_table$channel_name)
    ome_name = channel_table$ome_name
    ome_name[duplicated(ome_name) | duplicated(ome_name, fromLast = TRUE)] = NA
    k[is.na(k)] = match(x[is.na(k)], ome_name, incomparables = NA)
  }else{
    stop(arg, " must be channel numbers or names.")
  }
  if(anyNA(k)){
    stop("Unknown ", arg, ": ", paste(unique(x[is.na(k)]), collapse = ", "),
         ". Available channels: ", paste(channel_table$channel_name, collapse = ", "))
  }
  if(!allow_duplicates && anyDuplicated(k)){
    stop("Each channel can only be given once in ", arg, ".")
  }
  channel_table$channel[k]
}
