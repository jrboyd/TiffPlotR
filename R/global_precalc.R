
.get_res_ratio = function(res){
    2^(res-1)
}
.x_next_resolution = function(x){
    2*x
}
.x_full_resolultion = function(x, res){
    x*.get_res_ratio(res)
}


#' as.TiffRect
#'
#' @param df
#'
#' @returns
#'
#' @examples
.as.TiffRect = function(df){
    stopifnot(is.data.frame(df))
    stopifnot(c("xmin", "xmax", "ymin", "ymax") %in% colnames(df))
    if(is.null(df$name)){
        df$name = rownames(df)
    }
    tr = TiffRect(xmin = df$xmin, xmax = df$xmax, ymin = df$ymin, ymax = df$ymax)
    if(!is.null(df$name)){
        tr@coords$name = df$name
    }
    tr
}

#convert previous resolution array coord to current resolution array coord
#convert resolution array coord to total image coord

.mult_TiffRect = function(tr, m){
    tr@coords$xmin = m*tr@coords$xmin
    tr@coords$xmax = m*tr@coords$xmax
    tr@coords$ymin = m*tr@coords$ymin
    tr@coords$ymax = m*tr@coords$ymax
    tr
}



gather_channel_quantiles = function(tf, max_full_fetch = 100e6, query_probs = .query_probs()){
    #### step 0 prep ####
    img_meta_df = read_tiff_meta_data(tf)
    meta_max = dplyr::filter(img_meta_df, sizeX == max(sizeX))
    #select max (lowest pixel) res
    max_res = max(img_meta_df$resolutionLevel)

    rep_ranges = list()
    all_quantiles = list()

    for(res_i in seq(max_res, 1)){

        meta_i = dplyr::filter(img_meta_df, resolutionLevel == res_i)
        # res_ratio can be defined as 2^(res_i-1)
        # res_ratio = meta_max$sizeX / meta_i$sizeX
        res_ratio = 2^(res_i-1)

        if(res_i > 1){
            res_next = res_i - 1
            meta_next = dplyr::filter(img_meta_df, resolutionLevel == res_next)

            if(is.null(meta_next)){
                next_pixels = Inf
            }else{
                # next fetch may be low resolution, so we need rectangles
                next_pixels = as.numeric(meta_next$sizeX)*as.numeric(meta_next$sizeY)
            }

            preserve_rects = next_pixels > max_full_fetch
        }else{
            res_next = NULL
            meta_next = NULL
            preserve_rects = FALSE
        }

        res_pixels = as.numeric(meta_i$sizeX)*as.numeric(meta_i$sizeY)
        fetch_mode = "FULL"
        if(res_pixels > max_full_fetch){
            fetch_mode = "LOW"
        }
        message("using fetch mode ", fetch_mode)
        #### step 1 gather quantiles ####

        .gather_quantiles_full_res = function(mat, probs){
            quantile(mat, probs = probs)
        }

        switch (
            fetch_mode,
            FULL = {
                arr = fetchTiffArray(tf, resolution = res_i, max_pixels = Inf)
                arr = arr@.Data
                quant_res = apply(arr, 3, .gather_quantiles_full_res, probs = query_probs)
                if(preserve_rects){
                    message("locating representative areas")
                    #divide space into overlapping rectangles
                    sample_ranges = .divide_space(meta_i$sizeX, meta_i$sizeY)
                    #get .99 quantiles for all rectangles
                    q99_per_range = lapply(seq(nrow(sample_ranges)), function(i){
                        xs = seq(sample_ranges[i, "xmin"], sample_ranges[i, "xmax"])
                        ys = seq(sample_ranges[i, "ymin"], sample_ranges[i, "ymax"])
                        apply(arr[xs, ys,], 3, .gather_quantiles_full_res, probs = .99)
                    })
                    q99_per_range.mat = matrix(unlist(q99_per_range), ncol = dim(arr)[3], byrow = TRUE)
                    #select representive rectangles spaced at .25 quantiles apart
                    ranges_per_channel = apply(q99_per_range.mat, 2, .select_q, simplify = FALSE)
                    #check this way
                    # q99_per_range.mat[ranges_per_channel[[1]],]
                    # q99_per_range.mat[ranges_per_channel[[2]],]
                    # q99_per_range.mat[ranges_per_channel[[3]],]
                    # q99_per_range.mat[ranges_per_channel[[4]],]

                    #return rectangles per channel
                    representative_ranges.by_channel = lapply(ranges_per_channel, function(x){sample_ranges[x,]})
                }else{
                    representative_ranges.by_channel = NA
                }
            },
            LOW = {
                if(!is.list(rep_ranges[[res_i+1]])){
                    stop("first resolution exceeds allowed size")
                }
                prev_ranges = rep_ranges[[res_i+1]]
                quant_res = list()
                representative_ranges.by_channel = list()


                for(chan_i in seq_along(prev_ranges)){
                    representative_ranges.by_channel[[chan_i]] = list()
                    to_fetch = prev_ranges[[chan_i]]
                    fetch_contents = numeric()
                    fetch_contents = list()

                    sample_ranges_list = list()
                    q99_per_range_list = list()

                    for(fetch_i in seq(nrow(to_fetch))){
                        #multiple by 2 for current resolution pixels
                        prev_res_fetch_rect = .as.TiffRect(to_fetch[fetch_i,])
                        curr_res_fetch_rect = .mult_TiffRect(prev_res_fetch_rect, 2)
                        full_res_fetch_rect = .mult_TiffRect(curr_res_fetch_rect, res_ratio)
                        if(full_res_fetch_rect$ymin < 0){
                            browser()
                        }
                        tryCatch({
                            arr = fetchTiffArray(tf, resolution = res_i, max_pixels = Inf, rect = full_res_fetch_rect)
                        }, error = function(e){
                            browser()
                        })

                        arr = arr@.Data[,,chan_i, drop = FALSE]

                        # locate representative rect here
                        sample_ranges = .divide_space(nrow(arr)+1, ncol(arr), trim1 = FALSE)
                        sample_ranges$ymax[sample_ranges$ymax > ncol(arr)] = ncol(arr)
                        sample_ranges$xmax[sample_ranges$xmax > nrow(arr)] = nrow(arr)
                        q99_per_range = lapply(seq(nrow(sample_ranges)), function(i){
                            xs = seq(sample_ranges[i, "xmin"], sample_ranges[i, "xmax"])
                            ys = seq(sample_ranges[i, "ymin"], sample_ranges[i, "ymax"])
                            if(length(dim(arr)) == 3){
                                apply(arr[xs, ys,,drop = FALSE], 3, .gather_quantiles_full_res, probs = .99)
                            }else{
                                stop()
                                .gather_quantiles_full_res(arr[xs, ys], probs = .99)
                            }
                        })

                        #array index ranges need to convert to local resolution pixels, not max pixels
                        sample_ranges.at_res = .as.TiffRect(sample_ranges)
                        sample_ranges.at_res = shape_shift(sample_ranges.at_res, curr_res_fetch_rect$xmin, curr_res_fetch_rect$ymin)
                        sample_ranges.at_res = shape_resize_mult(sample_ranges.at_res, 2, anchor = "topleft")

                        sample_ranges_list = c(sample_ranges_list, list(sample_ranges.at_res@coords))
                        q99_per_range_list = c(q99_per_range_list, list(q99_per_range))


                        fetch_contents = c(fetch_contents, list(as.numeric(arr)))
                        # fetch_contents = c(fetch_contents, as.numeric(arr))
                    }

                    # select best rect
                    sample_ranges = bind_rows(sample_ranges_list)
                    q99_per_range = unlist(q99_per_range_list)

                    q99_per_range.mat = matrix(unlist(q99_per_range), ncol = dim(arr)[3], byrow = TRUE)

                    #select representative rectangles spaced at .25 quantiles apart
                    ranges_per_channel = apply(q99_per_range.mat, 2, .select_q, simplify = FALSE)

                    #return rectangles per channel
                    representative_ranges.i = lapply(ranges_per_channel, function(x){sample_ranges[x,]})
                    representative_ranges.by_channel[[chan_i]][[fetch_i]] = representative_ranges.i

                    quant_res.chan = .gather_quantiles_full_res(
                        unlist(fetch_contents),
                        probs = query_probs
                    )
                    quant_res[[chan_i]] = quant_res.chan

                    #return rectangles per channel
                    # representative_ranges.by_channel = lapply(ranges_per_channel, function(x){sample_ranges[x,]})
                }
                quant_res = do.call(cbind, quant_res)
                representative_ranges.by_channel = lapply(representative_ranges.by_channel, bind_rows)
            })
        rep_ranges[[res_i]] = representative_ranges.by_channel
        all_quantiles[[res_i]] = quant_res
    }

    list(signal_quantiles = all_quantiles, assessed_ranges = rep_ranges)
}

.divide_space = function(xmax, ymax, target_pixels = 50e6, trim1 = TRUE){
    last_diff = Inf

    best_split = 1
    n_split = 1
    while(TRUE){
        curr_diff = abs(target_pixels - xmax/n_split*ymax/n_split)
        if(curr_diff < last_diff){
            last_diff = curr_diff
            best_split = n_split
        }else{
            break
        }
        n_split = 2*n_split
    }
    .prep_breaks = function(max_val, n_split){
        step = max_val / n_split
        breaks_full = seq(0, max_val, by = step)
        breaks_half = breaks_full + step / 2
        breaks_half = breaks_half[breaks_half < max_val]

        breaks = sort(c(
            breaks_full, breaks_half
        ))
        breaks
    }

    x_breaks = .prep_breaks(xmax, n_split)
    y_breaks = .prep_breaks(ymax, n_split)

    x_starts = x_breaks[-length(x_breaks)]+1
    x_ends = x_breaks[-1]

    y_starts = y_breaks[-length(y_breaks)]+1
    y_ends = y_breaks[-1]

    full_grid = expand.grid(
        seq_along(x_starts),
        seq_along(y_starts)
    )
    ranges_df = data.frame(
        xmin = x_starts[full_grid$Var1], xmax = x_ends[full_grid$Var1],
        ymin = y_starts[full_grid$Var2], ymax = y_ends[full_grid$Var2]
    )
    #the actual array has 1 less pixel than expected
    if(trim1){
        ranges_df$xmax[ranges_df$xmax > xmax - 1] = xmax - 1
        ranges_df$ymax[ranges_df$ymax > ymax - 1] = ymax - 1
    }

    ranges_df
}
.select_q = function(x, qvals = c(0,.25,.5, .75, 1)){
    sel_q = quantile(order(x), probs = qvals, type = 1)

    idx = seq_along(x)
    o = order(x)

    x = x[o]
    idx = idx[o]
    idx[sel_q]
}

.query_probs = function(){
    sort(c(seq(0, 100, by = .1) / 100, 0.99 + seq(1, 9) / 1000, .9999))
}
