library(TiffPlotR)
library(tidyverse)
tf = exampleTiff()

img_meta_df = read_tiff_meta_data(tf)
sel_size = img_meta_df %>% filter(sizeX == min(sizeX))

img_data = fetchTiffData(tf)
fetchTiffData.rgb(tf)
img_data@data

qmax = .999

max_res = img_meta_df$resolutionLevel %>% max

arr = fetchTiffArray(tf, resolution = max_res)
dim(arr)
?quantile
dim(arr)[1:2] %>% prod()
apply(arr, 3, quantile, probs = c(.5, .9, .95, .99, .999, .9999, .99999, 1))
#starting at lowest resolution
# for each channel
# retrieve signal near previous resolution high signal regions OR for entire image if lowest resolution
# identify n high signal regions
# calculate cutoff
# increment next lowest resolution
arr@metadata$coreMetadata
arr@metadata$globalMetadata
arr@metadata$seriesMetadata

hist(arr[,,1])
hist(arr[,,2])

x = arr[,,1]
i = 1
hist_res = lapply(seq(dim(arr)[3]), function(i){
    x = arr[,,i]

    rng = range(x)
    brks = seq(rng[1], rng[2], length.out = 50)

    zero_count = sum(x == 0)
    zero_pct = round(100*zero_count/length(x), 2)

    binned = cut(x[x>0], breaks = brks, include.lowest = TRUE)
    tab_res = table(binned)
    tib = tibble(count = tab_res, bin_range = names(tab_res))
    tib$bin_range = factor(tib$bin_range, levels = levels(binned))
    tib = tib %>%
        mutate(xmin = brks[as.numeric(bin_range)])  %>%
        mutate(xmax = brks[as.numeric(bin_range)+1])

    tib = rbind(
        tibble(count = zero_count, bin_range = "[0,0]", xmin = 0, xmax = 0),
        tib
    )
    tib$channel = i
    tib
})
hist_res = hist_res %>% bind_rows()

ggplot(hist_res %>% filter(xmax > 0), aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = count)) +
    geom_rect() +
    facet_wrap(~channel, scales = "free")


my_arr_hist = function(x){
    arr_dim = dim(x)
    stopifnot(length(arr_dim) == 3)
    layout(matrix())
    a

}

hist(arr[,,3], breaks = 500)

dim(arr)
#
# all_tiff = dir("/mnt/c/project_data/UVMMIC-RA-13113/alignment/", pattern = "tiff$", full.names = TRUE)
align_dir = "/mnt/c/project_data/UVMMIC-RA-13113/alignment/"
if(!dir.exists(align_dir)){
    align_dir = r"(D:\project_data\alignment)"
}
if(!dir.exists(align_dir)){
    align_dir = r"(C:\project_data\UVMMIC-RA-13113\alignment)"
}
if(!dir.exists(align_dir)){
    stop()
}

all_tiff = dir(align_dir, pattern = "tiff$", full.names = TRUE)
tiff_meta.l = lapply(all_tiff, read_tiff_meta_data)
lapply(tiff_meta.l, function(x){
    as.numeric(x$sizeX)*as.numeric(x$sizeY)/1e6
})
tf = all_tiff[1]

#### params ####
# query_probs = sort(c(seq(0, 100) / 100, 0.99 + seq(1, 9) / 1000, .9999))
query_probs = sort(c(seq(0, 100, by = .1) / 100, 0.99 + seq(1, 9) / 1000, .9999))

max_full_fetch = 100e6

# .99 is used when selecting low resolution regions
if(! .99 %in% query_probs){
    query_probs = sort(c(.99, query_probs))
}
query_probs = query_probs %>% unique %>% sort


#### step 0 prep ####
img_meta_df = read_tiff_meta_data(tf)
(as.numeric(img_meta_df$sizeX) * as.numeric(img_meta_df$sizeY)) / 1e6
meta_max = dplyr::filter(img_meta_df, sizeX == max(sizeX))


#select max (lowest pixel) res
max_res = max(img_meta_df$resolutionLevel)


res_i = max_res
rep_ranges = list()
all_quantiles = list()

.divide_space = function(xmax, ymax, target_pixels = 10e6, trim1 = TRUE){
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

res_i = 4
for(res_i in seq(max_res, 1)){

    meta_i = img_meta_df %>% dplyr::filter(resolutionLevel == res_i)
    res_ratio = meta_max$sizeX / meta_i$sizeX

    if(res_i > 1){
        res_next = res_i - 1
        meta_next = img_meta_df %>% dplyr::filter(resolutionLevel == res_next)

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

    # time_res = system.time({
    #     arr = fetchTiffArray(tf, resolution = 3, max_pixels = Inf)
    # })
    # time_res
    # # r3 = 134M pixels
    # # r2 = 5 pixels - failed
    # time_res = system.time({
    #     arr = fetchTiffArray(tf, resolution = 2, max_pixels = Inf)
    # })
    # time_res[3]
    # format(object.size(arr), units = "GB")
    # length(arr)/1e6




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
                sample_ranges = .divide_space(meta_i$sizeX, meta_i$sizeY, target_pixels = 50e6)
                #get .99 quantiles for all rectangles
                q99_per_range = lapply(seq(nrow(sample_ranges)), function(i){
                    xs = seq(sample_ranges[i, "xmin"], sample_ranges[i, "xmax"])
                    ys = seq(sample_ranges[i, "ymin"], sample_ranges[i, "ymax"])
                    apply(arr[xs, ys,], 3, .gather_quantiles_full_res, probs = .99)
                })
                q99_per_range.mat = q99_per_range %>% unlist %>% matrix(ncol = dim(arr)[3], byrow = TRUE)
                #select representive rectangles spaced at .25 quantiles apart
                .select_q = function(x, qvals = c(0,.25,.5, .75, 1)){
                    sel_q = quantile(order(x), probs = qvals, type = 1)

                    idx = seq_along(x)
                    o = order(x)

                    x = x[o]
                    idx = idx[o]
                    idx[sel_q]
                }

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
                    fetch_rect = TiffRect(
                        2*res_ratio*to_fetch$xmin[fetch_i], 2*res_ratio*to_fetch$xmax[fetch_i],
                        2*res_ratio*to_fetch$ymin[fetch_i], 2*res_ratio*to_fetch$ymax[fetch_i]
                    )


                    arr = fetchTiffArray(tf, resolution = res_i, max_pixels = Inf, rect = fetch_rect)
                    arr = arr@.Data[,,chan_i, drop = FALSE]



                    # locate representative rect here
                    sample_ranges = .divide_space(nrow(arr)+1, ncol(arr), target_pixels = 50e6, trim1 = FALSE)
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
                    arr_top_y = 2*to_fetch$ymin[fetch_i]
                    arr_left_x = 2*to_fetch$xmin[fetch_i]
                    sample_ranges.at_res = sample_ranges

                    sample_ranges.at_res$xmin = sample_ranges$xmin + arr_left_x
                    sample_ranges.at_res$xmax = with(sample_ranges, {xmin + 2*(xmax - xmin)}) + arr_left_x
                    sample_ranges.at_res$ymin = sample_ranges$ymin + arr_top_y
                    sample_ranges.at_res$ymax = with(sample_ranges, {ymin + 2*(ymax - ymin)}) + arr_top_y


                    sample_ranges_list = c(sample_ranges_list, list(sample_ranges.at_res))
                    q99_per_range_list = c(q99_per_range_list, list(q99_per_range))


                    fetch_contents = c(fetch_contents, list(as.numeric(arr)))
                    # fetch_contents = c(fetch_contents, as.numeric(arr))
                }

                # select best rect
                sample_ranges = sample_ranges_list %>% bind_rows
                q99_per_range = q99_per_range_list %>% unlist

                q99_per_range.mat = q99_per_range %>% unlist %>% matrix(ncol = dim(arr)[3], byrow = TRUE)

                #select representative rectangles spaced at .25 quantiles apart
                .select_q = function(x, qvals = c(0,.25,.5, .75, 1)){
                    sel_q = quantile(order(x), probs = qvals, type = 1)

                    idx = seq_along(x)
                    o = order(x)

                    x = x[o]
                    idx = idx[o]
                    idx[sel_q]
                }

                ranges_per_channel = apply(q99_per_range.mat, 2, .select_q, simplify = FALSE)

                #return rectangles per channel
                representative_ranges.i = lapply(ranges_per_channel, function(x){sample_ranges[x,]})
                representative_ranges.by_channel[[chan_i]][[fetch_i]] = representative_ranges.i

                # hist(fetch_contents[[1]] %>% log10)
                # hist(fetch_contents[[2]] %>% log10)
                # hist(fetch_contents[[3]] %>% log10)
                # hist(fetch_contents[[4]] %>% log10)
                # hist(fetch_contents[[5]] %>% log10)
                #
                # hist(fetch_contents %>% unlist %>% log10)

                quant_res.chan = .gather_quantiles_full_res(
                    fetch_contents %>% unlist,
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

test_ranges = rep_ranges[lengths(rep_ranges) > 1]
x = test_ranges[[1]]
lapply(test_ranges, function(x){
    with(x[[1]], (xmax -xmin) * (ymax - ymin))
})

names(all_quantiles) = as.character(seq_along(all_quantiles))
lapply(all_quantiles, nrow)
lapply(all_quantiles, rownames)
x = all_quantiles[[1]]
q_df.l = lapply(all_quantiles, function(x){
    df = as.data.frame(x)
    rownames(df) = NULL
    colnames(df) = paste0("channel_", seq(ncol(df)))
    df$quantile_str = rownames(x)
    df = df %>% mutate(quantile_value = sub("%", "", quantile_str) %>% as.numeric)
    df
})
q_df = bind_rows(q_df.l, .id = "resolution")
q_df$resolution = q_df$resolution %>% as.numeric %>% factor
rownames(q_df) = NULL
q_df %>% head
q_df = q_df %>% pivot_longer(cols = starts_with("channel"), names_to = "channel")
q_df = q_df %>% mutate(channel = as.numeric(sub("channel_", "", channel)))

p_q = ggplot(q_df %>% filter(quantile_value < 100), aes(x = quantile_value, y = value, color = resolution, group = paste(resolution, channel))) +
    geom_path() +
    facet_wrap(~channel, scales = "free_y")
p_q
p_q + coord_cartesian(xlim = c(25, 100), ylim = c(0, 100))


q_df.even = q_df %>% filter(100*quantile_value %% 1 == 0)

q_df.even$quantile_value %>% table
q_df.even %>% filter(quantile_value == 1)
q_df.even = q_df.even %>% mutate(quantile_bin = cut(quantile_value, c(-1, 75, 98, 101)))
ggplot(q_df.even , aes(x = log10(value), fill = quantile_bin)) +
    geom_histogram() +
    facet_grid(resolution~channel)

ggplot(q_df.even %>% filter(quantile_value > 80), aes(x = log10(value))) +
    geom_histogram(bins = 25) +
    facet_grid(resolution~channel)


