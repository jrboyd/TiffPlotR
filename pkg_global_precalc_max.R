library(TiffPlotR)
library(tidyverse)

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

#### reem geomx ####

all_tiff = dir(align_dir, pattern = "tiff$", full.names = TRUE)
tiff_meta.l = lapply(all_tiff, read_tiff_meta_data)
lapply(tiff_meta.l, function(x){
    as.numeric(x$sizeX)*as.numeric(x$sizeY)/1e6
})

if(FALSE){
    names(all_tiff) = basename(all_tiff)
    all_gquant = lapply(all_tiff, gather_channel_quantiles, workers = 1)
    # saveRDS(all_gquant, "all_gquant.reem_geomx.Rds")
}

#### ashley ebv ####

ebv_tiffs = dir("C:/Users/boydj/project_data/EBV_image_files", full.names = TRUE) %>% dir(full.names = TRUE)
ebv_cache_dir = "~/../OneDrive - UVM Larner College of Medicine/projects_ashley/EBV_DLBCL/P1_phenocycler_and_rnascope/cached_global_precalc_max"
ebv_global_cache = file.path(ebv_cache_dir, "global_quantiles")
dir.create(ebv_global_cache, recursive = TRUE)

length(ebv_tiffs)
dir(ebv_global_cache, full.names = TRUE) %>% dir %>% length

for(tf in ebv_tiffs){
    assay_name = dirname(tf) %>% basename()
    assay_cache_dir = file.path(ebv_global_cache, assay_name)
    dir.create(assay_cache_dir, showWarnings = FALSE)


    cache_file = file.path(assay_cache_dir, paste0(basename(tf), ".gquant_cache.Rds"))
    message(tf)
    if(!file.exists(cache_file)){
        message("running gather_channel_quantiles")
        # debug(gather_channel_quantiles)
        if(nrow(TiffPlotR::read_tiff_meta_data(tf)) == 1){
            gquant = list()
        }else{
            gquant = gather_channel_quantiles(tf, workers = 1)
        }

        saveRDS(gquant, cache_file)
    }else{
        message("skip cached gquant")
    }
}

sig_q = all_gquant$TomatoRed_PanCk_CD45_3124.ome_.tiff$signal_quantiles
sig_q[[5]] %>% range
names(sig_q) = seq_along(sig_q)
lapply(sig_q, dim)
lapply(sig_q, colnames)
sig_df = bind_rows(lapply(sig_q, function(x){out = as.data.frame(x); rownames(out) = NULL; colnames(out) = seq(ncol(out)); out$q = rownames(x); out}), .id = "resolution")
sig_df = sig_df %>% mutate(qnum = sub("%", "", q) %>% as.numeric)
sig_df %>% head

sig_df = sig_df %>% pivot_longer(cols = -c(resolution, q, qnum), names_to = "channel", values_to = "signal")

sig_df %>% filter(resolution > 3)
sig_df %>% filter(qnum > 99.9) %>% pull(q) %>% unique()

p_q = ggplot(sig_df, aes(x = qnum, y = signal, color = channel, group = channel)) +
    geom_path() +
    facet_grid(resolution~., scales = "free_y")
p_q + coord_cartesian(xlim = c(99, 100))

g_quant$signal_quantiles %>% length
time_res


rep_ranges = g_quant$assessed_ranges
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
