
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

g_quant = gather_channel_quantiles(all_tiff[1])

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
