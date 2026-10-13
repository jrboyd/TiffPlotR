# library(testthat)
# library(tidyverse)
# tf = exampleTiff()
# 
# g_quants = TiffPlotR::gather_channel_quantiles(tf)
# 
# 
# g_quants.approx = TiffPlotR::gather_channel_quantiles(tf, max_full_fetch = 5e5)
# 
# g_quants$signal_quantiles
# g_quants.approx$signal_quantiles
# 
# 
# convert_q_matrix = function(x){
#   rn = rownames(x)
#   pvals = rn %>% sub("%", "", .) %>% as.numeric
#   x = as.data.frame(x)
#   rownames(x) = NULL
#   x$quantile_value = pvals/100
#   x = tidyr::pivot_longer(x, -quantile_value, names_to = "channel", values_to = "signal_value")
#   x
#   
# }
# 
# sig_q = g_quants$signal_quantiles
# names(sig_q) = seq_along(sig_q)
# gquant_df = lapply(sig_q, convert_q_matrix) %>% dplyr::bind_rows(.id = "resolution")
# gquant_df
# 
# sig_q.approx = g_quants.approx$signal_quantiles
# names(sig_q.approx) = seq_along(sig_q.approx)
# gquant_df.approx = lapply(sig_q.approx, convert_q_matrix) %>% dplyr::bind_rows(.id = "resolution")
# gquant_df.approx
# 
# 
# gquant_df.approx %>% group_by(resolution, channel) %>% summarize(min(signal_value), max(signal_value))
# 
# gquant_df$method = "exact"
# gquant_df.approx$method = "approximate"
# 
# gquant_df.plot = rbind(gquant_df, gquant_df.approx)
# 
# library(ggplot2)
# p = ggplot(gquant_df.plot %>% dplyr::filter(signal_value > 0), aes(x = quantile_value, y = signal_value+100, color = method, linetype = method)) +
#   geom_path() +
#   facet_grid(channel~resolution, scales = "free_y") +
#   scale_y_log10(labels = function(x)round(x-100)) +
#   labs(y = "signal value by channel", x = "quantile value by resolution")
# p
# p + coord_cartesian(xlim = c(.9998, 1))
# 
# p + coord_cartesian(xlim = c(.5, 1))
# 
# 
# 
# img5 = fetchTiffData.rgb(tf, resolution = 5)
# 
# 
# 
# .mult_TiffRect = function(tr, m){
#   tr@coords$xmin = m*tr@coords$xmin
#   tr@coords$xmax = m*tr@coords$xmax
#   tr@coords$ymin = m*tr@coords$ymin
#   tr@coords$ymax = m*tr@coords$ymax
#   tr
# }
# 
# i = 1
# .get_rects = function(i){
#   rng5 = g_quants.approx$assessed_ranges[[i]][[1]]
#   # rng5 = rng5[4, ]
#   rng5 = do.call(TiffRect, rng5)
#   rng5 = .mult_TiffRect(rng5, 2^(i-1))
#   rng5  
# }
# 
# g_quants.approx$assessed_ranges[[1]][[1]]
# g_quants.approx$assessed_ranges[[2]][[1]]
# g_quants.approx$assessed_ranges[[3]][[1]]
# 
# rng5 = .get_rects(5)
# rng4 = .get_rects(4)
# rng3 = .get_rects(3)
# rng2 = .get_rects(2)
# rng1 = .get_rects(1)
# 
# img5@plots$rgb %>% 
#   shape_annotate(rng5, color = "blue") %>%
#   shape_annotate(rng4, color = "cyan")%>%
#   shape_annotate(rng3, color = "lightblue")%>%
#   shape_annotate(rng2, color = "lightgreen")%>%
#   shape_annotate(rng1, color = "green")
# 
# img5@plots$rgb %>% shape_annotate(tr)
