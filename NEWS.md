# TiffPlotR 0.1.22

## New

* `precalcTiffDataScales()` returns the `precalc_max` table a fetch of a view
  would compute, without building the data or plot. Pass it to other fetches
  to give them the same contrast.
* `scale_channels_gather_quantiles(resolutions = )` gathers a subset of
  resolution levels.
* Fetches take `min_q` and `max_q`, the quantiles used for each channel's
  limits when there is no `precalc_max`. The defaults come from the new
  `TPR_MIN_Q` and `TPR_MAX_Q` options, otherwise `NULL` and 0.999. Both values
  are recorded in `@plot_parameters`.

## Changed

* Fetches without `precalc_max` pick their limits by the same rule as
  `scale_channels_set_thresholds()`: zero quantiles are skipped. With the
  defaults, limits are unchanged for channels whose 0.999 quantile is
  non-zero. A channel whose 0.999 quantile is 0 used to get NA values and
  draw as a grey panel; it now gets finite limits.
* `min_q = NULL` (the new fetch default) means a floor of 0. `min_q = 0` is
  now an ordinary quantile, in `scale_channels_set_thresholds()` too: when a
  channel's minimum is 0, it takes the smallest non-zero quantile instead.
  Use `min_q = NULL` for the old floor of 0.
* `min_q` and `max_q` take the place of `quantile_norm` in the argument
  list, and `quantile_norm` moves to the end. Calls that pass arguments by
  position after `show_raw` need updating.

## Deprecated

These still work, with a warning:

* `gather_channel_quantiles()`: use `scale_channels_gather_quantiles()`.
* `precalc_from_quantiles()`: use `scale_channels_set_thresholds()`.
* `fetchTiffData.rgb()` and `fetchTiffData.colorized()`: use
  `fetchTiffDataRGB()` and `fetchTiffDataColorized()`.
* `plotTiffData.rgb()` and `plotTiffData.colorized()`: use
  `plotTiffDataRGB()` and `plotTiffDataColorized()`.
* The `quantile_norm` argument: use `max_q`.
