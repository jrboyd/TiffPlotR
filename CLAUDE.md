# TiffPlotR

R package: efficient ggplot2 plots from (OME-)TIFF images, plus S4 shape classes
(`TiffRect`, `TiffEllipse`, `TiffPolygon`, all inheriting `TiffShape`) for annotating them.

## Running tests

```bash
export R_LIBS_USER=/gpfs1/home/j/r/jrboyd/R/x86_64-pc-linux-gnu-library/4.5
/gpfs1/sw/rh9/pkgs/stacks/gcc/13.3.0/R/4.5.1/bin/Rscript --vanilla -e \
  'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_tiff_shape_classes.R")'
```

Use `devtools::test()` for the whole suite. Shape-class tests are small and fine to run inline.

## Traps

- **Mixed line endings.** About half of `R/*.R` (e.g. `R/class_TiffPolygon.R`) use CRLF,
  and the others use LF. Editing with a tool that normalises line endings (Python text mode)
  rewrites every line, which turns the diff into noise. Keep each file's existing endings.
- **`TiffPolygon()` has two input forms.** You can pass a list of vertex vectors, one per
  polygon, or a long form: atomic `x`/`y` with a `name` the same length, which is `split()`
  by name, so polygons come back sorted by name. The long form applies only when `x`/`y`
  are atomic. A list input is never re-split.
- **`.unique_shape_names()`** suffixes `_1`, `_2`, ... only onto *duplicated* names.
  Names that are already unique (`"p1"`, `"p2"`) are left as they are.
- **The channel legend lives on the `colour` scale** of the rgb/colorized plots: an invisible
  `geom_point(x = NA, y = NA)` layer plus `scale_color_manual`. A caller's own
  `scale_colour_*` silently replaces it.
  - Plain `ggnewscale::new_scale_colour()` is also wrong. In ggplot2 >= 3.5, `p$guides` is
    an environment shared by every copy of a plot, so the guide gets renamed on all siblings
    and they draw grey, keyless legends.
  - `add_points()` handles this by rebuilding `p$guides` first.
  - Don't "fix" it with `ggproto(NULL, p$guides)`: that overflows the S7 stack.
- **`set_axes_um()` / `set_axes_pixel()` only relabel.** Data stay in pixel coordinates (breaks
  are chosen at round µm values and mapped back), so shapes, scale bars and coord limits keep
  working. They replace both position scales, keeping a `reverse` transform. Any other custom
  x/y scale settings (expand, limits) on the plot are dropped.
- **Label masks** (`label_polygons()`):
  - RBioFormats `subset` indices are 1-based, while `ocontour()` returns 0-based coordinates.
    The crop offset is therefore `min(xs) - 1`.
  - Label ids must be relabelled to 1..n before `ocontour()`, which indexes by value
    (ids can be about 1e8).
  - `as.character(1e8)` is `"1e+08"`, so names use `format(scientific = FALSE)`.
  - A 1-2 px label gives fewer than 3 vertices and is dropped.
- **`gather_channel_quantiles(max_full_fetch = "auto")`** resolves to
  `min(100e6, mem / (n_channels * 8 B * 10))`.
  - mem is the SLURM allocation (`SLURM_MEM_PER_NODE` / `SLURM_MEM_PER_CPU`) when set,
    otherwise MemAvailable. `.detect_cores()` caps workers by the same memory.
  - The 10x overhead factor is an estimate that hasn't been validated on real data. The
    old fixed 100e6 ran out of memory on AVITI (14 channels, 32 GB); auto gives about 30e6
    there. Check `MaxRSS` on the first real run.

- **dplyr is imported, never attached.** NAMESPACE imports only `group_by`, `mutate`,
  `select` and `summarise` from it. Any other verb must be written `dplyr::`, or it fails
  with "could not find function" for every caller who hasn't attached dplyr themselves.
  - The test suite doesn't attach dplyr, so it catches this, but only on code paths it
    actually runs (see `test_precalc_max_fetch.R`).
  - To sweep for these, run
    `codetools::checkUsageEnv(asNamespace("TiffPlotR"))` and look for "no visible global
    function".
  - That check misses bare symbols in a pipe (`x %>% ungroup %>% ...`), so also grep for those.
  - `sym`, `.data` and `expr` resolve through `import(ggplot2)`, which re-exports them from rlang.
- **The precalc pipeline is `gather_channel_quantiles()` → `precalc_from_quantiles()`.**
  - The older `collect_channel_quantiles_all_resolutions` / `quantiles_to_precalc_max` /
    `precalc_max_all_resolutions` pipeline was removed after 0.1.18. Its sampled path called a
    function that had already been deleted. `dev_global_precalc_max_claude.R` still
    references it.
  - `precalc_from_quantiles()` drops zero quantiles *before* picking limits.
    - `max_value` is the non-zero quantile nearest `max_q`.
    - `min_value` is `min_q` if that quantile is non-zero, else the smallest non-zero one.
  - This matches the AVITI and CosMx reductions. Their defaults (0.30 / 0.99) set the
    contrast of existing figures. On CosMx, the old 0.90 / 0.998 clipped nuclear signal to
    black.
  - A full `gather_channel_quantiles(exampleTiff(), max_full_fetch = 1e6, workers = 1)`
    takes about 35 s, so the tests use synthetic quantile matrices instead.
- **Channels: one dictionary, one resolver** (`R/channels.R`).
  - `@channels` lists *every* channel in the image, not only the selected ones. Its columns are
    `channel`, `channel_name` (the display label), `ome_name` and `color`. It is built by
    `.channel_table()`.
  - `@data$channel` is always the integer channel number. `@data$channel_name` is a factor in
    plotting order, and plots facet on it.
  - Code that filters by name must use `channel_name`. In 0.1.19 and earlier, `channel` held the names
    whenever `channel_names` was given.
  - Every channel argument goes through `.resolve_channels()`. That covers `selected_channels`,
    `red/green/blue_channel`, `channel_colors` names, a `precalc_max` keyed by name, and
    `renormalize(channels =)`.
  - `.resolve_channels()` matches `channel_name` first, then the OME name if no other channel
    shares it. So `"DAPI"` resolves even when `channel_names` was NULL.
  - `precalc_max` is always stored keyed by channel number.
  - **The default naming changed in 0.1.21.** It is now `"auto"`: each channel's OME name
    where it is unique and non-empty, else `"channel N"`. If that leaves a clash, every channel
    becomes `"channel N"` (see `.auto_channel_names()`).
    - `"number"` gives the old `"1"`, `"2"` labels. Set `options(TPR_CHANNEL_NAMES = "number")`
      to reproduce figures made with 0.1.20 or earlier.
    - The example TIFF names every channel, so its default labels are `DAPI`, `Opal 520`, ...
  - `update_TiffPlotData()` pins `"number"` for old objects, because their plots were labelled
    by number. `.default_channel_table()` (objects built without a TIFF, e.g. in tests) also still
    uses bare numbers.
- **`@package_version` records the TiffPlotR version that created an object.** It is set by the
  `initialize` method, so every creation path stamps it. `readRDS()` doesn't call `initialize`,
  so a saved object keeps the version it was made with.
  - Objects from 0.1.19 and earlier have neither `@package_version` nor `@channels`.
    `update_TiffPlotData()` (`R/update_TiffPlotData.R`) converts them and restamps the version.
  - Objects from 0.1.20 have no `@plot_parameters`. The update fills it with the default
    `scale_colors` and keeps their `@package_version`. `.needs_update()` is true whenever any
    class slot is missing, so adding a slot only needs a fill-in step in `update_TiffPlotData()`.
  - **Every function that takes a TiffPlotData must start with
    `.check_TiffPlotData_version()`**, which updates old objects with a warning. Accessing a slot
    the old object lacks errors, and `validObject()` fails, so the check has to come first.
  - 0.1.19 discarded channel numbers once `channel_names` was applied. The update maps names back
    through the TIFF's OME names; otherwise the caller must pass the fetch-time `channel_names`.
    Number-like labels (`"5"`, `"1"`) are treated as channel numbers.
  - A format change that keeps the same slots needs a step in `.needs_update()` /
    `update_TiffPlotData()` gated on `@package_version`, rather than guessing from the data's shape.
    That means bumping the version in the same change.
  - Real 0.1.19 objects can be made for testing with `git worktree add <scratch> bb5b802` and
    `devtools::load_all(<scratch>)`. The example TIFF is untracked, so pass its `inst/extdata` path.
    The 0.1.19 rgb fetch errors on an all-zero channel (channels 2–4 in the test region).
- **All image plots are built in `R/plot_tiff_data.R`.**
  - The builders are `.plot_channels()`, `.plot_masked()`, `.plot_rgb()` and `.plot_colorized()`.
    The last two share `.plot_composite()`, which sets `p$meta$tiffplotr_composite` for
    `renormalize()`.
  - Both the `.fetch_tiff_data*()` / `fetchTiffDataMasked()` path and the public
    `plotTiffData*()` wrappers call them. Change plot appearance there, not in the fetchers.
  - Moving the fetchers onto the builders left fetched plots identical (built layer data, scales,
    coords, theme and meta). Only the label order of the masked plot changed.
  - `plotTiffData*()` can only plot channels that are in `@data`. `selected_channels` at fetch
    time drops the others for good.
  - Four plot options are applied after the builders by `.apply_plot_options()`: `um_axes`,
    `scale_bar`, `scale_bar_style` and `plot_theme`.
    - On fetch, `.finish_plot_options()` runs at the end of each public `fetchTiffData*()`. The
      internal `.fetch_tiff_data*()` never apply them.
    - On replot, `.plot_return()` applies them.
    - For replots the precedence is explicit argument > `@plot_parameters` > option. A recorded
      `plot_theme = NULL` counts as recorded, so `.plot_option()` tests `%in% names()`, not
      `is.null()`.
    - Images without `@unit_per_pixel` warn and stay in pixels, so the options are safe to set
      globally.
- **Options all live in `R/options.R`** (except `TPR_SCALE_COLORS`, in `R/fill_scales.R`) and
  are documented in `?TiffPlotR`. The pattern for adding one:
  - Add a `.default_*()` getter that validates through `.check_option()`, so errors name the
    option.
  - Use the getter as the argument default.
  - Record the value in `@plot_parameters`.
  - Document it in `R/TiffPlotR-package.R`.
  - `verbose` is the one option deliberately not recorded. It works by wrapping the internal
    fetch in `.quiet_unless()`, which is `suppressMessages()`, so it silences every message from
    the fetch, not only TiffPlotR's.
  - `@plot_parameters` from a fetch holds `scale_colors`, `channel_names`, `max_pixels`,
    `um_axes`, `scale_bar`, `scale_bar_style` and `plot_theme`.
- **`man/hello.Rd` is stale.** It documents a `hello()` that no longer exists. The examples on
  the internal topics (`man/dot-*.Rd`) call private functions. Both make
  `devtools::run_examples()` stop early, so run the exported topics' examples individually.
- **Image fills come from two helpers in `R/fill_scales.R`.**
  - Single-channel plots use `.fill_gradient_layers(scale_colors)`: `geom_raster()` plus
    `scale_fill_gradientn()`.
  - rgb/colorized composites use `.fill_identity_layers(rgb_df)`: a `chex` raster plus
    `scale_fill_identity()`. `renormalize()` swaps in `.identity_raster()`.
  - The default `scale_colors` comes from `.default_scale_colors()`. That is the
    `TPR_SCALE_COLORS` option when it is set (documented in `?TiffPlotR`, `R/TiffPlotR-package.R`),
    otherwise `.magma_scale_colors()`.
  - Magma is a hardcoded `viridisLite::magma(6)`, which is exactly what
    `scale_fill_viridis_c(option = "magma")` interpolated, so old figures are unchanged. A test
    checks this. viridisLite isn't a declared dependency, which is why the colours are hardcoded.
  - Use `.magma_scale_colors()` (not the default) when you need what old plots were actually
    drawn with, as `update_TiffPlotData()` does. The option may have changed since.
  - `scale_colors` is recorded in `@plot_parameters`, a named list meant for any plotting
    option that later rebuilds need to recall. rgb/colorized objects also carry it, from the
    underlying `.fetch_tiff_data()` call.
- **`fetchTiffDataMasked()` checks masks before it reads the image.**
  - It stops if there are no mask points, they lack `i`/`j`, or none fall in `rect`. With
    `retain_unmasked = TRUE`, no points in `rect` is only a warning.
  - The example TIFF has no mask annotations. `test_fetch_masked.R` passes synthetic
    `mask_points` and mocks `.fetch_tiff_data()`.
  - **Below full resolution, pixel coordinates aren't integers.** The example TIFF is 8651 px wide,
    but 1082 at level 4, so the spacing is 7.9953789… Never join or compare `i`/`j` across
    sources with `==`/`merge()`. `fetchTiffDataMasked()` joins masks to the signal on the integer
    `i_bin`/`j_bin` instead. Before that fix, rebuilding `i`/`j` from the bins missed by a few ulps.
    Levels 3 and 4 then matched nothing, and level 2 silently lost about 30% of masked pixels.
  - A pixel covered by two ROIs gets one row per ROI, because overlap is decided per `ROI_ID`.
- **OME `Color` is RGBA (red in the high byte), but `to_rgba_hex()` decodes it as ARGB.**
  - `read_tiff_channels()` uses its own correct decoder, `.ome_color_to_hex()`.
  - The mask `FillColor` path still uses `to_rgba_hex()` via `.ome_fill_color_to_hex()`, so mask
    colours are probably wrong. That helper also prepends a second `#`. It hasn't been fixed
    because changing it changes existing mask colours.
- **`read_tiff_channels()` reads only the first OME `Image`** (series 1). `.fetch_ome_nodes()`
  searches the whole document, so it would mix in Channel nodes from label or macro images.
- **`renormalize()` rebuilds rgb/colorized plots from `p$meta$tiffplotr_composite`.** That field
  records the resolved channel numbers, colours and `value_var`. A plot without it can't be
  rebuilt and is left unchanged, with a warning.
  - **Swap in a new raster layer; don't edit it in place.** Layers are environments shared with
    the original plot, so assigning to `p$layers[[k]]$data` changes the original object too.
    `ggproto(NULL, layer)` overflows the S7 stack at build time, just as it does for guides.
- **The example TIFF holds small floats.** DAPI (channel 1) reaches about 115, and the other
  channels are below about 20. Several channels are entirely zero in small regions, so
  `TiffRect(900, 1000, 1400, 1500)` is a poor test region. Tests use
  `TiffRect(900, 2300, 1400, 2800)` at resolution 4.

## Tooling

- Optional dependencies (Suggests) are `ggnewscale` (needed by `add_points()` on plots that
  already have a colour scale) and `EBImage` (needed by `label_polygons()`). Both are
  installed in `R_LIBS_USER`.
- DESCRIPTION has an explicit `Collate:`, so a new `R/*.R` file must be added there.
- Code uses `future`, `future.apply`, `parallel`, `data.table` and `tibble`, but none of
  them are declared in DESCRIPTION.
