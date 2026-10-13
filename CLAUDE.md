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

## Tooling

- Optional dependencies (Suggests) are `ggnewscale` (needed by `add_points()` on plots that
  already have a colour scale) and `EBImage` (needed by `label_polygons()`). Both are
  installed in `R_LIBS_USER`.
- DESCRIPTION has an explicit `Collate:`, so a new `R/*.R` file must be added there.
- Code uses `future`, `future.apply`, `parallel`, `data.table` and `tibble`, but none of
  them are declared in DESCRIPTION.
