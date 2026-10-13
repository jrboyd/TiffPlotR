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
