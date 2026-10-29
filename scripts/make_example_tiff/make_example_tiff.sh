#!/usr/bin/env bash
# Regenerate inst/extdata/example_crop.ome.tiff, the example image returned by
# exampleTiff(): a 1024 x 1024 crop of 301_CellPellet_NegCTL_Scan1_PC3Guise.ome.tiff
# (8651 x 5998, 6 channels, float32) around the cell pellet's edge, with 4
# resolution levels. Light enough (one thread, under 1 GB) to run inline.
#
# usage: scripts/make_example_tiff/make_example_tiff.sh <source.ome.tiff> [fiji_dir]
set -euo pipefail

SRC=${1:?usage: make_example_tiff.sh <source.ome.tiff> [fiji_dir]}
FIJI=${2:-/gpfs1/home/j/r/jrboyd/tools/Fiji}
JAVA=$(ls -d "$FIJI"/java/linux64/*/bin/java | head -n 1)
HERE=$(cd "$(dirname "$0")" && pwd)
OUT="$HERE/../../inst/extdata/example_crop.ome.tiff"

rm -f "$OUT"
"$JAVA" -Xmx1g -XX:+UseSerialGC -XX:ActiveProcessorCount=1 -cp "$FIJI/jars/*" \
  "$HERE/CropOmeTiff.java" "$SRC" "$OUT" 1000 1500 1024 1024 4
ls -l "$OUT"
