// Crop the first image of a pyramidal OME-TIFF and write it as a new pyramidal
// OME-TIFF: tiled, deflate-compressed, with sub-resolutions made by 2x2 averaging.
// Channel names, colours and other channel attributes, and the physical pixel
// size are carried over.
//
// Run with Java 11+ and the Bio-Formats jars on the classpath (Fiji's jars/ work):
//   java -cp "<fiji>/jars/*" CropOmeTiff.java <in> <out> <x> <y> <width> <height> <levels>
// Only 32-bit float images are supported, which is what the TiffPlotR example is.

import loci.common.DataTools;
import loci.common.services.ServiceFactory;
import loci.formats.FormatTools;
import loci.formats.ImageReader;
import loci.formats.MetadataTools;
import loci.formats.meta.IMetadata;
import loci.formats.meta.IPyramidStore;
import loci.formats.out.OMETiffWriter;
import loci.formats.out.PyramidOMETiffWriter;
import loci.formats.services.OMEXMLService;
import ome.xml.model.primitives.PositiveInteger;

public class CropOmeTiff {
  public static void main(String[] args) throws Exception {
    if (args.length != 7) {
      System.err.println("usage: CropOmeTiff <in> <out> <x> <y> <width> <height> <levels>");
      System.exit(2);
    }
    String in = args[0], out = args[1];
    int x = Integer.parseInt(args[2]), y = Integer.parseInt(args[3]);
    int w = Integer.parseInt(args[4]), h = Integer.parseInt(args[5]);
    int levels = Integer.parseInt(args[6]);
    if (w % (1 << (levels - 1)) != 0 || h % (1 << (levels - 1)) != 0) {
      throw new IllegalArgumentException("width and height must be divisible by 2^(levels - 1)");
    }

    OMEXMLService service = new ServiceFactory().getInstance(OMEXMLService.class);
    IMetadata src = service.createOMEXMLMetadata();
    ImageReader reader = new ImageReader();
    reader.setFlattenedResolutions(false);
    reader.setMetadataStore(src);
    reader.setId(in);
    reader.setSeries(0);
    reader.setResolution(0);
    if (reader.getPixelType() != FormatTools.FLOAT) throw new IllegalArgumentException("only float images are supported");
    if (x + w > reader.getSizeX() || y + h > reader.getSizeY()) throw new IllegalArgumentException("crop is outside the image");
    int nC = reader.getSizeC();
    boolean little = reader.isLittleEndian();

    // full-resolution crop, one float plane per channel
    float[][] planes = new float[nC][];
    for (int c = 0; c < nC; c++) {
      byte[] bytes = reader.openBytes(reader.getIndex(0, c, 0), x, y, w, h);
      planes[c] = (float[]) DataTools.makeDataArray(bytes, 4, true, little);
    }
    reader.close();

    IMetadata dst = service.createOMEXMLMetadata();
    MetadataTools.populateMetadata(dst, 0, src.getImageName(0), little, "XYCZT",
        FormatTools.getPixelTypeString(FormatTools.FLOAT), w, h, 1, nC, 1, 1);
    if (src.getPixelsPhysicalSizeX(0) != null) dst.setPixelsPhysicalSizeX(src.getPixelsPhysicalSizeX(0), 0);
    if (src.getPixelsPhysicalSizeY(0) != null) dst.setPixelsPhysicalSizeY(src.getPixelsPhysicalSizeY(0), 0);
    for (int c = 0; c < nC; c++) {
      if (src.getChannelName(0, c) != null) dst.setChannelName(src.getChannelName(0, c), 0, c);
      if (src.getChannelColor(0, c) != null) dst.setChannelColor(src.getChannelColor(0, c), 0, c);
      if (src.getChannelFluor(0, c) != null) dst.setChannelFluor(src.getChannelFluor(0, c), 0, c);
      if (src.getChannelEmissionWavelength(0, c) != null) dst.setChannelEmissionWavelength(src.getChannelEmissionWavelength(0, c), 0, c);
      if (src.getChannelExcitationWavelength(0, c) != null) dst.setChannelExcitationWavelength(src.getChannelExcitationWavelength(0, c), 0, c);
    }
    IPyramidStore pyramid = (IPyramidStore) dst;
    for (int r = 1; r < levels; r++) {
      pyramid.setResolutionSizeX(new PositiveInteger(w >> r), 0, r);
      pyramid.setResolutionSizeY(new PositiveInteger(h >> r), 0, r);
    }

    // PyramidOMETiffWriter links the sub-resolutions to each plane as SubIFDs;
    // plain OMETiffWriter writes them as extra top-level IFDs that readers ignore
    OMETiffWriter writer = new PyramidOMETiffWriter();
    writer.setMetadataRetrieve(dst);
    writer.setWriteSequentially(true);
    writer.setCompression(OMETiffWriter.COMPRESSION_ZLIB);
    writer.setId(out);
    writer.setSeries(0);
    // every level of every channel, [level][channel]
    float[][][] pyr = new float[levels][][];
    pyr[0] = planes;
    for (int r = 1; r < levels; r++) {
      pyr[r] = new float[nC][];
      for (int c = 0; c < nC; c++) pyr[r][c] = halve(pyr[r - 1][c], w >> (r - 1), h >> (r - 1));
    }
    // all full-resolution planes first, then each sub-resolution in turn
    for (int r = 0; r < levels; r++) {
      writer.setResolution(r);
      writer.setTileSizeX(Math.min(256, w >> r));
      writer.setTileSizeY(Math.min(256, h >> r));
      for (int c = 0; c < nC; c++) writer.saveBytes(c, DataTools.floatsToBytes(pyr[r][c], little));
    }
    writer.close();
    for (int r = 0; r < levels; r++) System.out.println("level " + r + ": " + (w >> r) + " x " + (h >> r));
  }

  // 2x2 mean, for a w x h row-major plane with even w and h
  static float[] halve(float[] p, int w, int h) {
    int w2 = w / 2, h2 = h / 2;
    float[] out = new float[w2 * h2];
    for (int j = 0; j < h2; j++) {
      for (int i = 0; i < w2; i++) {
        int k = 2 * j * w + 2 * i;
        out[j * w2 + i] = (p[k] + p[k + 1] + p[k + w] + p[k + w + 1]) / 4f;
      }
    }
    return out;
  }
}
