// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.fits

import cats.effect.*
import cats.syntax.all.*
import fs2.*
import fs2.io.readClassResource
import munit.CatsEffectSuite

/**
 * Exercises the FITS reader against real mask files produced by GMMPS.
 *
 * The two fixtures are a deliberate pair: `ngc7796_ODF.fits` is a GMOS-S design and `n159_ODF.fits`
 * a Flamingos-2 one, so between them they cover both dispersion directions and both slit size
 * conventions.
 */
class FitsReaderSuite extends CatsEffectSuite:

  private def bytes(resource: String): Stream[IO, Byte] =
    readClassResource[IO, FitsReaderSuite](resource)

  test("read all header data units of a GMOS-S mask"):
    bytes("/ngc7796_ODF.fits")
      .through(Fits.headers[IO])
      .compile
      .toList
      .map: hs =>
        assertEquals(hs.length, 2)
        assert(hs.head.isPrimary)
        assert(hs(1).isBinaryTable)
        assertEquals(hs(1).string("EXTNAME"), "MINIMAL.TAB".some)
        assertEquals(hs(1).string("INSTRUME"), "GMOS-S".some)
        assertEquals(hs(1).string("DISPDIR"), "horizontal".some)
        assertEquals(hs(1).double("PIXSCALE"), 0.16.some)
        assertEquals(hs(1).double("MASK_PA"), 160.1.some)
        assertEquals(hs(1).string("GMMPSVER"), "1.4.3".some)

  test("duplicate keywords resolve to the first occurrence"):
    bytes("/ngc7796_ODF.fits")
      .through(Fits.binaryTable[IO])
      .compile
      .lastOrError
      .map: t =>
        assertEquals(t.header.cards.count(_.keyword == "FILE_OT"), 2)
        assertEquals(t.header.string("FILE_OT"), "preimage.sex_OT.fits".some)

  test("commentary cards are not retained"):
    bytes("/ngc7796_ODF.fits")
      .through(Fits.headers[IO])
      .head
      .compile
      .lastOrError
      .map: h =>
        assert(h.cards.forall(c => c.keyword != "COMMENT" && c.keyword != "HISTORY"))
        assertEquals(h.rawValues.get("COMMENT"), None)
        // The value carrying cards of the same header are retained.
        assertEquals(h.boolean("SIMPLE"), true.some)
        assertEquals(h.int("NAXIS"), 0.some)

  test("describe the binary table of a GMOS-S mask"):
    bytes("/ngc7796_ODF.fits")
      .through(Fits.binaryTable[IO])
      .compile
      .lastOrError
      .map: t =>
        assertEquals(t.rowLength, 66)
        assertEquals(t.rowCount, 40L)
        assertEquals(t.columns.length, 18)
        assertEquals(
          t.columnNames.toList,
          List(
            "ID",
            "RA",
            "DEC",
            "x_ccd",
            "y_ccd",
            "slitpos_x",
            "slitpos_y",
            "slitsize_x",
            "slitsize_y",
            "slittilt",
            "MAG",
            "priority",
            "slittype",
            "redshift",
            "specleft",
            "specright",
            "specbottom",
            "spectop"
          )
        )
        // Byte offsets are what a mis-decode would get wrong first.
        assertEquals(t.column("ID").map(_.byteOffset), 0.some)
        assertEquals(t.column("priority").map(_.byteOffset), 44.some)
        assertEquals(t.column("slittype").map(_.byteOffset), 45.some)
        assertEquals(t.column("spectop").map(_.byteOffset), 62.some)
        assertEquals(t.column("RA").map(_.unit), "H".some.some)
        assertEquals(t.column("slitsize_x").map(_.unit), "arcsec".some.some)

  test("decode the first row of a GMOS-S mask"):
    bytes("/ngc7796_ODF.fits")
      .through(Fits.binaryTableRows[IO])
      .head
      .compile
      .lastOrError
      .map: r =>
        // Exact, not approximate. The columns are 4-byte floats, so every value the file holds
        // widens to a double it hits precisely
        assertEquals(r.get("ID").flatMap(_.asInt), 10.some)
        assertEquals(r.get("RA").flatMap(_.asDouble), 23.98309326171875.some)
        assertEquals(r.get("DEC").flatMap(_.asDouble), (-55.5241813659668).some)
        assertEquals(r.get("x_ccd").flatMap(_.asDouble), 765.2130126953125.some)
        assertEquals(r.get("y_ccd").flatMap(_.asDouble), 70.89219665527344.some)
        assertEquals(r.get("slitsize_x").flatMap(_.asDouble), 1.0.some)
        assertEquals(r.get("slitsize_y").flatMap(_.asDouble), 4.0.some)
        assertEquals(r.get("MAG").flatMap(_.asDouble), 11.345999717712402.some)
        assertEquals(r.get("priority").flatMap(_.asChar), '2'.some)
        assertEquals(r.get("slittype").flatMap(_.asChar), 'R'.some)
        assertEquals(r.get("specleft").flatMap(_.asDouble), 277.56280517578125.some)
        assertEquals(r.get("spectop").flatMap(_.asDouble), 83.38123321533203.some)

  test("decode the first row of a Flamingos-2 mask"):
    // An acquisition star: mask design forces these to a 2 x 2 arcsecond square.
    bytes("/n159_ODF.fits")
      .through(Fits.binaryTableRows[IO])
      .head
      .compile
      .lastOrError
      .map: r =>
        assertEquals(r.get("ID").flatMap(_.asInt), 3550.some)
        assertEquals(r.get("RA").flatMap(_.asDouble), 5.66439151763916.some)
        assertEquals(r.get("DEC").flatMap(_.asDouble), (-69.77568817138672).some)
        assertEquals(r.get("slitsize_x").flatMap(_.asDouble), 2.0.some)
        assertEquals(r.get("slitsize_y").flatMap(_.asDouble), 2.0.some)
        assertEquals(r.get("priority").flatMap(_.asChar), '0'.some)

  test("read exactly the declared number of rows, not the block padding"):
    bytes("/n159_ODF.fits")
      .through(Fits.binaryTableRows[IO])
      .compile
      .toList
      .map(rs => assertEquals(rs.length, 53))

  test("row count of a GMOS-S mask"):
    bytes("/ngc7796_ODF.fits")
      .through(Fits.binaryTableRows[IO])
      .compile
      .toList
      .map(rs => assertEquals(rs.length, 40))

  test("decode the last row of a Flamingos-2 mask"):
    bytes("/n159_ODF.fits")
      .through(Fits.binaryTableRows[IO])
      .compile
      .lastOrError
      .map: r =>
        assertEquals(r.get("slittype").flatMap(_.asChar), 'R'.some)
        assert(r.get("ID").flatMap(_.asInt).exists(_ > 0))
