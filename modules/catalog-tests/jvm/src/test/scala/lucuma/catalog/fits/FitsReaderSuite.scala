// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.fits

import cats.effect.*
import fs2.*
import fs2.io.file.Files
import fs2.io.file.Path
import munit.CatsEffectSuite

/**
 * Exercises the FITS reader against real mask files produced by GMMPS.
 *
 * The two fixtures are a deliberate pair: `ngc7796_ODF.fits` is a GMOS-S design and
 * `n159_ODF.fits` a Flamingos-2 one, so between them they cover both dispersion directions and
 * both slit size conventions.
 */
class FitsReaderSuite extends CatsEffectSuite:

  private def bytes(resource: String): Stream[IO, Byte] =
    Stream
      .eval(IO(getClass().getResource(resource).getPath()))
      .flatMap(p => Files[IO].readAll(Path(p)))

  test("read all header data units of a GMOS-S mask"):
    bytes("/ngc7796_ODF.fits")
      .through(Fits.headers[IO])
      .compile
      .toList
      .map: hs =>
        assertEquals(hs.length, 2)
        assert(hs.head.isPrimary)
        assert(hs(1).isBinaryTable)
        assertEquals(hs(1).string("EXTNAME"), Some("MINIMAL.TAB"))
        assertEquals(hs(1).string("INSTRUME"), Some("GMOS-S"))
        assertEquals(hs(1).string("DISPDIR"), Some("horizontal"))
        assertEquals(hs(1).double("PIXSCALE"), Some(0.16))
        assertEquals(hs(1).double("MASK_PA"), Some(160.1))
        assertEquals(hs(1).string("GMMPSVER"), Some("1.4.3"))

  test("duplicate keywords resolve to the first occurrence"):
    // Both fixtures carry FILE_OT twice, a known GMMPS defect: the .cfg value is written first,
    // then overwritten with the *output* file name.
    bytes("/ngc7796_ODF.fits")
      .through(Fits.binaryTable[IO])
      .compile
      .lastOrError
      .map: t =>
        assertEquals(t.header.cards.count(_.keyword == "FILE_OT"), 2)
        assertEquals(t.header.string("FILE_OT"), Some("preimage.sex_OT.fits"))

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
          List("ID", "RA", "DEC", "x_ccd", "y_ccd", "slitpos_x", "slitpos_y", "slitsize_x",
               "slitsize_y", "slittilt", "MAG", "priority", "slittype", "redshift", "specleft",
               "specright", "specbottom", "spectop")
        )
        // Byte offsets are what a mis-decode would get wrong first.
        assertEquals(t.column("ID").map(_.byteOffset), Some(0))
        assertEquals(t.column("priority").map(_.byteOffset), Some(44))
        assertEquals(t.column("slittype").map(_.byteOffset), Some(45))
        assertEquals(t.column("spectop").map(_.byteOffset), Some(62))
        assertEquals(t.column("RA").map(_.unit), Some(Some("H")))
        assertEquals(t.column("slitsize_x").map(_.unit), Some(Some("arcsec")))

  test("decode the first row of a GMOS-S mask"):
    bytes("/ngc7796_ODF.fits")
      .through(Fits.binaryTableRows[IO])
      .head
      .compile
      .lastOrError
      .map: r =>
        assertEquals(r.get("ID").flatMap(_.asInt), Some(10))
        assertEqualsDouble(r.get("RA").flatMap(_.asDouble).get, 23.98309, 1e-5)
        assertEqualsDouble(r.get("DEC").flatMap(_.asDouble).get, -55.52418, 1e-5)
        assertEqualsDouble(r.get("x_ccd").flatMap(_.asDouble).get, 765.213, 1e-3)
        assertEqualsDouble(r.get("y_ccd").flatMap(_.asDouble).get, 70.892, 1e-3)
        assertEqualsDouble(r.get("slitsize_x").flatMap(_.asDouble).get, 1.0, 1e-6)
        assertEqualsDouble(r.get("slitsize_y").flatMap(_.asDouble).get, 4.0, 1e-6)
        assertEqualsDouble(r.get("MAG").flatMap(_.asDouble).get, 11.346, 1e-3)
        assertEquals(r.get("priority").flatMap(_.asChar), Some('2'))
        assertEquals(r.get("slittype").flatMap(_.asChar), Some('R'))
        assertEqualsDouble(r.get("specleft").flatMap(_.asDouble).get, 277.563, 1e-3)
        assertEqualsDouble(r.get("spectop").flatMap(_.asDouble).get, 83.381, 1e-3)

  test("decode the first row of a Flamingos-2 mask"):
    // An acquisition star: mask design forces these to a 2 x 2 arcsecond square.
    bytes("/n159_ODF.fits")
      .through(Fits.binaryTableRows[IO])
      .head
      .compile
      .lastOrError
      .map: r =>
        assertEquals(r.get("ID").flatMap(_.asInt), Some(3550))
        assertEqualsDouble(r.get("RA").flatMap(_.asDouble).get, 5.66439, 1e-5)
        assertEqualsDouble(r.get("DEC").flatMap(_.asDouble).get, -69.77569, 1e-5)
        assertEqualsDouble(r.get("slitsize_x").flatMap(_.asDouble).get, 2.0, 1e-6)
        assertEqualsDouble(r.get("slitsize_y").flatMap(_.asDouble).get, 2.0, 1e-6)
        assertEquals(r.get("priority").flatMap(_.asChar), Some('0'))

  test("read exactly the declared number of rows, not the block padding"):
    // n159 has 53 rows of 66 bytes = 3498, in a data section padded to 5760. Reading to the end
    // of the section instead of to NAXIS2 would yield 87 rows of mostly zeroes.
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
        assertEquals(r.get("slittype").flatMap(_.asChar), Some('R'))
        assert(r.get("ID").flatMap(_.asInt).exists(_ > 0))
