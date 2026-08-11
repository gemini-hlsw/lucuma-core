// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.mos

import cats.effect.*
import cats.syntax.all.*
import fs2.*
import fs2.io.file.Files
import fs2.io.file.Path
import lucuma.catalog.fits.FitsProblem
import lucuma.core.enums.Instrument
import lucuma.core.enums.MosDispersionDirection
import lucuma.core.enums.MosSlitPriority
import lucuma.core.enums.MosSlitType
import lucuma.core.math.Angle
import lucuma.core.model.mos.MosNodAndShuffle
import munit.CatsEffectSuite

/**
 * Exercises the mask reader against real GMMPS designs.
 *
 * `ngc7796_ODF.fits` is GMOS-S and disperses horizontally; `n159_ODF.fits` is Flamingos-2 and
 * disperses vertically. Between them they pin the axis convention in both directions, which is the
 * single thing this layer exists to get right.
 */
class MosMaskReaderSuite extends CatsEffectSuite:

  private def bytes(resource: String): Stream[IO, Byte] =
    Stream
      .eval(IO(getClass().getResource(resource).getPath()))
      .flatMap(p => Files[IO].readAll(Path(p)))

  private def arcsec(a: Angle): Double =
    Angle.signedDecimalArcseconds.get(a).toDouble

  private def degrees(a: Angle): Double =
    a.toSignedDoubleDegrees

  // -- Header -------------------------------------------------------------------------------

  test("decode the header of a GMOS-S design"):
    bytes("/ngc7796_ODF.fits")
      .through(MosMaskReader.header[IO])
      .compile
      .lastOrError
      .map: h =>
        assertEquals(h.instrument, Instrument.GmosSouth)
        assertEquals(h.dispersionDirection, MosDispersionDirection.Horizontal)
        assertEqualsDouble(h.pixelScale, 0.16, 1e-9)
        assertEqualsDouble(h.pointing.ra.toAngle.toDoubleDegrees, 359.7794, 1e-4)
        assertEqualsDouble(h.pointing.dec.toAngle.toSignedDoubleDegrees, -55.47128, 1e-5)
        assertEqualsDouble(h.positionAngle.map(degrees).get, 160.1, 1e-4)
        assertEquals(h.hasTiltedSlits, false)
        assertEquals(h.spectroscopy.filter, Some("r"))
        assertEquals(h.spectroscopy.grating, Some("R400"))
        assertEquals(h.provenance.softwareVersion, Some("1.4.3"))
        assertEquals(h.provenance.designer, Some("mischa"))
        // The first of the two FILE_OT cards wins.
        assertEquals(h.provenance.sourceObjectTable, Some("preimage.sex_OT.fits"))

  test("decode the header of a Flamingos-2 design"):
    bytes("/n159_ODF.fits")
      .through(MosMaskReader.header[IO])
      .compile
      .lastOrError
      .map: h =>
        assertEquals(h.instrument, Instrument.Flamingos2)
        assertEquals(h.dispersionDirection, MosDispersionDirection.Vertical)
        assertEqualsDouble(h.pixelScale, 0.1792, 1e-9)
        assertEquals(h.spectroscopy.grating, Some("R1200_JH"))
        assertEqualsDouble(h.spectroscopy.dispersion.get, 0.6667, 1e-6)

  test("GMOS-S design is microshuffling"):
    bytes("/ngc7796_ODF.fits")
      .through(MosMaskReader.header[IO])
      .compile
      .lastOrError
      .map: h =>
        h.nodAndShuffle match
          case MosNodAndShuffle.MicroShuffle(distance, binning, slitLength) =>
            assertEquals(distance, 50)
            assertEquals(binning, 2)
            assertEqualsDouble(arcsec(slitLength), 4.0, 1e-6)
          case other                                                        =>
            fail(s"expected microShuffle, got $other")

  test("Flamingos-2 design is not Nod & Shuffle"):
    bytes("/n159_ODF.fits")
      .through(MosMaskReader.header[IO])
      .compile
      .lastOrError
      .map(h => assertEquals(h.nodAndShuffle, MosNodAndShuffle.None))

  test("the header pipe does not read the slit rows"):
    // It should stop after the extension header. If it were draining the table it would still
    // succeed, so assert on bytes consumed rather than on the result.
    Ref[IO]
      .of(0L)
      .flatMap: counter =>
        bytes("/n159_ODF.fits")
          .evalTap(_ => counter.update(_ + 1))
          .through(MosMaskReader.header[IO])
          .compile
          .lastOrError *> counter.get
      .map: consumed =>
        // The extension header ends well before the 5760 byte data section that follows it.
        assert(consumed < 17280, s"header pipe consumed $consumed bytes")

  // -- Slits: the axis convention ----------------------------------------------------------

  test("GMOS-S disperses horizontally, so slitsize_x is the width"):
    bytes("/ngc7796_ODF.fits")
      .through(MosMaskReader.slits[IO])
      .head
      .compile
      .lastOrError
      .map: s =>
        // The file carries slitsize_x = 1.0, slitsize_y = 4.0.
        assertEqualsDouble(arcsec(s.slitWidth), 1.0, 1e-6)
        assertEqualsDouble(arcsec(s.slitLength), 4.0, 1e-6)

  test("Flamingos-2 disperses vertically, so slitsize_x is the length"):
    bytes("/n159_ODF.fits")
      .through(MosMaskReader.slits[IO])
      .drop(1) // row 0 is an acquisition star, forced square, so it cannot show the swap
      .head
      .compile
      .lastOrError
      .map: s =>
        // The file carries slitsize_x = 5.0, slitsize_y = 1.0 — the transpose of the GMOS case.
        assertEqualsDouble(arcsec(s.slitWidth), 1.0, 1e-6)
        assertEqualsDouble(arcsec(s.slitLength), 5.0, 1e-6)

  // -- Slits: values -----------------------------------------------------------------------

  test("decode the first slit of a GMOS-S design"):
    bytes("/ngc7796_ODF.fits")
      .through(MosMaskReader.slits[IO])
      .head
      .compile
      .lastOrError
      .map: s =>
        assertEquals(s.id.value, 10)
        // The RA column is in hours: 23.98309 h x 15 = 359.746 degrees.
        assertEqualsDouble(s.coordinates.ra.toAngle.toDoubleDegrees, 359.74634, 1e-4)
        assertEqualsDouble(s.coordinates.dec.toAngle.toSignedDoubleDegrees, -55.52418, 1e-5)
        assertEqualsDouble(s.x, 765.213, 1e-3)
        assertEqualsDouble(s.y, 70.892, 1e-3)
        assertEqualsDouble(s.magnitude, 11.346, 1e-3)
        assertEqualsDouble(arcsec(s.tilt), 0.0, 1e-6)
        assertEquals(s.priority, MosSlitPriority.Medium)
        assertEquals(s.slitType, MosSlitType.Rectangular)
        assertEquals(s.isAcquisition, false)
        assertEqualsDouble(s.spectrumFootprint.get.left, 277.563, 1e-3)
        assertEqualsDouble(s.spectrumFootprint.get.top, 83.381, 1e-3)

  test("acquisition stars are recognised"):
    bytes("/n159_ODF.fits")
      .through(MosMaskReader.slits[IO])
      .head
      .compile
      .lastOrError
      .map: s =>
        assertEquals(s.id.value, 3550)
        assertEquals(s.priority, MosSlitPriority.Acquisition)
        assert(s.isAcquisition)
        // Mask design forces acquisition apertures to a 2 x 2 arcsecond square.
        assertEqualsDouble(arcsec(s.slitWidth), 2.0, 1e-6)
        assertEqualsDouble(arcsec(s.slitLength), 2.0, 1e-6)

  test("slit counts and priority distribution"):
    bytes("/n159_ODF.fits")
      .through(MosMaskReader.slits[IO])
      .compile
      .toList
      .map: ss =>
        assertEquals(ss.length, 53)
        assertEquals(ss.count(_.priority === MosSlitPriority.Acquisition), 3)
        assertEquals(ss.count(_.priority === MosSlitPriority.High), 10)
        assertEquals(ss.count(_.priority === MosSlitPriority.Medium), 40)
        assert(ss.forall(_.slitType === MosSlitType.Rectangular))

  test("every slit of the GMOS-S design decodes"):
    bytes("/ngc7796_ODF.fits")
      .through(MosMaskReader.slits[IO])
      .compile
      .toList
      .map: ss =>
        assertEquals(ss.length, 40)
        assertEquals(ss.count(_.isAcquisition), 3)
