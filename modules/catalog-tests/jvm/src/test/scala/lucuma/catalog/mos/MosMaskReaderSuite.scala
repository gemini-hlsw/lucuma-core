// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.mos

import cats.effect.*
import cats.syntax.all.*
import coulomb.syntax.*
import fs2.*
import fs2.io.readClassResource
import lucuma.core.enums.Instrument
import lucuma.core.enums.MosDispersionDirection
import lucuma.core.enums.MosSlitPriority
import lucuma.core.enums.MosSlitType
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.syntax.int.*
import lucuma.core.math.syntax.units.*
import lucuma.core.math.units.NanometersPerPixel
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
    readClassResource[IO, MosMaskReaderSuite](resource)

  /** Sexagesimal is exact here: a decoded coordinate round trips through this form unchanged. */
  private def coordinates(hmsDms: String): Coordinates =
    Coordinates.fromHmsDms.getOption(hmsDms).getOrElse(fail(s"not a coordinate: $hmsDms"))

  // -- Header -------------------------------------------------------------------------------

  test("decode the header of a GMOS-S design"):
    bytes("/ngc7796_ODF.fits")
      .through(MosMaskReader.header[IO])
      .compile
      .lastOrError
      .map: h =>
        assertEquals(h.instrument, Instrument.GmosSouth)
        assertEquals(h.dispersionDirection, MosDispersionDirection.Horizontal)
        assertEquals(h.pixelScale, BigDecimal(0.16).pixelScale)
        // RA_IMAG and DEC_IMAG are degrees, unlike the table's RA column. 359.7794 deg / 15.
        assertEquals(h.pointing, coordinates("23:59:07.055999 -55:28:16.608000"))
        assertEquals(h.positionAngle, Angle.fromDoubleDegrees(160.1).some)
        assertEquals(h.hasTiltedSlits, false)
        assertEquals(h.spectroscopy.filter, "r".some)
        assertEquals(h.spectroscopy.grating, "R400".some)
        assertEquals(h.provenance.softwareVersion, "1.4.3".some)
        assertEquals(h.provenance.designer, "mischa".some)
        // The first of the two FILE_OT cards wins.
        assertEquals(h.provenance.sourceObjectTable, "preimage.sex_OT.fits".some)

  test("decode the header of a Flamingos-2 design"):
    bytes("/n159_ODF.fits")
      .through(MosMaskReader.header[IO])
      .compile
      .lastOrError
      .map: h =>
        assertEquals(h.instrument, Instrument.Flamingos2)
        assertEquals(h.dispersionDirection, MosDispersionDirection.Vertical)
        assertEquals(h.pixelScale, BigDecimal(0.1792).pixelScale)
        assertEquals(h.spectroscopy.grating, "R1200_JH".some)
        assertEquals(h.spectroscopy.dispersion,
                     BigDecimal(0.6667).withUnit[NanometersPerPixel].some
        )

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
            assertEquals(slitLength, 4.arcsec)
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
        assertEquals(s.slitWidth, 1.arcsec)
        assertEquals(s.slitLength, 4.arcsec)

  test("Flamingos-2 disperses vertically, so slitsize_x is the length"):
    bytes("/n159_ODF.fits")
      .through(MosMaskReader.slits[IO])
      .drop(1) // row 0 is an acquisition star, forced square
      .head
      .compile
      .lastOrError
      .map: s =>
        // The file carries slitsize_x = 5.0, slitsize_y = 1.0 — the transpose of the GMOS case.
        assertEquals(s.slitWidth, 1.arcsec)
        assertEquals(s.slitLength, 5.arcsec)

  // -- Slits: values -----------------------------------------------------------------------

  test("decode the first slit of a GMOS-S design"):
    bytes("/ngc7796_ODF.fits")
      .through(MosMaskReader.slits[IO])
      .head
      .compile
      .lastOrError
      .map: s =>
        assertEquals(s.id.value, 10)
        // The file holds RA = 23.9830933 in hours, which is exactly the 23:58:59.135742 below.
        // Sexagesimal makes that legible, and the comparison is exact, so a reader that forgot to
        // scale hours to degrees could not pass this.
        assertEquals(s.coordinates, coordinates("23:58:59.135742 -55:31:27.052917"))
        assertEqualsDouble(s.x, 765.2130127, 1e-6)
        assertEqualsDouble(s.y, 70.8921967, 1e-6)
        assertEqualsDouble(s.magnitude.value.value.toDouble, 11.3459997, 1e-6)
        assertEquals(s.tilt, Angle.Angle0)
        assertEquals(s.priority, MosSlitPriority.Medium)
        assertEquals(s.slitType, MosSlitType.Rectangular)
        assertEquals(s.isAcquisition, false)
        assertEqualsDouble(s.spectrumFootprint.get.left, 277.5628052, 1e-6)
        assertEqualsDouble(s.spectrumFootprint.get.top, 83.3812332, 1e-6)

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
        assertEquals(s.slitWidth, 2.arcsec)
        assertEquals(s.slitLength, 2.arcsec)

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
