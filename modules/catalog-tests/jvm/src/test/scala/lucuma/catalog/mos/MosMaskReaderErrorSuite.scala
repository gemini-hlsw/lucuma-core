// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.mos

import cats.effect.*
import cats.syntax.all.*
import fs2.*
import lucuma.catalog.fits.Fits
import lucuma.catalog.fits.FitsProblem
import lucuma.core.enums.MosSlitPriority
import munit.CatsEffectSuite

import java.nio.file.Files as JFiles
import java.nio.file.Paths

/**
 * Failure paths, exercised against deliberately damaged copies of a real design.
 *
 * The damage is applied in memory rather than committed as extra fixtures, so these cases cannot
 * drift away from the golden file they are derived from.
 */
class MosMaskReaderErrorSuite extends CatsEffectSuite:

  private val BlockSize = Fits.BlockSize

  private def golden: Array[Byte] =
    JFiles.readAllBytes(Paths.get(getClass().getResource("/ngc7796_ODF.fits").getPath()))

  private def stream(bs: Array[Byte]): Stream[IO, Byte] =
    Stream.emits(bs).covary[IO]

  /** Replaces an ASCII run in the header area with a replacement of exactly the same length. */
  private def patch(bs: Array[Byte], from: String, to: String): Array[Byte] =
    assertEquals(from.length, to.length, "patch must preserve card width")
    val text  = new String(bs.map(b => (b & 0xff).toChar))
    val index = text.indexOf(from)
    assert(index >= 0, s"'$from' not found in fixture")
    val out   = bs.clone()
    to.zipWithIndex.foreach((c, i) => out(index + i) = c.toByte)
    out

  /** Byte offset at which the first extension's data section begins. */
  private def dataOffset(bs: Array[Byte]): Int =
    var block = 0
    var ends  = 0
    while ends < 2 && block * BlockSize < bs.length do
      val base   = block * BlockSize
      val hasEnd = (0 until 36).exists: c =>
        val o = base + c * 80
        o + 3 <= bs.length && (0 until 3).forall(k => (bs(o + k) & 0xff).toChar === "END".charAt(k))
      block += 1
      if hasEnd then ends += 1
    block * BlockSize

  private def failsWith[A](s: Stream[IO, A])(check: Throwable => Boolean): IO[Unit] =
    s.compile.drain.attempt.map:
      case Left(t)  => assert(check(t), s"unexpected failure: $t")
      case Right(_) => fail("expected the stream to fail")

  // -- 1. Truncated mid row -----------------------------------------------------------------

  test("a table truncated mid row is reported as truncated"):
    // Keep the headers and a few whole rows, then cut partway through the next one.
    val bs = golden.take(dataOffset(golden) + 66 * 5 + 30)
    failsWith(stream(bs).through(MosMaskReader.slits[IO])):
      case MosMaskProblem.Fits(FitsProblem.TruncatedData(40L, actual)) => actual < 40L
      case FitsProblem.TruncatedData(40L, actual)                      => actual < 40L
      case _                                                           => false

  // -- 2. Not a FITS file -------------------------------------------------------------------

  test("a file that does not begin with SIMPLE is rejected"):
    val bs = patch(golden, "SIMPLE  =", "SIMPLX  =")
    failsWith(stream(bs).through(MosMaskReader.slits[IO])):
      case FitsProblem.NotFitsFormat                      => true
      case MosMaskProblem.Fits(FitsProblem.NotFitsFormat) => true
      case _                                              => false

  // -- 3. No binary table -------------------------------------------------------------------

  test("a FITS file with no binary table is rejected"):
    // The primary header alone is a structurally valid FITS file carrying no data.
    val bs = golden.take(BlockSize)
    failsWith(stream(bs).through(MosMaskReader.slits[IO])):
      case FitsProblem.NoBinaryTable                      => true
      case MosMaskProblem.Fits(FitsProblem.NoBinaryTable) => true
      case _                                              => false

  // -- 4. Missing mandatory column ----------------------------------------------------------

  test("a design missing a mandatory column names the column"):
    // Renaming the TTYPE leaves the byte layout intact but removes the column by name.
    val bs = patch(golden, "TTYPE12 = 'priority'", "TTYPE12 = 'prioritX'")
    failsWith(stream(bs).through(MosMaskReader.slits[IO])):
      case MosMaskProblem.MissingColumns(cs) => cs.toList === List("priority")
      case _                                 => false

  test("missing columns are reported together, not one at a time"):
    val bs = patch(
      patch(golden, "TTYPE12 = 'priority'", "TTYPE12 = 'prioritX'"),
      "TTYPE13 = 'slittype'",
      "TTYPE13 = 'slittypX'"
    )
    failsWith(stream(bs).through(MosMaskReader.slits[IO])):
      case MosMaskProblem.MissingColumns(cs) => cs.toList === List("priority", "slittype")
      case _                                 => false

  // -- 5. Declared row length disagrees with the columns ------------------------------------

  test("a header that lies about the row length is rejected"):
    // Without this check the stride would be wrong and every row would decode to plausible
    // nonsense rather than failing.
    val bs = patch(golden, "NAXIS1  =                   66", "NAXIS1  =                   67")
    failsWith(stream(bs).through(MosMaskReader.slits[IO])):
      case FitsProblem.RowLengthMismatch(67, 66)                      => true
      case MosMaskProblem.Fits(FitsProblem.RowLengthMismatch(67, 66)) => true
      case _                                                          => false

  // -- Unrecognised enumerated values -------------------------------------------------------

  test("an unrecognised priority is rejected, with the row it came from"):
    // Deliberately stricter than GMMPS. The output of this pipeline is a plate that gets cut, so
    // a design carrying a priority nobody defined must not decode into something plausible.
    val bs   = golden.clone()
    val prio = dataOffset(bs) + 66 * 3 + 44 // fourth row, priority column
    bs(prio) = '7'.toByte
    failsWith(stream(bs).through(MosMaskReader.slits[IO])):
      case MosMaskProblem.InvalidCell(3L, "priority", "7") => true
      case _                                               => false

  test("an unrecognised slit type is rejected"):
    val bs    = golden.clone()
    val stype = dataOffset(bs) + 45 // first row, slittype column
    bs(stype) = 'Q'.toByte
    failsWith(stream(bs).through(MosMaskReader.slits[IO])):
      case MosMaskProblem.InvalidCell(0L, "slittype", "Q") => true
      case _                                               => false

  // -- Priorities the golden files never exercise -------------------------------------------

  test("priorities 3 and X decode, though no fixture contains them"):
    val bs = golden.clone()
    bs(dataOffset(bs) + 66 * 1 + 44) = '3'.toByte
    bs(dataOffset(bs) + 66 * 2 + 44) = 'X'.toByte
    stream(bs)
      .through(MosMaskReader.slits[IO])
      .compile
      .toList
      .map: ss =>
        assertEquals(ss(1).priority, MosSlitPriority.Low)
        assertEquals(ss(2).priority, MosSlitPriority.Ignore)

  // -- Header keyword failures --------------------------------------------------------------

  test("an unrecognised instrument is rejected"):
    val bs = patch(golden, "INSTRUME= 'GMOS-S  '", "INSTRUME= 'GMOS-Q  '")
    failsWith(stream(bs).through(MosMaskReader.header[IO])):
      case MosMaskProblem.InvalidKeyword("INSTRUME", "GMOS-Q") => true
      case _                                                   => false
