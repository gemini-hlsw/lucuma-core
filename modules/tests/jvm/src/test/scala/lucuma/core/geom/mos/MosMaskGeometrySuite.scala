// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.mos

import fs2.Chunk
import fs2.Fallible
import fs2.Stream
import lucuma.catalog.mos.MosMaskReader
import lucuma.core.geom.jts.JtsShape
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.model.mos.MosMaskHeader
import lucuma.core.model.mos.MosMaskSlit
import munit.FunSuite
import org.locationtech.jts.geom.Geometry

/**
 * Checks the fitted mask geometry against real designs: `ngc7796_ODF.fits` is GMOS-S and
 * disperses horizontally, `n159_ODF.fits` is Flamingos-2 and disperses vertically.
 */
class MosMaskGeometrySuite extends FunSuite:

  private def mask(resource: String): (MosMaskHeader, List[MosMaskSlit]) =
    val bytes =
      val in = getClass.getResourceAsStream(resource)
      try in.readAllBytes finally in.close()
    val src   = Stream.chunk(Chunk.array(bytes))
    (for
      h <- src.through(MosMaskReader.header[Fallible]).compile.lastOrError
      s <- src.through(MosMaskReader.slits[Fallible]).compile.toList
    yield (h, s)).fold(throw _, identity)

  private def geometry(resource: String): (MosMaskGeometry, Int) =
    val (h, s) = mask(resource)
    (MosMaskGeometry.fromMask(h, s).getOrElse(fail(s"cannot orient $resource")), s.size)

  private def eval(e: lucuma.core.geom.ShapeExpression): Geometry =
    e.eval match
      case JtsShape(g) => g
      case x           => fail(s"unexpected shape type: $x")

  private def assertAllContained(resource: String): Unit =
    val (g, count) = geometry(resource)
    assertEquals(g.slits.size, count)
    val outline    = eval(g.outline)
    val escapees   = g.slits.zipWithIndex.filterNot((shape, _) => outline.contains(eval(shape)))
    assert(
      escapees.isEmpty,
      s"slits outside the placement area at rows: ${escapees.map(_._2).mkString(", ")}"
    )

  test("GMOS-S: every slit lies within the slit placement area"):
    assertAllContained("/ngc7796_ODF.fits")

  test("Flamingos-2: every slit lies within the slit placement area"):
    assertAllContained("/n159_ODF.fits")

  // The expected rotations come from the WCS of the pre-images these designs were cut from
  // (GMMPS examples/NGC7796_GMOS-S.fits and examples/N159_F2.fits): the mean rotation of the
  // CD matrix columns, in this library's parity convention. GMMPS's own get_OT_posangle tool
  // reads the equivalent OT position angles, 160.08° and 104.52°, off the same matrices.
  private def assertRotation(resource: String, expectedDeg: Double): Unit =
    val (g, _) = geometry(resource)
    assertEqualsDouble(g.rotation.toSignedDoubleDegrees, expectedDeg, 0.01)

  test("GMOS-S: fitted rotation matches the pre-image WCS"):
    assertRotation("/ngc7796_ODF.fits", 19.9394)

  test("Flamingos-2: fitted rotation matches the pre-image WCS"):
    assertRotation("/n159_ODF.fits", -165.4854)

  // Slit sky coordinates in the ODF were computed by GMMPS with wcstools from the pre-image
  // WCS; each aperture centroid must land back on them. JTS x is -p (east-left convention).
  private def assertCentroids(resource: String): Unit =
    val (h, s) = mask(resource)
    val g      = MosMaskGeometry.fromMask(h, s).getOrElse(fail(s"cannot orient $resource"))
    val worst  = s.zip(g.slits).map { (slit, shape) =>
      val c = eval(shape).getCentroid
      val o = h.pointing.diff(slit.coordinates).offset
      val p = o.p.toAngle.toSignedDoubleDecimalArcseconds
      val q = o.q.toAngle.toSignedDoubleDecimalArcseconds
      math.hypot(-c.getX / 1e6 - p, c.getY / 1e6 - q)
    }.max
    // includes the intentional along/across-slit offsets, hence the loose bound
    assert(worst < 0.3, f"max centroid residual $worst%.4f arcsec")

  test("GMOS-S: apertures land on the catalog sky positions"):
    assertCentroids("/ngc7796_ODF.fits")

  test("Flamingos-2: apertures land on the catalog sky positions"):
    assertCentroids("/n159_ODF.fits")

  test("a design with fewer than two slits cannot be oriented"):
    val (h, s) = mask("/ngc7796_ODF.fits")
    assertEquals(MosMaskGeometry.fromMask(h, s.take(1)), None)
    assertEquals(MosMaskGeometry.fromMask(h, Nil), None)
