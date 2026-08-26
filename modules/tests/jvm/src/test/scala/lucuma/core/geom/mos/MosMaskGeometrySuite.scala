// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.mos

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.readClassLoaderResource
import lucuma.catalog.mos.MosMaskReader
import lucuma.core.geom.jts.JtsShape
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.model.mos.MosMaskHeader
import lucuma.core.model.mos.MosMaskSlit
import munit.CatsEffectSuite
import org.locationtech.jts.geom.Geometry

/**
 * Checks the fitted mask geometry against real designs: `ngc7796_ODF.fits` is GMOS-S and
 * disperses horizontally, `n159_ODF.fits` is Flamingos-2 and disperses vertically.
 */
class MosMaskGeometrySuite extends CatsEffectSuite:

  private def mask(resource: String): IO[(MosMaskHeader, List[MosMaskSlit])] =
    val src = readClassLoaderResource[IO](resource)
    (
      src.through(MosMaskReader.header[IO]).compile.lastOrError,
      src.through(MosMaskReader.slits[IO]).compile.toList
    ).tupled

  private def geometry(resource: String): IO[(MosMaskGeometry, Int)] =
    mask(resource).map { (h, s) =>
      (MosMaskGeometry.fromMask(h, s).getOrElse(fail(s"cannot orient $resource")), s.size)
    }

  private def eval(e: lucuma.core.geom.ShapeExpression): Geometry =
    e.eval match
      case JtsShape(g) => g
      case x           => fail(s"unexpected shape type: $x")

  private def assertAllContained(resource: String): IO[Unit] =
    geometry(resource).map { (g, count) =>
      assertEquals(g.slits.size, count)
      val outline  = eval(g.outline)
      val escapees = g.slits.zipWithIndex.filterNot((shape, _) => outline.contains(eval(shape)))
      assert(
        escapees.isEmpty,
        s"slits outside the placement area at rows: ${escapees.map(_._2).mkString(", ")}"
      )
    }

  test("GMOS-S: every slit lies within the slit placement area"):
    assertAllContained("ngc7796_ODF.fits")

  test("Flamingos-2: every slit lies within the slit placement area"):
    assertAllContained("n159_ODF.fits")

  // The expected rotations come from the WCS of the pre-images these designs were cut from
  // (GMMPS examples/NGC7796_GMOS-S.fits and examples/N159_F2.fits)
  private def assertRotation(resource: String, expectedDeg: Double): IO[Unit] =
    geometry(resource).map { (g, _) =>
      assertEqualsDouble(g.rotation.toSignedDoubleDegrees, expectedDeg, 0.01)
    }

  test("GMOS-S: fitted rotation matches the pre-image WCS"):
    assertRotation("ngc7796_ODF.fits", 19.9394)

  test("Flamingos-2: fitted rotation matches the pre-image WCS"):
    assertRotation("n159_ODF.fits", -165.4854)
