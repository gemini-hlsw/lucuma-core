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

  test("a design with fewer than two slits cannot be oriented"):
    val (h, s) = mask("/ngc7796_ODF.fits")
    assertEquals(MosMaskGeometry.fromMask(h, s.take(1)), None)
    assertEquals(MosMaskGeometry.fromMask(h, Nil), None)
