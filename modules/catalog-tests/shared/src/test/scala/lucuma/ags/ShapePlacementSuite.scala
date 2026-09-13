// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import lucuma.core.geom.Shape
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

import AgsParamsVariants.*

/**
 * `posCalculations` evaluates the science area and the patrol field once and places them per
 * position with `Shape.transform`, instead of rebuilding each polygon at every position. That is
 * only sound while both are a fixed shape put somewhere, so this holds the law: for every variant,
 * position angle, offset and pivot, the placed constant must equal the placed expression.
 */
class ShapePlacementSuite extends munit.FunSuite:

  private val pivots: List[Offset] =
    List(Offset.Zero, off(-12.5, 8), off(30, 30))

  private def assertSame(placed: Shape, direct: Shape, clue: String): Unit =
    assertEquals(placed.area, direct.area, s"$clue area")
    assertEquals(placed.boundingOffsets, direct.boundingOffsets, s"$clue bbox")

  variants.foreach: (name, params) =>
    test(s"$name: placed science area equals the evaluated science area expression"):
      val constant = params.scienceAreaShape.eval
      for
        pa     <- posAngles
        offset <- offsets
      do
        assertSame(
          constant.transform(offset, pa, Offset.Zero),
          params.scienceArea(pa, offset).eval,
          s"$name PA ${pa.toDoubleDegrees} offset $offset"
        )

    test(s"$name: placed patrol field equals the evaluated patrol field expression"):
      val constant = params.patrolFieldShape.eval
      for
        pa     <- posAngles
        offset <- offsets
        pivot  <- pivots
      do
        assertSame(
          constant.transform(offset - pivot, pa, pivot),
          params.patrolFieldAt(pa, offset, pivot).eval,
          s"$name PA ${pa.toDoubleDegrees} offset $offset pivot $pivot"
        )

  // The only extended vignetting area is the Visitor's, and `posCalculations` hoists it too.
  test("extended vignetting area places like the science area"):
    val params = AgsParams.Visitor(30.arcsec, 10.arcsec)
    val f      = params.extendedVignettingArea.getOrElse(fail("Visitor has no extended area"))
    val constant = f(Angle.Angle0, Offset.Zero).eval
    for
      pa     <- posAngles
      offset <- offsets
    do
      assertSame(
        constant.transform(offset, pa, Offset.Zero),
        f(pa, offset).eval,
        s"Visitor PA ${pa.toDoubleDegrees} offset $offset"
      )
