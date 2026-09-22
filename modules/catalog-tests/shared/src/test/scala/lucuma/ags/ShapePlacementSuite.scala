// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import lucuma.core.geom.Shape
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

import AgsParamsVariants.*

/**
 * `posCalculations` evaluates the science area and the patrol field once and places them per
 * position with `Shape.transform`, instead of rebuilding each polygon at every position.
 */
class ShapePlacementSuite extends munit.FunSuite:

  private val pivots: List[Offset] =
    List(Offset.Zero, off(-12.5, 8), off(30, 30))

  // Equal areas plus a full-area overlap means the interiors coincide, not just their summaries.
  // The overlay itself carries noding noise of a few parts in 1e17, hence the tolerance.
  private def assertSame(placed: Shape, direct: Shape): Unit =
    assertEquals(placed.area, direct.area)
    val full    = direct.area.toMicroarcsecondsSquared
    val overlap = placed.intersection(direct).area.toMicroarcsecondsSquared
    assert(math.abs(full - overlap) <= math.max(1L, full / 1_000_000_000L), s"$overlap vs $full")

  variants.foreach: (name, params) =>
    test(s"$name: placed science area equals the evaluated science area expression"):
      val constant = params.scienceAreaShape.eval
      for
        pa     <- posAngles
        offset <- offsets
      do
        assertSame(
          constant.transform(offset, pa, Offset.Zero),
          params.scienceArea(pa, offset).eval
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
          params.patrolFieldAt(pa, offset, pivot).eval
        )

  // The only extended vignetting area is the Visitor's, and `posCalculations` hoists it too.
  test("extended vignetting area places like the science area"):
    val params   = AgsParams.Visitor(30.arcsec, 10.arcsec)
    val f        = params.extendedVignettingArea.getOrElse(fail("Visitor has no extended area"))
    val constant = f(Angle.Angle0, Offset.Zero).eval
    for
      pa     <- posAngles
      offset <- offsets
    do
      assertSame(
        constant.transform(offset, pa, Offset.Zero),
        f(pa, offset).eval
      )
