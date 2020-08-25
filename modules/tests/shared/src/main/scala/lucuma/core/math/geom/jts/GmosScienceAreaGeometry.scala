// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.geom.jts.demo

import lucuma.core.math.Angle
import lucuma.core.math.geom._
import lucuma.core.math.geom.syntax.all._

import lucuma.core.math.syntax.int._

// TODO: move to core

/**
 * GMOS science area geometry.
 */
object GmosScienceAreaGeometry {

  val imaging: ShapeExpression =
    imagingFov(330340.mas, 33840.mas)

  val mos: ShapeExpression =
    imagingFov(314240.mas, 17750.mas)

  private def imagingFov(size: Angle, corner: Angle): ShapeExpression = {
    val centerCcdWidth: Angle = 165600.mas
    val gapWidth: Angle       =   3000.mas

    val z   = Angle.Angle0

    // Used to make a square rotated by 45 degrees with side length =
    // sqrt(2 * ((size/2) + (size/2 - corner))^2) = sqrt(2*(size - corner)^2)
    // n = distance from center to any vertex of square
    val n   = size - corner

    // `ccd` is a square with the corners cut such that each missing corner is a
    // right isosceles triangle with the equal sides of length `corner`.
    val ccd = ShapeExpression.centeredRectangle(size, size) ∩
                ShapeExpression.polygonAt((z.p, n.q), (n.p, z.q), (z.p, -n.q), (-n.p, z.q))

    // Detector gap at the origin.
    val gap  = ShapeExpression.centeredRectangle(gapWidth, size)

    // Offset of detector gap from the center
    val off  = (centerCcdWidth + gapWidth).bisect.offsetInP

    // There are two gaps so three disjoint CCDs.
    ccd - (gap ↗ off) - (gap ↗ -off)
  }

  def longSlitFov(width: Angle): ShapeExpression = {
    val h = 108000.mas
    val g =   3200.mas

    val x = width.bisect
    val d = h + g

    // Slit in three sections of length `h` separated by gaps `g`.
    (-1 to 1).foldLeft(ShapeExpression.empty) { (e, i) =>
      val y = h.bisect + Angle.fromMicroarcseconds(d.toMicroarcseconds * i)
      e ∪ ShapeExpression.rectangleAt((x.p, y.q), (-x.p, (y - h).q))
    }
  }

}
