// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.geom.jts.demo

import gsp.math.{ Angle, Offset }
import gsp.math.geom._
import gsp.math.geom.syntax.all._

import gsp.math.syntax.int._

/**
 * GMOS science area geometry.
 */
object GmosScienceAreaGeometry {

  val imaging: ShapeExpression =
    imagingFov(330340.mas, 33840.mas)

  val mos: ShapeExpression =
    imagingFov(314240.mas, 17750.mas)

  private def offsetInP(p: Angle): Offset =
    Offset(Offset.P(p), Offset.Q.Zero)

  private def imagingFov(size: Angle, notch: Angle): ShapeExpression = {
    val centerCcdWidth: Angle = 165600.mas
    val gapWidth: Angle       =   3000.mas

    // `ccd` is a square with the corners cut such that each missing corner is a
    // right isosceles triangle with the equal sides of length `notch`.
    val z   = Angle.Angle0
    val n   = size - notch
    val ccd = ShapeExpression.centeredRectangle(size, size) ∩
                ShapeExpression.polygonAt((z, n), (n, z), (z, -n), (-n, z))

    // Detector gap at the origin.
    val gap  = ShapeExpression.centeredRectangle(gapWidth, size)

    // Offset of detector gap from the center
    val off  = offsetInP((centerCcdWidth + gapWidth).bisect)

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
      e ∪ ShapeExpression.rectangleAt((x, y), (-x, y - h))
    }
  }

}
