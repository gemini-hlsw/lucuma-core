// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gmos

import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.geom.*
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

/**
  * GMOS science area geometry.
  */
trait GmosScienceAreaGeometry {

  // base target
  def base: ShapeExpression =
    ShapeExpression.point(Offset.Zero)

  def pointAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    base.shapeAt(offsetPos, posAngle)

  val imaging: ShapeExpression =
    imagingFov(330340.mas, 33840.mas)

  val mos: ShapeExpression =
    imagingFov(314240.mas, 17750.mas)

  object imagingMode:
    def shapeAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
      imaging.shapeAt(offsetPos, posAngle)

  object longSlitMode:
    def shapeAt(
      posAngle:  Angle,
      offsetPos: Offset,
      fpu:       Either[GmosNorthFpu, GmosSouthFpu]
    ): ShapeExpression =
      shapeFromLongSlitFpu(fpu).shapeAt(offsetPos, posAngle)

  private def shapeFromLongSlitFpu(fpu: Either[GmosNorthFpu, GmosSouthFpu]): ShapeExpression =
    fpu.fold(
      {
        case GmosNorthFpu.Ns0 | GmosNorthFpu.Ns1 | GmosNorthFpu.Ns2 | GmosNorthFpu.Ns3 |
             GmosNorthFpu.Ns4 | GmosNorthFpu.Ns5 | GmosNorthFpu.Ifu2Slits | GmosNorthFpu.IfuBlue |
             GmosNorthFpu.IfuRed =>
          ShapeExpression.empty

        case n@(GmosNorthFpu.LongSlit_0_25 | GmosNorthFpu.LongSlit_0_50 |
                GmosNorthFpu.LongSlit_0_75 | GmosNorthFpu.LongSlit_1_00 |
                GmosNorthFpu.LongSlit_1_50 | GmosNorthFpu.LongSlit_2_00 |
                GmosNorthFpu.LongSlit_5_00) =>
          longSlitFov(n.effectiveSlitWidth)
      },
      {
        case GmosSouthFpu.Ns1 | GmosSouthFpu.Ns2 | GmosSouthFpu.Ns3 |
             GmosSouthFpu.Ns4 | GmosSouthFpu.Ns5 | GmosSouthFpu.Ifu2Slits | GmosSouthFpu.IfuBlue |
             GmosSouthFpu.IfuRed | GmosSouthFpu.IfuNS2Slits | GmosSouthFpu.IfuNSBlue |
             GmosSouthFpu.IfuNSRed =>
          ShapeExpression.empty

        case s@(GmosSouthFpu.LongSlit_0_25 | GmosSouthFpu.LongSlit_0_50 |
                GmosSouthFpu.LongSlit_0_75 | GmosSouthFpu.LongSlit_1_00 |
                GmosSouthFpu.LongSlit_1_50 | GmosSouthFpu.LongSlit_2_00 |
                GmosSouthFpu.LongSlit_5_00) =>
          longSlitFov(s.effectiveSlitWidth)
      }
    )

  private def imagingFov(size: Angle, corner: Angle): ShapeExpression = {
    val centerCcdWidth: Angle = 165600.mas
    val gapWidth: Angle       = 3000.mas

    val z = Angle.Angle0

    // Used to make a square rotated by 45 degrees with side length =
    // sqrt(2 * ((size/2) + (size/2 - corner))^2) = sqrt(2*(size - corner)^2)
    // n = distance from center to any vertex of square
    val n = size - corner

    // `ccd` is a square with the corners cut such that each missing corner is a
    // right isosceles triangle with the equal sides of length `corner`.
    val ccd = ShapeExpression.centeredRectangle(size, size) ∩
      ShapeExpression.polygonAt((z.p, n.q), (n.p, z.q), (z.p, -n.q), (-n.p, z.q))

    // Detector gap at the origin.
    val gap = ShapeExpression.centeredRectangle(gapWidth, size)

    // Offset of detector gap from the center
    val off = (centerCcdWidth + gapWidth).bisect.offsetInP

    // There are two gaps so three disjoint CCDs.
    ccd - (gap ↗ off) - (gap ↗ -off)
  }

  def longSlitFov(width: Angle): ShapeExpression = {
    val h = 108000.mas
    val g = 3200.mas

    val x = width.bisect
    val d = h + g

    // Slit in three sections of length `h` separated by gaps `g`.
    (-1 to 1).foldLeft(ShapeExpression.empty) { (e, i) =>
      val y = h.bisect + Angle.fromMicroarcseconds(d.toMicroarcseconds * i)
      e ∪ ShapeExpression.rectangleAt((x.p, y.q), (-x.p, (y - h).q))
    }
  }

}

object scienceArea extends GmosScienceAreaGeometry
