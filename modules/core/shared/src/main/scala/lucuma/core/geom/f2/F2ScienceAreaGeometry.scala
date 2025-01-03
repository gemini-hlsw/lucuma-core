// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.f2

import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.accepted.*
import coulomb.units.si.prefixes.*
import lucuma.core.enums.F2Fpu
import lucuma.core.enums.F2LyotWheel
import lucuma.core.geom.*
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.units.*
import spire.math.*
import spire.std.bigDecimal.*

/**
  * F2 science area geometry.
  */
trait F2ScienceAreaGeometry {

  // base target
  def base: ShapeExpression =
    ShapeExpression.point(Offset.Zero)

  def scienceAreaDimensions(lyotWheel: F2LyotWheel, fpu: Option[F2Fpu]):
    (Angle, Angle) =
    lyotWheel match {
      case F2LyotWheel.F16 | F2LyotWheel.F32High | F2LyotWheel.F32Low =>
        val pixelScale = lyotWheel.pixelScale
        val plateScale = BigDecimal(lyotWheel.plateScale).withUnit[ArcSecondPerMillimeter]
        fpu match {
          case None | Some(F2Fpu.Pinhole) | Some(F2Fpu.SubPixPinhole) =>
            val size = ImagingFOVSize.withPlateScale(plateScale)
            (size.toAngle, size.toAngle)
          case Some(fpu) =>
            (Angle.fromBigDecimalArcseconds(fpu.slitWidth * pixelScale),
             LongSlitFOVHeight.withPlateScale(plateScale).toAngle)
        }
      case _ => (Angle.Angle0, Angle.Angle0)
    }

  def shapeAt(
    posAngle:  Angle,
    offsetPos: Offset,
    lyotWheel: F2LyotWheel,
    fpu:       Option[F2Fpu]
  ): ShapeExpression =
    val plateScale = BigDecimal(lyotWheel.plateScale).withUnit[ArcSecondPerMillimeter]
    val scienceAreaWidth = scienceAreaDimensions(lyotWheel, fpu)._1
    val shape = fpu match {
      case None => imaging(plateScale)
      // case _    => mosFOV(plateScale) // TODO: handle MOS
      case Some(fpu)    => longslit(plateScale, scienceAreaWidth)
    }
    shape ↗ offsetPos ⟲ posAngle

  /**
   * Create the F2 imaging field of view.
   * @param plateScale the plate scale in arcsec/mm
   * @return           a shape representing the FOV
   */
  private def imaging(plateScale: F2PlateScale): ShapeExpression = {
    val size: Quantity[BigDecimal, ArcSecond]   = ImagingFOVSize.toUnit[Millimeter] * plateScale
    val radius = size / 2.0
    ShapeExpression.centeredEllipse(-radius.toAngle, size.toAngle)
  }

  /**
   * Create the F2 MOS field of view shape.
   * @param plateScale the plate scale in arcsec/mm
   * @return           a shape representing the FOV
   */
  def mos(plateScale: F2PlateScale): ShapeExpression = {
    val width  = MOSFOVWidth * plateScale
    val height = ImagingFOVSize * plateScale
    val radius = height / 2.0

    // The FOV is the intersection of a rectangle and a circle.
    val circle = ShapeExpression.centeredEllipse(-radius.toAngle, height.toAngle)
    val rectangle = ShapeExpression.rectangleAt((-width.toAngle.bisect.p, -radius.toAngle.q), (width.toAngle.p, height.toAngle.q))
    circle ∩ rectangle
  }

  /**
   * Create the F2 long slit field of view shape.
   * @param plateScale       the plate scale in arcsec/mm
   * @param scienceAreaWidth the width of the science area for the F2 configuration
   * @return                 a shape representing the FOV
   */
  private def longslit(
    plateScale: F2PlateScale,
    scienceAreaWidth: Angle
  ): ShapeExpression = {
    val slitHeight = LongSlitFOVHeight.withPlateScale(plateScale)
    val y          = -LongSlitFOVNorthPos.withPlateScale(plateScale)

    ShapeExpression.centeredRectangle(scienceAreaWidth, slitHeight.toAngle) ↗ Offset.Zero.copy(q = y.toAngle.q)
  }

}

object scienceArea extends F2ScienceAreaGeometry

