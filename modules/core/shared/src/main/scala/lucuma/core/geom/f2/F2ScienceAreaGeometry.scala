// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.f2

import algebra.instances.all.given
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
import lucuma.core.model.sequence.f2.F2FpuMask
import spire.math.*

/**
  * F2 science area geometry.
  */
trait F2ScienceAreaGeometry:

  // base target
  def base: ShapeExpression =
    ShapeExpression.point(Offset.Zero)

  def scienceAreaDimensions(lyotWheel: F2LyotWheel, fpu: F2FpuMask): (Angle, Angle) =
    lyotWheel match
      case F2LyotWheel.F16 =>
        val pixelScale = lyotWheel.pixelScale
        val plateScale = lyotWheel.plateScale
        fpu match
          case F2FpuMask.Imaging | F2FpuMask.Builtin(F2Fpu.Pinhole) | F2FpuMask.Builtin(F2Fpu.SubPixPinhole) =>
            val size = ImagingFOVSize ⨱ plateScale
            (size.toAngle, size.toAngle)
          case F2FpuMask.Builtin(fpu) =>
            (Angle.fromBigDecimalArcseconds((fpu.slitWidth * pixelScale).value),
              (LongSlitFOVHeight ⨱ plateScale).toAngle)
          case F2FpuMask.Custom(_, _) =>
            ((MOSFOVWidth ⨱ plateScale).toAngle,
              (LongSlitFOVHeight ⨱ plateScale).toAngle)
      case _                                                          =>
        (Angle.Angle0, Angle.Angle0)

  def shapeAt(
    posAngle:  Angle,
    offsetPos: Offset,
    lyotWheel: F2LyotWheel,
    fpu:       F2FpuMask
  ): ShapeExpression =
    val plateScale = lyotWheel.plateScale
    val scienceAreaWidth = scienceAreaDimensions(lyotWheel, fpu)._1
    val shape =
      fpu.fold(
        imaging(plateScale),
        _ => longslit(plateScale, scienceAreaWidth),
        _ => mosFOV(plateScale)
      )
    shape ↗ offsetPos ⟲ posAngle

  /**
   * Create the F2 imaging field of view.
   * @param plateScale the plate scale in arcsec/mm
   * @return           a shape representing the FOV
   */
  private def imaging(plateScale: PlateScale): ShapeExpression =
    val size = ImagingFOVSize ⨱ plateScale
    ShapeExpression.centeredEllipse(size.toAngle, size.toAngle)

  /**
   * Create the F2 MOS field of view shape.
   * @param plateScale the plate scale in arcsec/mm
   * @return           a shape representing the FOV
   */
  private def mosFOV(plateScale: PlateScale): ShapeExpression =
    val width  = (MOSFOVWidth ⨱ plateScale).toAngle
    val height = (ImagingFOVSize ⨱ plateScale).toAngle

    // The FOV is the intersection of a rectangle and a circle.
    val circle = ShapeExpression.centeredEllipse(height, height)
    val rectangle = ShapeExpression.centeredRectangle(width, height)
    circle ∩ rectangle

  /**
   * Create the F2 long slit field of view shape.
   * @param plateScale       the plate scale in arcsec/mm
   * @param scienceAreaWidth the width of the science area for the F2 configuration
   * @return                 a shape representing the FOV
   */
  private def longslit(
    plateScale: PlateScale,
    scienceAreaWidth: Angle
  ): ShapeExpression =
    val slitHeight = LongSlitFOVHeight ⨱ plateScale
    val y          = (-LongSlitFOVNorthPos / 2) ⨱ plateScale

    ShapeExpression.centeredRectangle(scienceAreaWidth, slitHeight.toAngle) ↗ Offset.Zero.copy(q = y.toAngle.q)

object scienceArea extends F2ScienceAreaGeometry

