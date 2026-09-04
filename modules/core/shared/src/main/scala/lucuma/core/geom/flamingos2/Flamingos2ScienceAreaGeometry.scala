// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.flamingos2

import algebra.instances.all.given
import coulomb.*
import coulomb.conversion.*
import coulomb.units.accepted.*
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.geom.*
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.math.units.*
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask

import scala.language.implicitConversions

/**
  * Flamingos2 science area geometry.
  */
trait Flamingos2ScienceAreaGeometry:

  // base target
  def base: ShapeExpression =
    ShapeExpression.point(Offset.Zero)

  def scienceAreaDimensions(lyotWheel: Flamingos2LyotWheel, fpu: Flamingos2FpuMask): (Angle, Angle) =
    lyotWheel match
      case Flamingos2LyotWheel.F16 =>
        val pixelScale = lyotWheel.pixelScale
        val plateScale = lyotWheel.plateScale
        fpu match
          case Flamingos2FpuMask.Imaging | Flamingos2FpuMask.Builtin(Flamingos2Fpu.Pinhole) | Flamingos2FpuMask.Builtin(Flamingos2Fpu.SubPixPinhole) =>
            val size = ImagingFOVSize ⨱ plateScale
            (size.toAngle, size.toAngle)
          case Flamingos2FpuMask.Builtin(fpu) =>
            (Angle.fromBigDecimalArcseconds((fpu.slitWidth.toValue[BigDecimal] * pixelScale).value),
              (LongSlitFOVHeight ⨱ plateScale).toAngle)
          case Flamingos2FpuMask.Custom(_, _) =>
            (MosFOVWidth, MosFOVHeight)
      case _                                                          =>
        (Angle.Angle0, Angle.Angle0)

  def shapeAt(
    posAngle:  Angle,
    offsetPos: Offset,
    lyotWheel: Flamingos2LyotWheel,
    fpu:       Flamingos2FpuMask
  ): ShapeExpression =
    val plateScale = lyotWheel.plateScale
    val scienceAreaWidth = scienceAreaDimensions(lyotWheel, fpu)._1
    val shape =
      fpu.fold(
        imaging(plateScale),
        _ => longslit(plateScale, scienceAreaWidth),
        _ => mosOutline
      )
    shape.shapeAt(offsetPos, posAngle)

  /**
   * Create the Flamingos2 imaging field of view.
   * @param plateScale the plate scale in arcsec/mm
   * @return           a shape representing the FOV
   */
  private def imaging(plateScale: PlateScale): ShapeExpression =
    val size = ImagingFOVSize ⨱ plateScale
    ShapeExpression.centeredEllipse(size.toAngle, size.toAngle)

  /**
   * Create the Flamingos2 MOS field of view shape.
   * @param plateScale the plate scale in arcsec/mm
   * @return           a shape representing the FOV
   */
  val mos: ShapeExpression =
    val plateScale = Flamingos2LyotWheel.F16.plateScale
    val width      = (MOSFOVWidth ⨱ plateScale).toAngle
    val height     = (ImagingFOVSize ⨱ plateScale).toAngle
    ShapeExpression.centeredEllipse(height, height) ∩ ShapeExpression.centeredRectangle(width, height)

  // GMMPS "slit placement area" for Flamingos2 MOS masks.
  // The vertices come from the GMMPS FoV definition, in arcsec relative to
  // the pointing center, in the pre-image's pixel axes.
  val mosVertices: List[(Int, Int)] =
    List(
      (-174, -60), (-183, -21), (-183,  21), (-174,  60),
      (173,   60), ( 182,  21), ( 182, -21), ( 173, -60)
    )

  private val MosFOVWidth: Angle  = 120.arcsec
  private val MosFOVHeight: Angle = 365.arcsec

  val mosOutline: ShapeExpression =
    ShapeExpression.polygonAt(
      mosVertices.map((x, y) => (y.arcsec.p, x.arcsec.q))*
    )

  /**
   * The Flamingos2 MOS field of view, positioned at a given angle/offset.
   * MOS is only defined at the F16 lyot wheel.
   */
  object mosMode:
    def shapeAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
      mosOutline.shapeAt(offsetPos, posAngle)

  /**
   * Create the Flamingos2 long slit field of view shape.
   * @param plateScale       the plate scale in arcsec/mm
   * @param scienceAreaWidth the width of the science area for the Flamingos2 configuration
   * @return                 a shape representing the FOV
   */
  private def longslit(
    plateScale: PlateScale,
    scienceAreaWidth: Angle
  ): ShapeExpression =
    val slitHeight = LongSlitFOVHeight ⨱ plateScale
    val y          = (-LongSlitFOVNorthPos / 2) ⨱ plateScale

    ShapeExpression.centeredRectangle(scienceAreaWidth, slitHeight.toAngle) ↗ Offset.Zero.copy(q = y.toAngle.q)

object scienceArea extends Flamingos2ScienceAreaGeometry
