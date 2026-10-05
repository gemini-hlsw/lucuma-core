// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.scorpio

import lucuma.core.enums.ScorpioFpu
import lucuma.core.geom.*
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset

/**
 * SCORPIO science area geometry. VIS and IR share the same slits.
 */
trait ScorpioScienceAreaGeometry:

  def base: ShapeExpression =
    ShapeExpression.point(Offset.Zero)

  def pointAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    base.shapeAt(offsetPos, posAngle)

  val imagingFov: ShapeExpression =
    ShapeExpression.centeredRectangle(ImagingFieldSize, ImagingFieldSize)

  object imagingMode:
    def shapeAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
      imagingFov.shapeAt(offsetPos, posAngle)

  object longSlitMode:
    def shapeAt(posAngle: Angle, offsetPos: Offset, fpu: ScorpioFpu): ShapeExpression =
      longSlitFov(fpu.slitWidth).shapeAt(offsetPos, posAngle)

  def longSlitFov(width: Angle): ShapeExpression =
    ShapeExpression.centeredRectangle(width, LongSlitHeight)

object scienceArea extends ScorpioScienceAreaGeometry
