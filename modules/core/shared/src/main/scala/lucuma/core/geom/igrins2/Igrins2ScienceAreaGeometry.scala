// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.igrins2

import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

trait Igrins2ScienceAreaGeometry:

  def base: ShapeExpression =
    ShapeExpression.point(Offset.Zero)

  val ScienceFovWidth: Angle  = 300.mas
  val ScienceFovHeight: Angle = 5000.mas
  val SVCDiameter: Angle      = 46.arcsec

  val svcFieldOfView: ShapeExpression =
    ShapeExpression.centeredEllipse(SVCDiameter, SVCDiameter)

  val scienceSlitFOV: ShapeExpression =
    ShapeExpression.centeredRectangle(ScienceFovWidth, ScienceFovHeight)

  // I'm a bit unsure about this, in OCS it is defined this way
  // but i don't think we have a need for this
  val _scienceArea: ShapeExpression =
    svcFieldOfView - scienceSlitFOV

  def svcFieldOfView(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    svcFieldOfView.shapeAt(offsetPos, posAngle)

  def scienceSlitFOV(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    scienceSlitFOV.shapeAt(offsetPos, posAngle)

  def _shapeAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    _scienceArea.shapeAt(offsetPos, posAngle)

object scienceArea extends Igrins2ScienceAreaGeometry
