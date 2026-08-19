// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.igrins2

import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.ApertureExtent
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

trait Igrins2ScienceAreaGeometry:

  def base: ShapeExpression =
    ShapeExpression.point(Offset.Zero)

  val ScienceFovWidth: Angle  = 300.mas
  val ScienceFovHeight: Angle = 5000.mas
  val SVCDiameter: Angle      = 46.arcsec

  /** Focal-plane extent of the science slit. IGRINS-2 has a single fixed aperture. */
  val ScienceFovExtent: ApertureExtent =
    ApertureExtent(ScienceFovWidth, ScienceFovHeight)

  val svcFieldOfView: ShapeExpression =
    ShapeExpression.centeredEllipse(SVCDiameter, SVCDiameter)

  val scienceSlitFOV: ShapeExpression =
    ShapeExpression.centeredRectangle(ScienceFovExtent)

  def svcFieldOfView(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    svcFieldOfView.shapeAt(offsetPos, posAngle)

  def scienceSlitFOV(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    scienceSlitFOV.shapeAt(offsetPos, posAngle)

object scienceArea extends Igrins2ScienceAreaGeometry
