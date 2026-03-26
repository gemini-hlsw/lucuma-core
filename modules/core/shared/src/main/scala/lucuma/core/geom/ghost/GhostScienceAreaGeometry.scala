// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.ghost

import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset

trait GhostScienceAreaGeometry:
  // base target
  def base: ShapeExpression =
    ShapeExpression.point(Offset.Zero)

  val fov: ShapeExpression =
    ShapeExpression.centeredEllipse(FovDiameter, FovDiameter)

  def fovAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    fov.shapeAt(offsetPos, posAngle)

object scienceArea extends GhostScienceAreaGeometry
