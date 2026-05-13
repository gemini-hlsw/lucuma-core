// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.visitors

import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset

/**
 * Generic visitor science area geometry
 *
 * A circular FOV of a supplied science fov diameter.
 */
trait VisitorScienceAreaGeometry:

  def base: ShapeExpression =
    ShapeExpression.point(Offset.Zero)

  def fov(scienceFov: Angle): ShapeExpression =
    ShapeExpression.centeredEllipse(scienceFov, scienceFov)

  def shapeAt(posAngle: Angle, offsetPos: Offset, scienceFov: Angle): ShapeExpression =
    fov(scienceFov).shapeAt(offsetPos, posAngle)

object visitorScienceArea extends VisitorScienceAreaGeometry
