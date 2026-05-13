// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.visitors

import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset

/**
 * MAROON-X science area.
 *
 * https://www.gemini.edu/instrumentation/maroon-x/components
 *
 * Light is fed by a 100 um octagonal fiber at f/3.3, which projects to a 0.77" flat-to-flat fiberShape on sky.
 */
trait MaroonXScienceAreaGeometry:

  def base: ShapeExpression =
    ShapeExpression.point(Offset.Zero)

  // Octagon inscribed in a 0.77" diameter circle
  // circumscribed radius for an octagon is diameter / (2 * cos(pi/8)).
  val fiberShape: ShapeExpression =
    val octagonWidth = MaroonXScienceFov.toMicroarcseconds.toDouble
    val radius     = Angle.fromMicroarcseconds(
      (octagonWidth / (2.0 * Math.cos(Math.PI / 8.0))).round
    )
    ShapeExpression.regularPolygon(radius, 8)

  def shapeAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    // Does the fiber rotate with pos angle?
    fiberShape.shapeAt(offsetPos, posAngle)

object maroonXScienceArea extends MaroonXScienceAreaGeometry
