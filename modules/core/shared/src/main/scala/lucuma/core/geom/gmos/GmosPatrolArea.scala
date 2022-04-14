// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gmos

import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all._
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int._

/**
  * GMOS area that could be reachable by the patrol arm
  * https://www.gemini.edu/instrumentation/gmos/capability#Guiding
  */
trait GmosPatrolArea {

  /**
    * GMOS area where the probe arm can reach centered at 0
    */
  def patrolArea: ShapeExpression =
    // 4.9 arcmin radius
    // NOTE There is some debate on whether this should be 4.8
    ShapeExpression.centeredEllipse((4.9 * 60 * 2).toInt.arcsec,
                                    (4.9 * 60 * 2).toInt.arcsec
    )

  /**
    * GMOS area where the probe arm can reach centered with a given posAngle and offset
    */
  def patrolAreaAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    patrolArea ↗ offsetPos ⟲ posAngle

  /**
    * GMOS area reachable by the problem arm for a set of posAngles and offsets
    */
  def patrolAreaAt(posAngles: List[Angle], offsetPositions: List[Offset]): ShapeExpression =
    (for {
      a <- posAngles
      o <- offsetPositions
    } yield patrolAreaAt(a, o)).fold(ShapeExpression.Empty)(_ ∪ _)
}

object patrolArea extends GmosPatrolArea

