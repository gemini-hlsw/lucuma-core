// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gmos

import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

/**
  * GMOS area that could be reachable by the patrol arm
  * https://www.gemini.edu/instrumentation/gmos/capability#Guiding
  */
trait GmosCandidatesArea:

  /**
    * GMOS area where the probe arm can reach centered at 0
    */
  def candidatesArea: ShapeExpression =
    // 4.9 arcmin radius
    // NOTE There is some debate on whether this should be 4.8
    ShapeExpression.centeredEllipse(
      (4.9 * 60 * 2).toInt.arcsec,
      (4.9 * 60 * 2).toInt.arcsec
    )

  /**
    * GMOS area where the probe arm can reach centered with a given posAngle and offset
    */
  def candidatesAreaAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    candidatesArea.shapeAt(posAngle, offsetPos)

  /**
    * GMOS area reachable by the problem arm for a set of posAngles and offsets
    */
  def candidatesAreaAt(posAngles: List[Angle], offsetPositions: List[Offset]): ShapeExpression =
    candidatesArea.intersectionShape(posAngles, offsetPositions)

object candidatesArea extends GmosCandidatesArea
