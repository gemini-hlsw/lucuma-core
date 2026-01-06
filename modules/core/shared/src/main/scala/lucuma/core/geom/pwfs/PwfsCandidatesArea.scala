// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.pwfs

import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset

/**
 * PWFS area where guide star candidates can be found.
 * For PWFS, this is the same as the patrol field.
 */
trait PwfsCandidatesArea extends PwfsPatrolField:

  /**
   * PWFS candidates area centered at origin.
   */
  def candidatesArea: ShapeExpression = patrolField

  /**
   * PWFS candidates area with a given posAngle and offset.
   */
  def candidatesAreaAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    candidatesArea.positionAt(posAngle, offsetPos)

  /**
   * PWFS candidates area reachable for a set of posAngles and offsets.
   */
  def candidatesAreaAt(posAngles: List[Angle], offsetPositions: List[Offset]): ShapeExpression =
    (for {
      a <- posAngles
      o <- offsetPositions
    } yield candidatesAreaAt(a, o)).fold(ShapeExpression.Empty)(_ ∩ _)

object candidatesArea extends PwfsCandidatesArea
