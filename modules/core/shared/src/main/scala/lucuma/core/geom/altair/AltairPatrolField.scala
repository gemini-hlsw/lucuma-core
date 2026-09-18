// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.altair

import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

/**
 * Altair AOWFS patrol field geometry, from the OCS `AltairAowfsGuider`: an oval made of two half
 * ellipses sharing the p axis, slightly taller towards positive q.
 */
trait AltairPatrolField:

  val PLimit: Angle      = 25.arcsec
  val QPlusLimit: Angle  = 27.arcsec
  val QMinusLimit: Angle = 23.arcsec

  /** Half plane q >= 0 (upper) or q <= 0 (lower), wide enough to cover the ellipses. */
  private def halfPlane(height: Angle, upper: Boolean): ShapeExpression =
    val shift: Angle = if upper then height else -height
    ShapeExpression.centeredRectangle(PLimit * 4, height * 2) ↗
      Offset(Offset.P.Zero, Offset.Q(shift))

  /** Patrol field centered at the base position. */
  val patrolField: ShapeExpression =
    val upper: ShapeExpression =
      ShapeExpression.centeredEllipse(PLimit * 2, QPlusLimit * 2) ∩ halfPlane(QPlusLimit, true)
    val lower: ShapeExpression =
      ShapeExpression.centeredEllipse(PLimit * 2, QMinusLimit * 2) ∩ halfPlane(QMinusLimit, false)
    upper ∪ lower

  /** Circle covering the patrol field at every position angle, for catalog searches. */
  val candidatesArea: ShapeExpression =
    ShapeExpression.centeredEllipse(QPlusLimit * 2, QPlusLimit * 2)

  /**
   * Patrol field shape, in context.
   *
   * @param posAngle position angle where positive is counterclockwise
   * @param offsetPos offset position from the base, if any
   * @param pivot reference to rotate
   */
  def patrolFieldAt(
    posAngle:  Angle,
    offsetPos: Offset,
    pivot:     Offset = Offset.Zero
  ): ShapeExpression =
    patrolField.shapePivotAt(offsetPos, posAngle, pivot)

object patrolField extends AltairPatrolField
