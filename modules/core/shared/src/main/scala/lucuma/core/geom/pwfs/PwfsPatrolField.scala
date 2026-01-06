// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.pwfs

import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

/**
 * Description of the PWFS patrol field geometry.
 */
trait PwfsPatrolField:
  /**
   * PWFS patrol field radius (417 arcsec, valid for all instruments).
   */
  val PwfsRadius: Angle = 417.arcsec

  /**
   * Boundary margin for guide star checking.
   */
  val Margin: Angle = 2.arcsec

  val PwfsDiameter: Angle = PwfsRadius * 2

  /**
   * PWFS patrol field circle centered at the base position and with 417 arcsec radius.
   */
  val patrolField: ShapeExpression =
    ShapeExpression.centeredEllipse(PwfsDiameter, PwfsDiameter)

  /**
   * Inner patrol field boundary
   */
  val patrolFieldIn: ShapeExpression =
    val innerDiameter = PwfsDiameter - Margin * 2
    ShapeExpression.centeredEllipse(innerDiameter, innerDiameter)

  /**
   * Outer patrol field boundary
   */
  val patrolFieldOut: ShapeExpression =
    val outerDiameter = PwfsDiameter + Margin * 2
    ShapeExpression.centeredEllipse(outerDiameter, outerDiameter)

  /**
   * PWFS patrol field shape, in context.
   *
   * @param posAngle position angle where positive is counterclockwise
   * @param offsetPos offset position from the base, if any
   * @param pivot reference to rotate
   *
   * @return patrol field shape rotated and offset
   */
  def patrolFieldAt(
    posAngle:  Angle,
    offsetPos: Offset,
    pivot:     Offset = Offset.Zero
  ): ShapeExpression =
    patrolField ↗ (offsetPos - pivot) ⟲ posAngle ↗ pivot

  def patrolFieldInAt(
    posAngle:  Angle,
    offsetPos: Offset,
    pivot:     Offset = Offset.Zero
  ): ShapeExpression =
    patrolFieldIn ↗ (offsetPos - pivot) ⟲ posAngle ↗ pivot

  def patrolFieldOutAt(
    posAngle:  Angle,
    offsetPos: Offset,
    pivot:     Offset = Offset.Zero
  ): ShapeExpression =
    patrolFieldOut ↗ (offsetPos - pivot) ⟲ posAngle ↗ pivot

object patrolField extends PwfsPatrolField
