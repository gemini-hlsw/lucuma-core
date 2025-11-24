// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gmos

import cats.syntax.all.*
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

/**
  * Description of the GMOS OIWFS probe arm geometry.
  */
trait GmosPatrolField:
  /**
    * GMOS patrol field shape centered at the base position.
    */
  val patrolField: ShapeExpression =
    ShapeExpression.centeredRectangle(212700.mas, 249600.mas)

  /**
    * GMOS patrol field shape, in context.
    *
    * @param posAngle position angle where positive is counterclockwise
    * @param offsetPos offset position from the base, if any
    * @param pivot reference to rotate
    * @param fpu focal plane unit, if any
    * @param port port disposition
    *
    * @return probe field shape rotated and offset
    */
  def patrolFieldAt(
    posAngle:  Angle,
    offsetPos: Offset,
    fpu:       Option[Either[GmosNorthFpu, GmosSouthFpu]],
    port:      PortDisposition,
    pivot:    Offset = Offset.Zero
  ): ShapeExpression =
    val pf = patrolField ↗ (ifuOffset(fpu) - Offset(94950.mas.p, 89880.mas.q))
    val s  = if (port === PortDisposition.Side) pf.flipQ else pf
    s ↗ (offsetPos - pivot) ⟲ posAngle ↗ pivot

object patrolField extends GmosPatrolField
