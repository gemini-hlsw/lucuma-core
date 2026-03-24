// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gmos

import cats.syntax.all.*
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.gmos.ifuOffset
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

/**
  * Description of the GMOS OIWFS patrol field geometry.
  */
trait GmosOiwfsPatrolField:
  /**
    * GMOS patrol field shape centered at the base position.
    */
  val patrolField: ShapeExpression =
    ShapeExpression.centeredRectangle(212700.mas, 249600.mas)

  private def patrolFieldAtBase(
    posAngle:  Angle,
    offsetPos: Offset,
    fpuOffset: Offset,
    port:      PortDisposition,
    pivot:     Offset
  ): ShapeExpression =
    val pf = patrolField ↗ (fpuOffset - Offset(94950.mas.p, 89880.mas.q))
    val s  = if (port === PortDisposition.Side) pf.flipQ else pf
    s ↗ (offsetPos - pivot) ⟲ posAngle ↗ pivot

  object imagingMode:
    /**
      * GMOS patrol field shape for imaging mode.
      *
      * @param posAngle position angle where positive is counterclockwise
      * @param offsetPos offset position from the base, if any
      * @param port port disposition
      * @param pivot reference to rotate
      *
      * @return probe field shape rotated and offset
      */
    def patrolFieldAt(
      posAngle:  Angle,
      offsetPos: Offset,
      port:      PortDisposition,
      pivot:     Offset = Offset.Zero
    ): ShapeExpression =
      patrolFieldAtBase(posAngle, offsetPos, Offset.Zero, port, pivot)

  object longSlitMode:
    /**
      * GMOS patrol field shape for long-slit mode.
      *
      * @param posAngle position angle where positive is counterclockwise
      * @param offsetPos offset position from the base, if any
      * @param fpu focal plane unit
      * @param port port disposition
      * @param pivot reference to rotate
      *
      * @return probe field shape rotated and offset
      */
    def patrolFieldAt(
      posAngle:  Angle,
      offsetPos: Offset,
      fpu:       Either[GmosNorthFpu, GmosSouthFpu],
      port:      PortDisposition,
      pivot:     Offset = Offset.Zero
    ): ShapeExpression =
      patrolFieldAtBase(posAngle, offsetPos, ifuOffset(fpu), port, pivot)
