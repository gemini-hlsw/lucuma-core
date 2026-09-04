// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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

  /**
   * The FPU offset enters negated here. Only affects IFU. OCS applies it to the patrol field
   * in the shape frame, where `x = -p` (`GmosOiwfsGuideProbe.getCorrectedPatrolField`), but to the
   * probe arm in sky `p` (`GmosOiwfsProbeArm.armAdjustment`). The two must pull in opposite
   * directions in `p` or the field and the arm drift apart: the offset has to cancel for a guide
   * star fixed relative to the field, or stars well inside the patrol field become unreachable.
   */
  private def patrolFieldAtBase(
    posAngle:  Angle,
    offsetPos: Offset,
    fpuOffset: Offset,
    port:      PortDisposition,
    pivot:     Offset
  ): ShapeExpression =
    val pf = patrolField ↗ (Offset.Zero - fpuOffset - Offset(94950.mas.p, 89880.mas.q))
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

  object fpuMode:
    /**
      * GMOS patrol field shape for the modes that carry a focal plane unit: long slit, nod & shuffle
      * and IFU.  Taking an FPU is what brings in `ifuOffset`, which is zero for every slit and
      * non-zero only for the IFU apertures, so this is the only path that shifts the field.
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
