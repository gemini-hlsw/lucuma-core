// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.offsets

import cats.Order
import cats.derived.*
import cats.syntax.eq.*
import cats.syntax.option.*
import lucuma.core.enums.SequenceType
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig


private given Order[Angle] = Angle.SignedAngleOrder

/**
 * An offset of a specific geometry type with parameters to rotate around a pivot. This is useful
 * both for AGS and for visualization of offsets.
 */
final case class OffsetPosition(
  geometryType: GeometryType,
  offsetPos:    Offset,
  posAngle:     Angle,
  pivot:        Offset = Offset.Zero
) derives Order:
  lazy val rotatedOffset: RotatedOffset = RotatedOffset:
    (offsetPos - pivot).rotate(posAngle) + pivot

  def onlyIfGuided: Option[OffsetPosition] =
    this.some.filter(_.geometryType.isGuided)

object OffsetPosition:
  def forTelescopeConfigAndSequenceType(
    telescopeConfig:   TelescopeConfig,
    sequenceType:      SequenceType,
    posAngle:          Angle,
    blindOffsetOffset: Option[Offset]
  ): OffsetPosition =
    val pivot: Offset =
      blindOffsetOffset.filter(_ => sequenceType === SequenceType.Acquisition).orEmpty

    OffsetPosition(
      GeometryType.fromSequenceTypeAndGuiding(sequenceType, telescopeConfig.guiding),
      telescopeConfig.offset + pivot,
      posAngle,
      pivot
    )

  def forBlindOffsetOff(blindOffsetOff: Offset, posAngle: Angle): OffsetPosition =
    OffsetPosition(GeometryType.BlindOffset, blindOffsetOff, posAngle, blindOffsetOff)

  def base(posAngle: Angle): OffsetPosition =
    OffsetPosition(GeometryType.Base, Offset.Zero, posAngle, Offset.Zero)
