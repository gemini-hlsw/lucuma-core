// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Order.given
import cats.data.NonEmptySet
import cats.syntax.all.*
import lucuma.core.enums.SequenceType
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig

import scala.collection.immutable.SortedSet

opaque type OffsetPositions = NonEmptySet[OffsetPosition]

object OffsetPositions:
  private def telescopeConfigsToOffsetPositions(
    posAngle:       Angle,
    sequenceType:   SequenceType,
    blindOffsetOff: Option[Offset],
    configs:        Option[NonEmptySet[TelescopeConfig]]
  ): SortedSet[OffsetPosition] =
    configs
      .map(_.toSortedSet)
      .orEmpty
      .map: tc =>
        OffsetPosition.forTelescopeConfigAndSequenceType(
          tc,
          sequenceType,
          posAngle,
          blindOffsetOff
        )

  def fromTelescopeConfigs(
    baseCoordinates:    Option[Coordinates],
    blindOffset:        Option[Coordinates],
    posAngle:           Angle,
    acquisitionConfigs: Option[NonEmptySet[TelescopeConfig]],
    scienceConfigs:     Option[NonEmptySet[TelescopeConfig]]
  ): OffsetPositions = {
    val blindOffsetOff: Option[Offset] =
      (baseCoordinates, blindOffset).mapN((base, blind) => base.diff(blind).offset)

    val basePosition: OffsetPosition = OffsetPosition.base(posAngle)

    val allOffsets: SortedSet[OffsetPosition] =
      SortedSet(basePosition) ++
        SortedSet.from(blindOffsetOff.map(OffsetPosition.forBlindOffsetOff(_, posAngle))) ++
        telescopeConfigsToOffsetPositions(
          posAngle,
          SequenceType.Acquisition,
          blindOffsetOff,
          acquisitionConfigs
        ) ++
        telescopeConfigsToOffsetPositions(
          posAngle,
          SequenceType.Science,
          blindOffsetOff,
          scienceConfigs
        )

    NonEmptySet
      .fromSet(allOffsets)
      .getOrElse: // In this case, return base offset
        NonEmptySet.of(basePosition)
  }

  def fromTelescopeConfigs(
    baseCoordinates:    Option[Coordinates],
    blindOffset:        Option[Coordinates],
    posAngles:          NonEmptySet[Angle],
    acquisitionConfigs: Option[NonEmptySet[TelescopeConfig]],
    scienceConfigs:     Option[NonEmptySet[TelescopeConfig]]
  ): OffsetPositions =
    posAngles
      .map: angle =>
        fromTelescopeConfigs(
          baseCoordinates,
          blindOffset,
          angle,
          acquisitionConfigs,
          scienceConfigs
        )
      .reduce

  extension (ops: OffsetPositions) def value: NonEmptySet[OffsetPosition] = ops
