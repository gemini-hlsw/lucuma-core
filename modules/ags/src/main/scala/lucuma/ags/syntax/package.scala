// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags.syntax

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.syntax.all.*
import lucuma.ags.*
import lucuma.core.enums.Site
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.skycalc.averageParallacticAngle
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Tracking
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.util.TimeSpan

import java.time.Duration
import java.time.Instant
import scala.collection.immutable.SortedSet

extension (posAngleConstraint: PosAngleConstraint)
  def anglesToTestAt(
    site:     Site,
    tracking: Tracking,
    vizTime:  Instant,
    duration: Duration
  ): Option[NonEmptyList[Angle]] =
    anglesToTestAt(
      TimeSpan
        .fromDuration(duration)
        .filter(_ > TimeSpan.Zero)
        .flatMap: ts =>
          averageParallacticAngle(site.place, tracking, vizTime, ts)
    )

  def anglesToTestAt(
    averageParallacticAngle: => Option[Angle]
  ): Option[NonEmptyList[Angle]] =
    posAngleConstraint match
      case PosAngleConstraint.Fixed(a)               => NonEmptyList.of(a).some
      case PosAngleConstraint.AllowFlip(a)           => NonEmptyList.of(a, a.flip).some
      case PosAngleConstraint.ParallacticOverride(a) => NonEmptyList.of(a).some
      case PosAngleConstraint.AverageParallactic     =>
        averageParallacticAngle.map(a => NonEmptyList.of(a, a.flip))
      case PosAngleConstraint.Unbounded              => UnconstrainedAngles

extension (o: Offset) def guided: GuidedOffset = GuidedOffset(o)

extension (c: NonEmptyList[TelescopeConfig])
  def guidedOffsets: Option[NonEmptySet[GuidedOffset]] =
    NonEmptySet.fromSet:
      SortedSet.from:
        c.collect:
          case TelescopeConfig(o, StepGuideState.Enabled) => GuidedOffset(o)

  def asAcqOffsets: Option[AcquisitionOffsets] = guidedOffsets.map(AcquisitionOffsets.apply)

  def asSciOffsets: Option[ScienceOffsets] = guidedOffsets.map(ScienceOffsets.apply)
