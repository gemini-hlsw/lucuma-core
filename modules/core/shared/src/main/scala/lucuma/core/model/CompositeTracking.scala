// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.math.Coordinates
import java.time.Instant
import cats.data.NonEmptyList

case class CompositeTracking(toNonEmptyList: NonEmptyList[Tracking]) extends Tracking:
  def apply(i: Instant): Option[Coordinates] =
    toNonEmptyList
      .traverse(_.apply(i))
      .map(Coordinates.centerOf)
