// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.syntax

import cats.data.NonEmptyList
import cats.syntax.all.given
import lucuma.core.math.Coordinates
import lucuma.core.model.SiderealTracking

import java.time.Instant
import scala.annotation.targetName

trait ToTrackingOps {
  extension(trackings: NonEmptyList[SiderealTracking])
    // We calculate the coordinates at a given time by doing PM
    // correction of each tracking and then finding the center
    @targetName("TrackingList_centerOfAt")
    @deprecated
    def centerOfAt(i: Instant): Option[Coordinates] =
      trackings
        .map(_(i))
        .sequence
        .map(nel => Coordinates.centerOf(nel))

    @targetName("TrackingList_centerOf")
    @deprecated
    def centerOf: Coordinates =
      val coords = trackings.map(_.baseCoordinates)
      Coordinates.centerOf(coords)
    
}

object tracking extends ToTrackingOps
