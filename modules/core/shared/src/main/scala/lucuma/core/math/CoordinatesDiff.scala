// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq

/**
 * Result of the calculation of a difference beteween 2 coordinates
 */
final case class CoordinatesDiff(posAngle: Angle, distance: Angle) {

  lazy val offset: Offset = {
    val h = distance.toMicroarcseconds
    Offset(Angle.fromMicroarcseconds((h * posAngle.sin).toLong).p,
           Angle.fromMicroarcseconds((h * posAngle.cos).toLong).q
    )
  }
}

object CoordinatesDiff {
  given Eq[CoordinatesDiff] =
    Eq.by(x => (x.posAngle, x.distance))
}
