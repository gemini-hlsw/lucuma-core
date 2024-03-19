// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math.Angle
import lucuma.core.math.CoordinatesDiff
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen.*

trait ArbCoordinatesDiff {
  import ArbAngle.given

  implicit val arbCoordinatesDiff: Arbitrary[CoordinatesDiff] =
    Arbitrary {
      for {
        pa <- arbitrary[Angle]
        d  <- arbitrary[Angle]
      } yield CoordinatesDiff(pa, d)
    }

  implicit val cogCoordinatesDiff: Cogen[CoordinatesDiff] =
    Cogen[(Angle, Angle)].contramap(cs => (cs.posAngle, cs.distance))

}

object ArbCoordinatesDiff extends ArbCoordinatesDiff
