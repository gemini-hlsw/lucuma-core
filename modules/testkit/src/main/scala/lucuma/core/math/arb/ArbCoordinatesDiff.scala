// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math.Angle
import lucuma.core.math.CoordinatesDiff
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbCoordinatesDiff {
  import ArbAngle._

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
