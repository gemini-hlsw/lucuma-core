// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.math._
import lucuma.core.math.arb._
import lucuma.core.model._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbSiderealTracking {
  import ArbEpoch._
  import ArbCoordinates._
  import ArbProperMotion._
  import ArbRadialVelocity._
  import ArbParallax._

  implicit val arbSiderealTracking: Arbitrary[SiderealTracking] =
    Arbitrary {
      for {
        cs <- arbitrary[Coordinates]
        ap <- arbitrary[Epoch]
        pv <- arbitrary[Option[ProperMotion]]
        rv <- arbitrary[Option[RadialVelocity]]
        px <- arbitrary[Option[Parallax]]
      } yield SiderealTracking(cs, ap, pv, rv, px)
    }

  implicit val cogSiderealTracking: Cogen[SiderealTracking] =
    Cogen[
      (
        Coordinates,
        Epoch,
        Option[ProperMotion],
        Option[RadialVelocity],
        Option[Parallax]
      )
    ].contramap { p =>
      (p.baseCoordinates, p.epoch, p.properMotion, p.radialVelocity, p.parallax)
    }

}

object ArbSiderealTracking extends ArbSiderealTracking
