// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.math.*
import lucuma.core.math.arb.*
import lucuma.core.model.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen.*
import org.scalacheck.*

trait ArbSiderealTracking {
  import ArbEpoch.given
  import ArbCoordinates.given
  import ArbProperMotion.given
  import ArbRadialVelocity.given
  import ArbParallax.given

  given Arbitrary[SiderealTracking] =
    Arbitrary {
      for {
        cs <- arbitrary[Coordinates]
        ap <- arbitrary[Epoch]
        pv <- arbitrary[Option[ProperMotion]]
        rv <- arbitrary[Option[RadialVelocity]]
        px <- arbitrary[Option[Parallax]]
      } yield SiderealTracking(cs, ap, pv, rv, px)
    }

  given Cogen[SiderealTracking] =
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
