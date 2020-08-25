// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbProperMotion {
  import ArbEpoch._
  import ArbCoordinates._
  import ArbProperVelocity._
  import ArbRadialVelocity._
  import ArbParallax._

  implicit val arbProperMotion: Arbitrary[ProperMotion] =
    Arbitrary {
      for {
        cs <- arbitrary[Coordinates]
        ap <- arbitrary[Epoch]
        pv <- arbitrary[Option[ProperVelocity]]
        rv <- arbitrary[Option[RadialVelocity]]
        px <- arbitrary[Option[Parallax]]
      } yield ProperMotion(cs, ap, pv, rv, px)
    }

  implicit val cogProperMotion: Cogen[ProperMotion] =
    Cogen[(Coordinates, Epoch, Option[ProperVelocity], Option[RadialVelocity], Option[Parallax])]
      .contramap { p =>
        (p.baseCoordinates, p.epoch, p.properVelocity, p.radialVelocity, p.parallax)
      }

}

object ArbProperMotion extends ArbProperMotion
