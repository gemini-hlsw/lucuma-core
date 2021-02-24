// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
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
  import ArbCatalogId._

  implicit val arbSiderealTracking: Arbitrary[SiderealTracking] =
    Arbitrary {
      for {
        ci <- arbitrary[Option[CatalogId]]
        cs <- arbitrary[Coordinates]
        ap <- arbitrary[Epoch]
        pv <- arbitrary[Option[ProperMotion]]
        rv <- arbitrary[Option[RadialVelocity]]
        px <- arbitrary[Option[Parallax]]
      } yield SiderealTracking(ci, cs, ap, pv, rv, px)
    }

  implicit val cogSiderealTracking: Cogen[SiderealTracking] =
    Cogen[
      (
        Option[CatalogId],
        Coordinates,
        Epoch,
        Option[ProperMotion],
        Option[RadialVelocity],
        Option[Parallax]
      )
    ].contramap { p =>
        (p.catalogId, p.baseCoordinates, p.epoch, p.properMotion, p.radialVelocity, p.parallax)
      }

}

object ArbSiderealTracking extends ArbSiderealTracking
