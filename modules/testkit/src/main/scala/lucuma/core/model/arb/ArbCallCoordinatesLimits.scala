// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbCallCoordinatesLimits {
  import ArbSiteCoordinatesLimits.given

  given Arbitrary[CallCoordinatesLimits] =
    Arbitrary {
      for {
        north <- arbitrary[SiteCoordinatesLimits]
        south <- arbitrary[SiteCoordinatesLimits]
      } yield CallCoordinatesLimits(north, south)
    }

  given Cogen[CallCoordinatesLimits] =
    Cogen[(SiteCoordinatesLimits, SiteCoordinatesLimits)].contramap(p => (p.north, p.south))

}

object ArbCallCoordinatesLimits extends ArbCallCoordinatesLimits
