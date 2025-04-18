// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.arb.*
import lucuma.core.model.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbSiteCoordinatesLimits {
  import ArbRightAscension.given
  import ArbDeclination.given

  given Arbitrary[SiteCoordinatesLimits] =
    Arbitrary {
      for {
        raStart  <- arbitrary[RightAscension]
        raEnd    <- arbitrary[RightAscension]
        decStart <- arbitrary[Declination]
        decEnd   <- arbitrary[Declination]
      } yield SiteCoordinatesLimits(raStart, raEnd, decStart, decEnd)
    }

  given Cogen[SiteCoordinatesLimits] =
    Cogen[(RightAscension, RightAscension, Declination, Declination)].contramap {
      x => (x.raStart, x.raEnd, x.decStart, x.decEnd)
    }

}

object ArbSiteCoordinatesLimits extends ArbSiteCoordinatesLimits
