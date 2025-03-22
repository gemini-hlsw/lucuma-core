// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.arb

import lucuma.catalog.AngularSize
import lucuma.catalog.CatalogTargetResult
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbTarget
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbCatalogTargetResult {
  import ArbTarget.given
  import ArbAngularSize.given

  given Arbitrary[CatalogTargetResult] =
    Arbitrary {
      for {
        t <- arbitrary[Target.Sidereal]
        s <- arbitrary[Option[AngularSize]]
      } yield CatalogTargetResult(t, s)
    }

  given Cogen[CatalogTargetResult] =
    Cogen[(Target.Sidereal, Option[AngularSize])].contramap(r => (r.target, r.angularSize))
}

object ArbCatalogTargetResult extends ArbCatalogTargetResult
