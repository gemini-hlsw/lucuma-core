// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.arb

import lucuma.catalog.AngularSize
import lucuma.catalog.CatalogTargetResult
import lucuma.catalog.StellarParameters
import lucuma.catalog.StellarParametersSource
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbTarget
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbCatalogTargetResult {
  import ArbTarget.given
  import ArbAngularSize.given

  given Arbitrary[StellarParameters] =
    Arbitrary {
      for {
        t <- Gen.choose(2000, 50000)
        g <- Gen.choose(0.0, 6.0)
        s <- arbitrary[StellarParametersSource]
      } yield StellarParameters(t, g, s)
    }

  given Cogen[StellarParameters] =
    Cogen[(Int, Double, StellarParametersSource)].contramap(p => (p.teff, p.logG, p.source))

  given Arbitrary[CatalogTargetResult] =
    Arbitrary {
      for {
        t <- arbitrary[Target.Sidereal]
        s <- arbitrary[Option[AngularSize]]
        p <- arbitrary[Option[StellarParameters]]
      } yield CatalogTargetResult(t, s, p)
    }

  given Cogen[CatalogTargetResult] =
    Cogen[(Target.Sidereal, Option[AngularSize], Option[StellarParameters])]
      .contramap(r => (r.target, r.angularSize, r.stellarParameters))
}

object ArbCatalogTargetResult extends ArbCatalogTargetResult
