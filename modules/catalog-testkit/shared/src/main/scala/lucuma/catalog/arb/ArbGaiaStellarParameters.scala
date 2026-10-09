// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.arb

import coulomb.syntax.*
import coulomb.units.si.Kelvin
import lucuma.catalog.GaiaStellarParameters
import lucuma.catalog.GaiaStellarParametersSource
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbGaiaStellarParameters {
  given Arbitrary[GaiaStellarParameters] =
    Arbitrary {
      for {
        t <- Gen.choose(2000, 50000)
        g <- Gen.choose(0.0, 6.0)
        s <- arbitrary[GaiaStellarParametersSource]
      } yield GaiaStellarParameters(t.withUnit[Kelvin], g, s)
    }

  given Cogen[GaiaStellarParameters] =
    Cogen[(Int, Double, GaiaStellarParametersSource)].contramap(p =>
      (p.teff.value, p.logG, p.source)
    )
}

object ArbGaiaStellarParameters extends ArbGaiaStellarParameters
