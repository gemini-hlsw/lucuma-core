// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.kernel.laws.discipline.*
import coulomb.integrations.cats.quantity.given
import lucuma.catalog.arb.all.given
import lucuma.core.math.arb.ArbQuantity.given
import lucuma.core.util.arb.ArbEnumerated.given
import monocle.law.discipline.*
import munit.*

class GaiaStellarParametersLawsSuite extends DisciplineSuite:
  checkAll("Eq[GaiaStellarParameters]", EqTests[GaiaStellarParameters].eqv)
  checkAll("GaiaStellarParameters.teff", LensTests(GaiaStellarParameters.teff))
  checkAll("GaiaStellarParameters.logG", LensTests(GaiaStellarParameters.logG))
  checkAll("GaiaStellarParameters.source", LensTests(GaiaStellarParameters.source))
