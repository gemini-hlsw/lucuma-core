// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import coulomb.ops.algebra.cats.all.given
import coulomb.units.accepted.Percent
import eu.timepit.refined.cats._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.scalacheck.string._
import lucuma.core.math.arb.ArbQuantity
import lucuma.core.math.arb.ArbRefined
import monocle.law.discipline.IsoTests
import monocle.law.discipline.PrismTests
import munit.DisciplineSuite

class OpticsSpec extends DisciplineSuite {
  import ArbRefined.given
  import ArbQuantity.given // This import has to be last.
  
  checkAll("refinedPrism[String, NonEmpty]", PrismTests(refinedPrism[String, NonEmpty]))
  checkAll("refinedPrism[Int, Positive]", PrismTests(refinedPrism[Int, Positive]))
  checkAll("refinedPrism[BigDecimal, Positive]", PrismTests(refinedPrism[BigDecimal, Positive]))
  checkAll("quantityIso[Int, Percent]", IsoTests(quantityIso[Int, Percent]))
}
