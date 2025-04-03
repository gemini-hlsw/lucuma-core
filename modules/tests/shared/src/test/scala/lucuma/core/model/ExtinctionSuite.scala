// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.OrderTests
import coulomb.testkit.given
import eu.timepit.refined.scalacheck.all.*
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.math.arb.ArbRefined.given
import lucuma.core.optics.laws.discipline.FormatTests
import lucuma.core.util.arb.ArbNewType.given
import monocle.law.discipline.PrismTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen

class ExtinctionSuite extends DisciplineSuite with ArbitraryInstances {
  checkAll("Order", OrderTests[Extinction].order)
  checkAll("FromMilliVegaMagnitude", PrismTests(Extinction.FromMilliVegaMagnitude))
  checkAll("FromVegaMagnitude", FormatTests(Extinction.FromVegaMagnitude).format)
  checkAll("Codec", CodecTests[Extinction].codec)

  test("Refine to CloudExtinction") {
    val e1 = Extinction.FromVegaMagnitude.getOption(2).get
    val e2 = Extinction.FromVegaMagnitude.getOption(20).get
    assert(CloudExtinction.from(e1).isRight)
    assert(CloudExtinction.from(e2).isLeft)
  }

}