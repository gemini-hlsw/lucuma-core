// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.OrderTests
import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.optics.laws.discipline.FormatTests
import monocle.law.discipline.PrismTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen

class ExtinctionSuite extends DisciplineSuite with ArbitraryInstances {
  
  given Arbitrary[Extinction] =
    Arbitrary(Arbitrary.arbitrary[NonNegShort].map(Extinction.apply))

  given Cogen[Extinction] =
    Cogen[NonNegShort].contramap(_.underlying)

  checkAll("Order", OrderTests[Extinction].order)
  checkAll("FromMillimags", PrismTests(Extinction.FromMillimags))
  checkAll("FromMags", FormatTests(Extinction.FromMags).format)
  checkAll("Cocdec", CodecTests[Extinction].codec)

}