// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.data

import cats.kernel.Eq
import cats.kernel.laws.discipline.EqTests
import eu.timepit.refined.cats.*
import io.circe.*
import io.circe.refined.*
import io.circe.testing.CodecTests
import io.circe.testing.instances.arbitraryJson
import lucuma.core.data.arb.ArbEmailAddress.given
import monocle.law.discipline.PrismTests

/**
  * Tests the EmailAddress typeclasses
  */
class EmailAddressSuite extends munit.DisciplineSuite {

  test("typeclasses") {
    checkAll("EmailAddress", EqTests[EmailAddress].eqv)
    checkAll("EmailAddressCodec", CodecTests[EmailAddress].codec)
    checkAll("EmailAddress.from", PrismTests(EmailAddress.From))
  }
}
