// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.model.arb._
import lucuma.core.util.arb._

import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import lucuma.core.optics.laws.discipline._
import munit._
import org.scalacheck.Prop._

final class EphemerisKeySuite extends DisciplineSuite {

  import ArbEphemerisKey._
  import ArbEnumerated._

  // Laws
  checkAll("EphemerisKey", OrderTests[EphemerisKey].order)
  checkAll("fromString", FormatTests(EphemerisKey.fromString).formatWith(ArbEphemerisKey.strings))
  checkAll("fromTypeAndDes",
           FormatTests(EphemerisKey.fromTypeAndDes).formatWith(ArbEphemerisKey.keyAndDes)
  )

  test("Equality must be natural") {
    forAll { (a: EphemerisKey, b: EphemerisKey) =>
      assertEquals(a.equals(b), Eq[EphemerisKey].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: EphemerisKey) =>
      assertEquals(a.toString, Show[EphemerisKey].show(a))
    }
  }

}
