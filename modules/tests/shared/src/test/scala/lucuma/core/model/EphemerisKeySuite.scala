// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Show
import cats.kernel.laws.discipline.*
import io.circe.testing.CodecTests
import io.circe.testing.instances.*
import lucuma.core.model.arb.*
import lucuma.core.optics.laws.discipline.*
import munit.*
import org.scalacheck.Prop.*

final class EphemerisKeySuite extends DisciplineSuite {

  import ArbEphemerisKey.given

  // Laws
  checkAll("Ephemeris.Key", OrderTests[Ephemeris.Key].order)
  checkAll("fromString", FormatTests(Ephemeris.Key.fromString).formatWith(ArbEphemerisKey.strings))
  checkAll(
    "fromTypeAndDes",
    FormatTests(Ephemeris.Key.fromTypeAndDes).formatWith(ArbEphemerisKey.keyAndDes)
  )
  checkAll("JSON Codec", CodecTests[Ephemeris.Key].codec)

  test("Equality must be natural") {
    forAll { (a: Ephemeris.Key, b: Ephemeris.Key) =>
      assertEquals(a.equals(b), Eq[Ephemeris.Key].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: Ephemeris.Key) =>
      assertEquals(a.toString, Show[Ephemeris.Key].show(a))
    }
  }

}
