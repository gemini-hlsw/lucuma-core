// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.{ Eq, Show }
import cats.implicits._
import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.optics.laws.discipline._
import java.time.LocalDateTime
import lucuma.core.arb.ArbTime
import org.scalacheck.Prop._
import lucuma.core.math.parser.EpochParsers._
import lucuma.core.syntax.parser._

final class EpochSuite extends munit.DisciplineSuite {
  import ArbEpoch._
  import ArbTime._

  // Laws
  checkAll("Epoch", OrderTests[Epoch].order)
  checkAll("fromString", FormatTests(Epoch.fromString).formatWith(ArbEpoch.strings))
  checkAll("fromStringNoScheme",
           FormatTests(Epoch.fromStringNoScheme)
             .formatWith(ArbEpoch.stringsNoScheme)(Eq[String], arbJulianEpoch, Eq[Epoch])
  )

  test("Epoch.eq.natural") {
    forAll { (a: Epoch, b: Epoch) =>
      assertEquals(a.equals(b), Eq[Epoch].eqv(a, b))
    }
  }

  test("Epoch.show.natural") {
    forAll { (a: Epoch) =>
      assertEquals(a.toString, Show[Epoch].show(a))
    }
  }

  test("Epoch.until.identity") {
    forAll { (a: Epoch) =>
      assertEquals(a.untilEpochYear(a.epochYear), 0.0)
    }
  }

  test("Epoch.until.sanity") {
    forAll { (a: Epoch, s: Short) =>
      assertEquals(a.untilEpochYear(a.epochYear + s.toDouble), s.toDouble)
    }
  }

  test("Epoch.until.sanity2") {
    forAll { (s: Epoch.Scheme, d1: LocalDateTime, d2: LocalDateTime) =>
      val Δ1 = s.fromLocalDateTime(d1).untilLocalDateTime(d2)
      val Δ2 = s.fromLocalDateTime(d2).epochYear - s.fromLocalDateTime(d1).epochYear
      assertEquals(Δ1, Δ2)
    }
  }

  test("epochLenient") {
    assertEquals(epochLenient.parseExact("J2014.123"), Epoch.Julian.fromMilliyears(2014123).some)
    assertEquals(epochLenient.parseExact("J2014"), Epoch.Julian.fromMilliyears(2014000).some)
    assertEquals(epochLenient.parseExact("J2014."), Epoch.Julian.fromMilliyears(2014000).some)
    assertEquals(epochLenient.parseExact("J2014.1"), Epoch.Julian.fromMilliyears(2014100).some)
    assertEquals(epochLenient.parseExact("2014.123"), Epoch.Julian.fromMilliyears(2014123).some)
    assertEquals(epochLenient.parseExact("2014"), Epoch.Julian.fromMilliyears(2014000).some)
    assertEquals(epochLenient.parseExact("2014."), Epoch.Julian.fromMilliyears(2014000).some)
    assertEquals(epochLenient.parseExact("2014.1"), Epoch.Julian.fromMilliyears(2014100).some)
    assertEquals(epochLenient.parseExact("2014.092"), Epoch.Julian.fromMilliyears(2014092).some)
    assertEquals(epochLenient.parseExact("2014.002"), Epoch.Julian.fromMilliyears(2014002).some)
  }

  test("epochFormatNoScheme") {
    assertEquals(Epoch.fromStringNoScheme.reverseGet(Epoch.Julian.fromMilliyears(2014123)),
                 "2014.123"
    )
    assertEquals(Epoch.fromStringNoScheme.reverseGet(Epoch.Julian.fromMilliyears(2014000)), "2014")
    assertEquals(Epoch.fromStringNoScheme.reverseGet(Epoch.Julian.fromMilliyears(2014300)),
                 "2014.3"
    )
    assertEquals(Epoch.fromStringNoScheme.reverseGet(Epoch.Julian.fromMilliyears(2014092)),
                 "2014.092"
    )
    assertEquals(Epoch.fromStringNoScheme.reverseGet(Epoch.Julian.fromMilliyears(2014002)),
                 "2014.002"
    )
    assertEquals(Epoch.fromStringNoScheme.reverseGet(Epoch.Julian.fromMilliyears(2014350)),
                 "2014.35"
    )
  }
}
