// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.Eq
import cats.Show
import cats.kernel.laws.discipline.*
import lucuma.core.arb.ArbTime.given
import lucuma.core.model.LocalObservingNight
import lucuma.core.model.arb.ArbObservingNight.given
import monocle.law.discipline.*
import munit.*
import org.scalacheck.Prop.*
import org.typelevel.cats.time.*

final class LocalObservingNightSuite extends DisciplineSuite {

  checkAll("LocalObservingNight", OrderTests[LocalObservingNight].order)
  checkAll("LocalObservingNight.localDate", IsoTests(LocalObservingNight.localDate))

  test("Equality must be natural") {
    forAll { (a: LocalObservingNight, b: LocalObservingNight) =>
      assertEquals(a.equals(b), Eq[LocalObservingNight].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (o: LocalObservingNight) =>
      assertEquals(o.toString, Show[LocalObservingNight].show(o))
    }
  }

  test("Always begins at 2PM") {
    forAll { (o: LocalObservingNight) =>
      assertEquals(o.start.toLocalTime, LocalObservingNight.StartTime)
    }
  }

  test("Always ends at 2PM") {
    forAll { (o: LocalObservingNight) =>
      assertEquals(o.end.toLocalTime, LocalObservingNight.StartTime)
    }
  }

  test("Is contiguous (1)") {
    forAll { (o: LocalObservingNight) =>
      assertEquals(o.previous.end, o.start)
    }
  }

  test("Is contiguous (2)") {
    forAll { (o: LocalObservingNight) =>
      assertEquals(o.next.start, o.end)
    }
  }

  test("Includes start") {
    forAll { (o: LocalObservingNight) =>
      assertEquals(o.includes(o.start), true)
    }
  }

  test("Excludes end") {
    forAll { (o: LocalObservingNight) =>
      assertEquals(o.includes(o.end), false)
    }
  }

  test("night.previous.next shouldEqual night") {
    forAll { (o: LocalObservingNight) =>
      assertEquals(o.previous.next, o)
    }
  }

  test("can always parse a formatted night") {
    forAll { (o: LocalObservingNight) =>
      assertEquals(LocalObservingNight.fromString(o.format), Some(o))
    }
  }

}
