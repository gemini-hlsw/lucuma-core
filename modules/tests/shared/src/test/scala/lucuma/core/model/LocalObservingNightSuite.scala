// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import munit._
import org.scalacheck.Prop._
import cats.{ Eq, Show }
import lucuma.core.model.LocalObservingNight
import lucuma.core.arb.ArbTime._
import lucuma.core.model.arb.ArbObservingNight._
import lucuma.core.util.arb.ArbEnumerated._
import cats.kernel.laws.discipline._
import monocle.law.discipline._
import io.chrisdavenport.cats.time._

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
