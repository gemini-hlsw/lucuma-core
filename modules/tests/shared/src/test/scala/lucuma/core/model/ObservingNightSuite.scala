// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import munit._
import org.scalacheck.Prop._
import cats.{ Eq, Show }
import java.time._
import lucuma.core.enum.Site
import lucuma.core.arb.ArbTime._
import lucuma.core.model.arb.ArbObservingNight._
import lucuma.core.util.arb.ArbEnumerated._
import cats.kernel.laws.discipline._
import monocle.law.discipline._
import org.typelevel.cats.time._

final class ObservingNightSuite extends DisciplineSuite {

  checkAll("ObservingNight", OrderTests[ObservingNight].order)
  checkAll("ObservingNight.site", LensTests(ObservingNight.site))
  checkAll("ObservingNight.localObservingNight", LensTests(ObservingNight.localObservingNight))
  checkAll("ObservingNight.localDate", LensTests(ObservingNight.localDate))

  test("Equality must be natural") {
    forAll { (a: ObservingNight, b: ObservingNight) =>
      assertEquals(a.equals(b), Eq[ObservingNight].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (o: ObservingNight) =>
      assertEquals(o.toString, Show[ObservingNight].show(o))
    }
  }

  test("Start time consistent with LocalObservingNight") {
    forAll { (o: ObservingNight) =>
      assertEquals(o.start.atZone(o.site.timezone).toLocalDateTime, o.toLocalObservingNight.start)
    }
  }

  test("End time consistent with LocalObservingNight") {
    forAll { (o: ObservingNight) =>
      assertEquals(o.end.atZone(o.site.timezone).toLocalDateTime, o.toLocalObservingNight.end)
    }
  }

  test("Is contiguous (1)") {
    forAll { (o: ObservingNight) =>
      assertEquals(o.previous.end, o.start)
    }
  }

  test("Is contiguous (2)") {
    forAll { (o: ObservingNight) =>
      assertEquals(o.next.start, o.end)
    }
  }

  test("Includes start") {
    forAll { (o: ObservingNight) =>
      assertEquals(o.includes(o.start), true)
    }
  }

  test("Excludes end") {
    forAll { (o: ObservingNight) =>
      assertEquals(o.includes(o.end), false)
    }
  }

  test("night.previous.next shouldEqual night") {
    forAll { (o: ObservingNight) =>
      assertEquals(o.previous.next, o)
    }
  }

  test("night.next.previous shouldEqual night") {
    forAll { (o: ObservingNight) =>
      assertEquals(o.next.previous, o)
    }
  }

  test("handle daylight savings correctly (summer end)") {
    val h = LocalObservingNight.fromString("20180513").map(_.atSite(Site.GS).duration.toHours.toInt)
    assertEquals(h, Some(25))
  }

  test("handle daylight savings correctly (winter end)") {
    val h = LocalObservingNight.fromString("20180812").map(_.atSite(Site.GS).duration.toHours.toInt)
    assertEquals(h, Some(23))
  }

  test("fromSiteAndLocalDate consistent") {
    forAll { (s: Site, l: LocalDate) =>
      assertEquals(ObservingNight.fromSiteAndLocalDate(s, l).toLocalDate, l)
    }
  }

  test("fromSiteAndLocalDateTime consistent") {
    forAll { (s: Site, l: LocalDateTime) =>
      val n  = ObservingNight.fromSiteAndLocalDateTime(s, l)
      val d  = l.toLocalDate
      val dʹ = if (l.toLocalTime.isBefore(LocalObservingNight.StartTime)) d else d.plusDays(1L)
      assertEquals(n.toLocalDate, dʹ)
    }
  }
}
