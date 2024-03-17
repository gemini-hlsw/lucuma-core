// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Show
import cats.kernel.laws.discipline._
import cats.syntax.all.*
import lucuma.core.arb.ArbTime.*
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.model.arb.ArbTwilightBoundedNight.*
import lucuma.core.util.arb.ArbEnumerated.given
import munit.*
import org.scalacheck.Prop.*
import org.typelevel.cats.time.*

import java.time.*

final class TwilightBoundedNightSuite extends DisciplineSuite {
  checkAll("TwilightBoundedNight", OrderTests[TwilightBoundedNight].order)

  test("Equality must be natural") {
    forAll { (a: TwilightBoundedNight, b: TwilightBoundedNight) =>
      assertEquals(a.equals(b), Eq[TwilightBoundedNight].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (o: TwilightBoundedNight) =>
      assertEquals(o.toString, Show[TwilightBoundedNight].show(o))
    }
  }

  test("Start time consistent with ObservingNight") {
    forAll { (o: TwilightBoundedNight) =>
      assert(o.start > o.toObservingNight.start)
      assert(o.start < o.toObservingNight.end)
    }
  }

  test("End time consistent with ObservingNight") {
    forAll { (o: TwilightBoundedNight) =>
      assert(o.end > o.toObservingNight.start)
      assert(o.end < o.toObservingNight.end)
    }
  }

  test("night.previous.next shouldEqual night") {
    forAll { (o: TwilightBoundedNight) =>
      for {
        p <- o.previous
        n <- p.next
      } assertEquals(n, o)
    }
  }

  test("night.next.previous shouldEqual night") {
    forAll { (o: TwilightBoundedNight) =>
      for {
        n <- o.next
        p <- n.previous
      } assertEquals(p, o)
    }
  }

  test("fromTwilightTypeAndSiteAndLocalDate consistent") {
    forAll { (b: TwilightType, s: Site, l: LocalDate) =>
      TwilightBoundedNight
        .fromTwilightTypeAndSiteAndLocalDate(b, s, l)
        .foreach(tbn => assertEquals(tbn.toLocalDate, l))
    }
  }

  test("fromTwilightTypeAndSiteAndLocalDateTime consistent") {
    forAll { (b: TwilightType, s: Site, l: LocalDateTime) =>
      val n  = TwilightBoundedNight.fromTwilightTypeAndSiteAndLocalDateTime(b, s, l)
      val d  = l.toLocalDate
      val dʹ = if (l.toLocalTime.isBefore(LocalObservingNight.StartTime)) d else d.plusDays(1L)
      n.foreach(tbn => assertEquals(tbn.toLocalDate, dʹ))
    }
  }
}
