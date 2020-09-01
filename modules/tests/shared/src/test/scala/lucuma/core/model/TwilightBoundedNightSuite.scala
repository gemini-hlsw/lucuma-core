// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import munit._
import org.scalacheck.Prop._
import cats.implicits._
import cats.{ Eq, Show }
import java.time._
import lucuma.core.enum.Site
import lucuma.core.math.skycalc.TwilightBoundType
import lucuma.core.math.arb.ArbTime._
import lucuma.core.math.arb.ArbTwilightBoundType._
import lucuma.core.model.arb.ArbObservingNight._
import lucuma.core.model.arb.ArbTwilightBoundedNight._
import lucuma.core.util.arb.ArbEnumerated._
import cats.kernel.laws.discipline._
import monocle.law.discipline._
import io.chrisdavenport.cats.time._

final class TwilightBoundedNightSuite extends DisciplineSuite {
  checkAll("TwilightBoundedNight", OrderTests[TwilightBoundedNight].order)
  checkAll("TwilightBoundedNight.boundType", LensTests(TwilightBoundedNight.boundType))
  checkAll("TwilightBoundedNight.observingNight", LensTests(TwilightBoundedNight.observingNight))
  checkAll("TwilightBoundedNight.localObservingNight",
           LensTests(TwilightBoundedNight.localObservingNight)
  )
  checkAll("TwilightBoundedNight.site", LensTests(ObservingNight.site))
  checkAll("TwilightBoundedNight.localDate", LensTests(ObservingNight.localDate))

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

  test("fromBoundTypeAndSiteAndLocalDate consistent") {
    forAll { (b: TwilightBoundType, s: Site, l: LocalDate) =>
      TwilightBoundedNight
        .fromBoundTypeAndSiteAndLocalDate(b, s, l)
        .foreach(tbn => assertEquals(tbn.toLocalDate, l))
    }
  }

  test("fromBoundTypeAndSiteAndLocalDateTime consistent") {
    forAll { (b: TwilightBoundType, s: Site, l: LocalDateTime) =>
      val n  = TwilightBoundedNight.fromBoundTypeAndSiteAndLocalDateTime(b, s, l)
      val d  = l.toLocalDate
      val dʹ = if (l.toLocalTime.isBefore(LocalObservingNight.Start)) d else d.plusDays(1L)
      n.foreach(tbn => assertEquals(tbn.toLocalDate, dʹ))
    }
  }
}
