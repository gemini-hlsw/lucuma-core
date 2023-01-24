// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.kernel.laws.discipline.*
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.numeric._
import lucuma.core.arb.ArbTime.arbDuration
import lucuma.core.optics.laws.discipline.*
import lucuma.core.util.arb.ArbTimeSpan
import lucuma.core.util.arb.ArbTimeSpan.genDuration
import lucuma.core.util.arb.ArbTimeSpan.genTimeSpanString
import monocle.law.discipline.IsoTests
import monocle.law.discipline.PrismTests
import munit.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.typelevel.cats.time.*

import java.time.Duration


class TimeSpanSuite extends DisciplineSuite {

  import ArbTimeSpan.given

  checkAll("TimeSpan.NonNegMicroseconds", IsoTests(TimeSpan.NonNegMicroseconds))
  checkAll("TimeSpan.FromMicroseconds",   PrismTests(TimeSpan.FromMicroseconds))
  checkAll("TimeSpan.FromMilliseconds",   FormatTests(TimeSpan.FromMilliseconds).format)
  checkAll("TimeSpan.FromSeconds",        FormatTests(TimeSpan.FromSeconds).format)
  checkAll("TimeSpan.FromMinutes",        FormatTests(TimeSpan.FromMinutes).format)
  checkAll("TimeSpan.FromHours",          FormatTests(TimeSpan.FromHours).format)
  checkAll("TimeSpan.FromDuration",       FormatTests(TimeSpan.FromDuration).formatWith(genDuration))
  checkAll("TimeSpan.FromString",         FormatTests(TimeSpan.FromString).formatWith(genTimeSpanString))
  checkAll("TimeSpan.Order",              OrderTests[TimeSpan].order)

  import lucuma.core.syntax.timespan.*

  test("literal syntax") {
    assertEquals(1.µsTimeSpan,  TimeSpan.fromMicroseconds(1).get)
    assertEquals(1.msTimeSpan,  TimeSpan.fromMilliseconds(BigDecimal(1)).get)
    assertEquals(1.secTimeSpan, TimeSpan.fromSeconds(BigDecimal(1)).get)
    assertEquals(1.minTimeSpan, TimeSpan.fromMinutes(BigDecimal(1)).get)
    assertEquals(1.hrTimeSpan,  TimeSpan.fromHours(BigDecimal(1)).get)

    assertEquals(1L.µsTimeSpan,  TimeSpan.fromMicroseconds(1L).get)
    assertEquals(1L.msTimeSpan,  TimeSpan.fromMilliseconds(BigDecimal(1L)).get)
    assertEquals(1L.secTimeSpan, TimeSpan.fromSeconds(BigDecimal(1L)).get)
    assertEquals(1L.minTimeSpan, TimeSpan.fromMinutes(BigDecimal(1L)).get)
    assertEquals(1L.hrTimeSpan,  TimeSpan.fromHours(BigDecimal(1L)).get)

    assertEquals(2562047788L.hrTimeSpan,  TimeSpan.fromHours(BigDecimal(2562047788L)).get)

    // Out of range, won't compile.
    // assertEquals(2562047789L.hrTimeSpan,  TimeSpan.fromHours(BigDecimal(2562047789L)).get)

    intercept[NoSuchElementException] {
      TimeSpan.fromHours(BigDecimal(2562047789L)).get
    }
  }
}
