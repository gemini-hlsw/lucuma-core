// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.kernel.laws.discipline.*
import cats.syntax.order.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.optics.laws.discipline.*
import lucuma.core.refined.numeric.NonZeroBigDecimal
import lucuma.core.refined.numeric.NonZeroInt
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
  checkAll("TimeSpan.Monoid",             MonoidTests[TimeSpan].monoid)

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

  test("bounded addition") {
    forAll { (t0: TimeSpan, t1: TimeSpan) =>
      val big    = BigInt(t0.toMicroseconds) + BigInt(t1.toMicroseconds)
      val expect = Option.when(big.isValidLong)(TimeSpan.unsafeFromMicroseconds(big.longValue)).getOrElse(TimeSpan.Max)
      assertEquals(t0 +| t1, expect)
    }
  }

  test("bounded subtraction") {
    forAll { (t0: TimeSpan, t1: TimeSpan) =>
      val big    = BigInt(t0.toMicroseconds) - BigInt(t1.toMicroseconds)
      val expect = Option.when(big >= BigInt(0))(TimeSpan.unsafeFromMicroseconds(big.longValue)).getOrElse(TimeSpan.Min)
      assertEquals(t0 -| t1, expect)
    }
  }

  test("bounded int division") {
    forAll { (t0: TimeSpan, i: Int) =>
      NonZeroInt.from(i).forall { nz =>
        val res = t0 /| nz
        if (nz.value < 0) res === TimeSpan.Zero
        else {
          val diff = t0 -| res *| nz.value
          TimeSpan.Zero <= diff && diff < TimeSpan.unsafeFromMicroseconds(nz.value.toLong)
        }
      }
    }
  }

  test("multiply by 0.5 is divide by 2") {
    forAll { (t: TimeSpan) =>
      assertEquals(t *| BigDecimal(0.5), t /| NonZeroBigDecimal.unsafeFrom(2.0))
    }
  }

  test("multiply by negative is Zero") {
    forAll { (t: TimeSpan) =>
      assertEquals(t *| -1, TimeSpan.Zero)
      assertEquals(t *| BigDecimal(-1), TimeSpan.Zero)
    }
  }

  test("divide by negative is Zero") {
    forAll { (t: TimeSpan) =>
      assertEquals(t /| NonZeroInt.unsafeFrom(-1), TimeSpan.Zero)
      assertEquals(t /| NonZeroBigDecimal.unsafeFrom(-1.0), TimeSpan.Zero)
    }
  }

  test("multiply by big is Max") {
    assertEquals(TimeSpan.unsafeFromMicroseconds(4294967299L) *| Int.MaxValue, TimeSpan.Max)
    assertEquals(TimeSpan.unsafeFromMicroseconds(4294967299L) *| BigDecimal(Int.MaxValue), TimeSpan.Max)
  }

  test("divide by small is Max") {
    assertEquals(TimeSpan.unsafeFromMicroseconds(4294967299L) /| NonZeroBigDecimal.unsafeFrom(BigDecimal(1) / Int.MaxValue), TimeSpan.Max)
  }

  test("timespan parts") {
    assertEquals(TimeSpan.fromMicroseconds(10L).get.toSecondsPart, 0)
    assertEquals(TimeSpan.fromMicroseconds(10L).get.toMinutesPart, 0)
    assertEquals(TimeSpan.fromMicroseconds(10L).get.toHoursPart, 0)
    assertEquals(TimeSpan.fromMicroseconds(10L).get.toMillisPart, 0)
    assertEquals(TimeSpan.fromMicroseconds(1000L).get.toSecondsPart, 0)
    assertEquals(TimeSpan.fromMicroseconds(1000L).get.toMinutesPart, 0)
    assertEquals(TimeSpan.fromMicroseconds(1000L).get.toHoursPart, 0)
    assertEquals(TimeSpan.fromMicroseconds(1000L).get.toMillisPart, 1)
    assertEquals(TimeSpan.fromSeconds(0L).get.toSecondsPart, 0)
    assertEquals(TimeSpan.fromSeconds(0L).get.toMinutesPart, 0)
    assertEquals(TimeSpan.fromSeconds(0L).get.toHoursPart, 0)
    assertEquals(TimeSpan.fromSeconds(0L).get.toMillisPart, 0)
    assertEquals(TimeSpan.fromSeconds(10L).get.toSecondsPart, 10)
    assertEquals(TimeSpan.fromSeconds(10L).get.toMinutesPart, 0)
    assertEquals(TimeSpan.fromSeconds(10L).get.toHoursPart, 0)
    assertEquals(TimeSpan.fromSeconds(10L).get.toMillisPart, 0)
    assertEquals(TimeSpan.fromSeconds(60L).get.toSecondsPart, 0)
    assertEquals(TimeSpan.fromSeconds(60L).get.toMinutesPart, 1)
    assertEquals(TimeSpan.fromSeconds(60L).get.toHoursPart, 0)
    assertEquals(TimeSpan.fromSeconds(60L).get.toMillisPart, 0)
    assertEquals(TimeSpan.fromSeconds(90L).get.toSecondsPart, 30)
    assertEquals(TimeSpan.fromSeconds(90L).get.toMinutesPart, 1)
    assertEquals(TimeSpan.fromSeconds(90L).get.toHoursPart, 0)
    assertEquals(TimeSpan.fromSeconds(90L).get.toMillisPart, 0)
    assertEquals(TimeSpan.fromSeconds(3725L).get.toSecondsPart, 5)
    assertEquals(TimeSpan.fromSeconds(3725L).get.toMinutesPart, 2)
    assertEquals(TimeSpan.fromSeconds(3725L).get.toHoursPart, 1)
    assertEquals(TimeSpan.fromSeconds(3725L).get.toMillisPart, 0)
  }
}
