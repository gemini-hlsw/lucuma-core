// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.kernel.laws.discipline.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.arb.ArbTime.arbDuration
import lucuma.core.optics.laws.discipline.*
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

  test("bounded division") {
    forAll { (t0: TimeSpan, i: Int) =>
      NonZeroInt.from(i).forall { nzi =>
        val res = t0 /| nzi
        if (nzi.value < 0) res === TimeSpan.Zero
        else {
          val diff = t0 -| res *| nzi.value
          TimeSpan.Zero <= diff && diff < TimeSpan.unsafeFromMicroseconds(nzi.value.toLong)
        }
      }
    }
  }

}
