// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order.*
import cats.syntax.all.*
import lucuma.core.arb.ArbTime
import lucuma.core.math.BoundedInterval
import lucuma.core.math.arb.ArbInterval
import lucuma.core.math.arb.*
import lucuma.core.optics.Spire
import lucuma.core.optics.laws.discipline.FormatTests
import lucuma.core.optics.laws.discipline.SplitEpiTests
import lucuma.core.syntax.time.*
import lucuma.core.tests.ScalaCheckFlaky
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.typelevel.cats.time.*

import java.time.Duration
import java.time.Instant
import java.time.LocalTime
import java.time.ZoneId

final class IntervalSuite extends munit.DisciplineSuite {
  import ArbInterval.given
  import ArbTime.*

  // Optics
  checkAll(
    "Spire.openUpperIntervalFromTuple",
    FormatTests(Spire.openUpperIntervalFromTuple[Int]).format
  )
  checkAll(
    "Spire.intervalListUnion",
    SplitEpiTests(Spire.intervalListUnion[Int]).splitEpi
  )

  test("ToFullDays".tag(ScalaCheckFlaky)) {
    forAll { (i: BoundedInterval[Instant], z: ZoneId, t: LocalTime) =>
      val allDay = i.toFullDays(z, t)
      assert(i.isSubsetOf(allDay))
      // We can't comparte LocalTimes directly since the LocalTime may not exist at
      // the given day if there was a DST transition.
      val start  = allDay.lower.atZone(z)
      assertEquals(start, start.`with`(t))
      val end    = allDay.upper.atZone(z)
      assertEquals(end, end.`with`(t))
      assert((allDay -- i).forall(_.asInstanceOf[BoundedInterval[Instant]].duration < Duration.ofDays(1)))
    }
  }
}
