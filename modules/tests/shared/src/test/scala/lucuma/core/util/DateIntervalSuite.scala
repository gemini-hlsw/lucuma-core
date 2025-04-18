// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.kernel.laws.discipline.*
import lucuma.core.util.arb.ArbDateInterval
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

class DateIntervalSuite extends DisciplineSuite {

  import ArbDateInterval.given

  checkAll("Eq", EqTests[DateInterval].eqv)

  test("period in days") {
    forAll { (interval: DateInterval) =>
      assertEquals(interval.start.plusDays(interval.days), interval.end)
    }
  }

}
