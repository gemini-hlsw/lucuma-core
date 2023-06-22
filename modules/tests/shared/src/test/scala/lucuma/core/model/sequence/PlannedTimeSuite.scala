// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.syntax.monoid.*
import lucuma.core.enums.ChargeClass
import lucuma.core.model.sequence.arb.ArbPlannedTime
import munit.*
import org.scalacheck.Prop.forAll

final class PlannedTimeSuite extends DisciplineSuite {

  import ArbPlannedTime.given

  checkAll("Eq[PlannedTime]",     EqTests[PlannedTime].eqv)
  checkAll("Monoid[PlannedTime]", MonoidTests[PlannedTime].monoid)

  test("monoid") {
    forAll { (a: PlannedTime, b: PlannedTime) =>
      val c = a |+| b
      val d = a.charges.foldLeft(b) { case (r, (cc, ts)) =>
        r.sumCharge(cc, ts)
      }
      assertEquals(c, d)
    }
  }

  test("apply") {
    forAll { (a: PlannedTime) =>
      assertEquals(a(ChargeClass.Program), a.getOrZero(ChargeClass.Program))
    }
  }

}
