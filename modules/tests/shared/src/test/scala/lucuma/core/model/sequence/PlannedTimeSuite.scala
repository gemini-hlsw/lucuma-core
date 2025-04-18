// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.syntax.monoid.*
import lucuma.core.enums.ChargeClass
import lucuma.core.model.sequence.arb.ArbCategorizedTime
import lucuma.core.model.sequence.arb.ArbPlannedTime
import monocle.law.discipline.*
import munit.*
import org.scalacheck.Prop.forAll

final class PlannedTimeSuite extends DisciplineSuite {

  import ArbPlannedTime.given
  import ArbCategorizedTime.given

  checkAll("Order[PlannedTime]",  OrderTests[PlannedTime].order)
  checkAll("Monoid[PlannedTime]", MonoidTests[PlannedTime].monoid)
  checkAll("ToCategorizedTime", IsoTests(PlannedTime.ToCategorizedTime))

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
