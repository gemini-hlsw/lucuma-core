// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.syntax.monoid.*
import lucuma.core.enums.ChargeClass
import lucuma.core.model.sequence.arb.ArbCategorizedTime
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimeSpan
import munit.*
import org.scalacheck.Prop.forAll

final class CategorizedTimeSuite extends DisciplineSuite {

  import ArbEnumerated.*
  import ArbCategorizedTime.given
  import ArbTimeSpan.given

  checkAll("Order[CategorizedTime]",  OrderTests[CategorizedTime].order)
  checkAll("Monoid[CategorizedTime]", CommutativeMonoidTests[CategorizedTime].commutativeMonoid)

  test("monoid") {
    forAll { (a: CategorizedTime, b: CategorizedTime) =>
      val c = a |+| b
      val d = a.charges.foldLeft(b) { case (r, (cc, ts)) =>
        r.sumCharge(cc, ts)
      }
      assertEquals(c, d)
    }
  }

  test("modify") {
    forAll { (tc: CategorizedTime, c: ChargeClass, s: TimeSpan) =>
      assertEquals(tc.modify(c, _ +| s)(c), tc(c) +| s)
      assertEquals(tc.modify(c, _ -| s)(c), tc(c) -| s)
    }
  }

  test("apply") {
    forAll { (a: CategorizedTime) =>
      assertEquals(a(ChargeClass.Program), a.getOrZero(ChargeClass.Program))
    }
  }

}
