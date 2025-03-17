// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.syntax.eq.*
import cats.syntax.monoid.*
import cats.syntax.option.*
import lucuma.core.enums.ChargeClass
import lucuma.core.model.ServiceUser
import lucuma.core.model.User
import lucuma.core.model.sequence.arb.ArbCategorizedTime
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimeSpan
import munit.*
import org.scalacheck.Prop.forAll

final class CategorizedTimeSuite extends DisciplineSuite {

  import ArbEnumerated.given
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
    forAll { (a: CategorizedTime, c: ChargeClass, s: TimeSpan) =>
      assertEquals(a.modify(c, _ +| s)(c), a(c) +| s)
      assertEquals(a.modify(c, _ -| s)(c), a(c) -| s)
    }
  }

  test("apply") {
    forAll { (a: CategorizedTime) =>
      assertEquals(a(ChargeClass.Program), a.programTime)
      assertEquals(a(ChargeClass.NonCharged), a.nonCharged)
    }
  }

  test("sumCharge") {
    forAll { (a: CategorizedTime, t: TimeSpan, c: ChargeClass) =>
      assertEquals(a.sumCharge(c, t)(c), a(c) +| t)
    }
  }

  test("sum") {
    forAll { (a: CategorizedTime) =>
      assertEquals(a.sum, a(ChargeClass.Program) +| a(ChargeClass.NonCharged))
    }
  }

  test("CategorizedTime.Zero equals explicitly specified TimeSpan.Zero charges") {
    assertEquals(
      CategorizedTime(
        ChargeClass.Program    -> TimeSpan.Zero,
        ChargeClass.NonCharged -> TimeSpan.Zero
      ),
      CategorizedTime.Zero
    )

    assertEquals(
      CategorizedTime.from(List(ChargeClass.Program -> TimeSpan.Zero)),
      CategorizedTime.Zero
    )
  }

  test("CategorizedTime.Zero equals computed zero") {
    assertEquals(
      CategorizedTime.Zero.sumCharge(ChargeClass.Program, TimeSpan.Zero),
      CategorizedTime.Zero
    )

    val ms         = TimeSpan.unsafeFromMicroseconds(1000L)
    val correction = TimeChargeCorrection(
      Timestamp.Min,
      ServiceUser(User.Id.fromLong(1L).get, "Test"),
      ChargeClass.Program,
      TimeChargeCorrection.Op.Subtract,
      ms,
      none
    )

    assertEquals(
      CategorizedTime(ChargeClass.Program -> ms).correct(correction),
      CategorizedTime.Zero
    )
  }

  test("equals and Order agree") {
    forAll { (a: CategorizedTime, b: CategorizedTime) =>
      (a == b) === (a === b)
    }
  }

}
