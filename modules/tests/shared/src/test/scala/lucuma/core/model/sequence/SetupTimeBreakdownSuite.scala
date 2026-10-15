// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.syntax.monoid.*
import lucuma.core.enums.ChargeClass
import lucuma.core.model.sequence.arb.ArbCategorizedTime.given
import lucuma.core.model.sequence.arb.ArbSetupTimeBreakdown.given
import lucuma.core.util.TimeSpan
import monocle.law.discipline.*
import munit.*

class SetupTimeBreakdownSuite extends DisciplineSuite:
  checkAll("Order[SetupTimeBreakdown]",             OrderTests[SetupTimeBreakdown].order)
  checkAll("CommutativeMonoid[SetupTimeBreakdown]", CommutativeMonoidTests[SetupTimeBreakdown].commutativeMonoid)
  checkAll("SetupTimeBreakdown.slew",               LensTests(SetupTimeBreakdown.slew))
  checkAll("SetupTimeBreakdown.acquisition",        LensTests(SetupTimeBreakdown.acquisition))
  checkAll("SetupTimeBreakdown.other",              LensTests(SetupTimeBreakdown.other))

  private def minutes(m: Int): CategorizedTime =
    CategorizedTime.Zero.sumCharge(ChargeClass.Program, TimeSpan.fromMinutes(m).get)

  test("total sums the components"):
    assertEquals(SetupTimeBreakdown(minutes(1), minutes(2), minutes(3)).total, minutes(6))

  test("total of a sum is the sum of the totals"):
    val a = SetupTimeBreakdown(minutes(1), minutes(2), minutes(3))
    val b = SetupTimeBreakdown(minutes(4), minutes(5), minutes(6))
    assertEquals((a |+| b).total, a.total |+| b.total)
