// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.syntax.monoid.*
import lucuma.core.enums.SetupKind
import lucuma.core.model.sequence.arb.ArbSetupCharge.given
import lucuma.core.model.sequence.arb.ArbSetupChargeSummary.given
import monocle.law.discipline.*
import munit.*
import org.scalacheck.Prop.forAll

class SetupChargeSuite extends DisciplineSuite:
  checkAll("Order[SetupCharge]",             OrderTests[SetupCharge].order)
  checkAll("CommutativeMonoid[SetupCharge]", CommutativeMonoidTests[SetupCharge].commutativeMonoid)
  checkAll("SetupCharge.full",               LensTests(SetupCharge.full))
  checkAll("SetupCharge.reacquisition",      LensTests(SetupCharge.reacquisition))

  property("of places the summary under its kind"):
    forAll: (s: SetupChargeSummary) =>
      SetupKind.values.toList.foreach: k =>
        assertEquals(SetupCharge.of(k, s)(k), s)

  property("total combines both kinds"):
    forAll: (c: SetupCharge) =>
      assertEquals(c.total, c.full.time |+| c.reacquisition.time)
