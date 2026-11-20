// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.sequence.arb.ArbExecutedSteps.given
import monocle.law.discipline.*
import munit.*
import org.scalacheck.Prop.forAll

class ExecutedStepsSuite extends DisciplineSuite:
  checkAll("Eq[ExecutedSteps]",          EqTests[ExecutedSteps].eqv)
  checkAll("Monoid[ExecutedSteps]",      MonoidTests[ExecutedSteps].monoid)
  checkAll("ExecutedSteps.successful",   LensTests(ExecutedSteps.successful))
  checkAll("ExecutedSteps.unsuccessful", LensTests(ExecutedSteps.unsuccessful))

  property("total is successful + unsuccessful, saturating"):
    forAll: (e: ExecutedSteps) =>
      val expected = (e.successful.value.toLong + e.unsuccessful.value).min(Int.MaxValue)
      assertEquals(e.total.value.toLong, expected)

  test("total saturates"):
    val e = ExecutedSteps(NonNegInt.MaxValue, NonNegInt.unsafeFrom(1))
    assertEquals(e.total, NonNegInt.MaxValue)
