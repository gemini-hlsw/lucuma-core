// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ChargeClass
import lucuma.core.model.sequence.arb.ArbExecutionDigest.given
import lucuma.core.util.TimeSpan
import munit.*

final class ExecutionDigestSuite extends DisciplineSuite:

  checkAll("Eq[ExecutionDigest]", EqTests[ExecutionDigest].eqv)

  test("fullTimeEstimate includes setups and reacquisitions"):
    val digest =
      ExecutionDigest.Zero.copy(
        setup              = SetupTime(TimeSpan.fromMinutes(16).get, TimeSpan.fromMinutes(5).get),
        setupCount         = NonNegInt.unsafeFrom(2),
        reacquisitionCount = NonNegInt.unsafeFrom(3)
      )

    assertEquals(digest.totalSetupTime, TimeSpan.fromMinutes(47).get)
    assertEquals(digest.fullTimeEstimate.sum, TimeSpan.fromMinutes(47).get)

  test("fullTimeEstimate includes expected calibrations but not existing ones"):
    def estimate(minutes: Int): CalibrationEstimate =
      CalibrationEstimate(
        NonNegInt.unsafeFrom(1),
        CategorizedTime.Zero.sumCharge(ChargeClass.Program, TimeSpan.fromMinutes(minutes).get)
      )

    val digest =
      ExecutionDigest.Zero.copy(
        calibrations = CalibrationDigest(existing = estimate(10), expected = estimate(15))
      )

    assertEquals(digest.fullTimeEstimate.sum, TimeSpan.fromMinutes(15).get)
