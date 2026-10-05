// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ChargeClass
import lucuma.core.model.sequence.arb.ArbCalibrationDigest.given
import lucuma.core.model.sequence.arb.ArbCalibrationEstimate.given
import lucuma.core.util.TimeSpan
import monocle.law.discipline.*
import munit.*

class CalibrationDigestSuite extends DisciplineSuite:
  checkAll("Eq[CalibrationDigest]",      EqTests[CalibrationDigest].eqv)
  checkAll("CalibrationDigest.existing", LensTests(CalibrationDigest.existing))
  checkAll("CalibrationDigest.expected", LensTests(CalibrationDigest.expected))

  private def estimate(count: Int, minutes: Int): CalibrationEstimate =
    CalibrationEstimate(
      NonNegInt.unsafeFrom(count),
      CategorizedTime.Zero.sumCharge(ChargeClass.Program, TimeSpan.fromMinutes(minutes).get)
    )

  test("count includes existing and expected"):
    assertEquals(CalibrationDigest(estimate(2, 10), estimate(3, 15)).count.value, 5)

  test("count saturates"):
    val max = CalibrationEstimate(NonNegInt.unsafeFrom(Int.MaxValue), CategorizedTime.Zero)
    assertEquals(CalibrationDigest(max, max).count.value, Int.MaxValue)

  test("only expected calibrations are charged"):
    val digest = CalibrationDigest(estimate(2, 10), estimate(3, 15))
    assertEquals(digest.chargedTime.sum, TimeSpan.fromMinutes(15).get)
