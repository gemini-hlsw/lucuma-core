// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.OrderTests
import lucuma.core.model.sequence.arb.ArbTimeChargeCorrection
import munit.*

final class TimeChargeCorrectionSuite extends DisciplineSuite {

  import ArbTimeChargeCorrection.given

  checkAll("Order[TimeChargeCorrection]", OrderTests[TimeChargeCorrection].order)

}
