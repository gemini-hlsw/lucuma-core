// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.OrderTests
import lucuma.core.model.arb.ArbTimingWindow
import munit.*

final class TimingWindowSuite extends DisciplineSuite {
  import ArbTimingWindow.given

  // Laws
  checkAll("Order[TimingWindow]", OrderTests[TimingWindow].eqv)
}
