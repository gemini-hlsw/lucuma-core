// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.ArbBandedTime
import munit.DisciplineSuite

final class BandedTimeSuite extends DisciplineSuite:

  import ArbBandedTime.given

  checkAll("Order[BandedTime]", OrderTests[BandedTime].order)