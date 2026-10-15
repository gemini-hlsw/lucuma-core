// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.ArbBandedSetupCharge
import munit.DisciplineSuite

final class BandedSetupChargeSuite extends DisciplineSuite:

  import ArbBandedSetupCharge.given

  checkAll("Order[BandedSetupCharge]", OrderTests[BandedSetupCharge].order)
