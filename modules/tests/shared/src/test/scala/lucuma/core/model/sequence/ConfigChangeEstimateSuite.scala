// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.ArbConfigChangeEstimate
import munit.*

final class ConfigChangeEstimateSuite extends DisciplineSuite {

  import ArbConfigChangeEstimate.given

  checkAll("Order[ConfigChangeEstimate]", OrderTests[ConfigChangeEstimate].order)

}
