// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.ArbDatasetEstimate
import munit.*

final class DatasetEstimateSuite extends DisciplineSuite {

  import ArbDatasetEstimate.given

  checkAll("Order[DatasetEstimate]", OrderTests[DatasetEstimate].order)
  checkAll("Monoid[DatasetEstimate", MonoidTests[DatasetEstimate].monoid)

}
