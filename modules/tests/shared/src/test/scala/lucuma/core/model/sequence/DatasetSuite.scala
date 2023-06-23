// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.*
import monocle.law.discipline.*
import munit.*

final class DatasetSuite extends DisciplineSuite {
  import ArbDataset.given

  checkAll("Order[Dataset.Id]",       OrderTests[Dataset.Id].order)
  checkAll("Order[Dataset.Filename]", OrderTests[Dataset.Filename].order)

  checkAll("Dataset.Filename.FromString", PrismTests(Dataset.Filename.FromString))
}
