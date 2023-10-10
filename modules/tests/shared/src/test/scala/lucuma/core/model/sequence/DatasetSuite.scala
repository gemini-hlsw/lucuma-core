// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.syntax.option.*
import lucuma.core.model.sequence.arb.*
import lucuma.core.util.arb.ArbGid
import monocle.law.discipline.*
import munit.*

final class DatasetSuite extends DisciplineSuite {
  import ArbDataset.given
  import ArbGid.*

  checkAll("Order[Dataset.Id]",       OrderTests[Dataset.Id].order)
  checkAll("Order[Dataset.Filename]", OrderTests[Dataset.Filename].order)

  checkAll("Dataset.Filename.FromString", PrismTests(Dataset.Filename.FromString))

  test("issue #780") {
    import eu.timepit.refined.types.numeric.PosInt
    import lucuma.core.enums.Site
    import java.time.LocalDate

    val f = Dataset.Filename.from(Site.GS, LocalDate.of(0, 12, 16), PosInt.unsafeFrom(1701874008)).get
    assertEquals(Dataset.Filename.FromString.getOption(f.format), f.some)
  }
}
