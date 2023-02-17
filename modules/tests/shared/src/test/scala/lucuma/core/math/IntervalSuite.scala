// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import lucuma.core.math.arb.ArbInterval
import lucuma.core.optics.Spire
import lucuma.core.optics.laws.discipline.SplitEpiTests

final class IntervalSuite extends munit.DisciplineSuite {
  import ArbInterval.given
  // import ArbTime.*

  checkAll(
    "Spire.intervalListUnion",
    SplitEpiTests(Spire.intervalListUnion[Int]).splitEpi
  )
}
