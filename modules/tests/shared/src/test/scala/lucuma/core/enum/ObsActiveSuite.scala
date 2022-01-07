// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.`enum`

import cats.kernel.laws.discipline.OrderTests
import monocle.law.discipline.IsoTests
import munit.DisciplineSuite

final class ObsActiveSuite extends DisciplineSuite {

  import lucuma.core.util.arb.ArbEnumerated._

  checkAll("Order[ObsActiveStatus]", OrderTests[ObsActiveStatus].order)
  checkAll("Iso[ObsActiveStatus]", IsoTests(ObsActiveStatus.FromBoolean))

}
