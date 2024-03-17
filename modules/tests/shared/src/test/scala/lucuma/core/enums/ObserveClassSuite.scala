// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.kernel.laws.discipline.MonoidTests
import munit.DisciplineSuite

final class ObserveClassSuite extends DisciplineSuite {

  import lucuma.core.util.arb.ArbEnumerated.given

  checkAll("Monoid[ObserveClass]", MonoidTests[ObserveClass].monoid)

}
