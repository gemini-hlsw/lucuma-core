// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import lucuma.core.math.arb._
import munit.DisciplineSuite
import cats.kernel.laws.discipline.EqTests

final class PlaceSuite extends DisciplineSuite {
  import ArbPlace._

  checkAll("Place", EqTests[Place].eqv)
}
