// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import lucuma.core.model.arb._
import munit._

class AngularSizeSuite extends DisciplineSuite {
  import ArbAngularSize._

  // Laws
  checkAll("Eq[AngularSize]", EqTests[AngularSize].eqv)
}
