// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.model.arb._

import munit._
import cats.kernel.laws.discipline.EqTests

final class OrcidProfileSuite extends DisciplineSuite {
  import ArbOrcidProfile._

  // Laws
  checkAll("OrcidProfile", EqTests[OrcidProfile].eqv)

}
