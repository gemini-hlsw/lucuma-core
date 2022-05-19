// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.util.arb._
import lucuma.core.util.laws.EnumeratedTests
import munit._

final class PartnerSuite extends DisciplineSuite {
  import ArbEnumerated._

  // Laws
  checkAll("Partner", EnumeratedTests[Partner].enumerated)

}
