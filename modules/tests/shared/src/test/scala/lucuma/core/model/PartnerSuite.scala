// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.util.arb.*
import lucuma.core.util.laws.EnumeratedTests
import munit.*

final class PartnerSuite extends DisciplineSuite {
  import ArbEnumerated.given

  // Laws
  checkAll("Partner", EnumeratedTests[Partner].enumerated)

}
