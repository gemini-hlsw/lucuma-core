// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import lucuma.core.model.arb.*
import munit.*

final class UserProfileSuite extends DisciplineSuite:
  import ArbUserProfile.given

  // Laws
  checkAll("UserProfile", EqTests[UserProfile].eqv)