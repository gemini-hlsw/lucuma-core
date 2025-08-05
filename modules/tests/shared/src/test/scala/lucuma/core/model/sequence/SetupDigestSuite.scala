// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.ArbSetupDigest
import munit.*

final class SetupDigestSuite extends DisciplineSuite:

  import ArbSetupDigest.given

  checkAll("Eq[SetupDigest]", EqTests[SetupDigest].eqv)