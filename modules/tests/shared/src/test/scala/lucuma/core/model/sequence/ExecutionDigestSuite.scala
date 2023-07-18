// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.ArbExecutionDigest
import munit.*

final class ExecutionDigestSuite extends DisciplineSuite {

  import ArbExecutionDigest.given

  checkAll("Eq[ExecutionDigest]", EqTests[ExecutionDigest].eqv)

}
