// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.ghost.arb.*
import munit.*

final class GhostDetectorSuite extends DisciplineSuite:

  import ArbGhostDetector.given

  checkAll("Eq[GhostDetector]", EqTests[GhostDetector].eqv)