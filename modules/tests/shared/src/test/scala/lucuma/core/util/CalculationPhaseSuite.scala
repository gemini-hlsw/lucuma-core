// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.kernel.laws.discipline.*
import lucuma.core.util.arb.ArbEnumerated.given
import munit.*

class CalculationPhaseSuite extends DisciplineSuite:
  checkAll("CalculationPhase.Monoid", CommutativeMonoidTests[CalculationPhase].commutativeMonoid)