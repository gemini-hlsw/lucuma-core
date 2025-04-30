// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.laws.discipline.*
import cats.kernel.laws.discipline.*
import lucuma.core.util.arb.ArbCalculationValue.given
import munit.*

class CalculationValueSuite extends DisciplineSuite:
  checkAll("CalculationValue.Monoid", MonoidTests[CalculationValue[Int]].monoid)

  // How do you fix this?
  // But both object given_Monad_CalculationValue in object CalculationValue and object given_Traverse_CalculationValue in object CalculationValue match type cats.Invariant[lucuma.core.util.CalculationValue].
  // checkAll("CalculationValue.Monad",  MonadTests[CalculationValue].monad[Int, String, Long])

  checkAll("CalculationValue.Traverse", TraverseTests[CalculationValue].traverse[Int, Int, Int, Int, Option, Option])
