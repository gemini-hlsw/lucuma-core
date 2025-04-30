// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.kernel.laws.discipline.*
import cats.laws.discipline.*
import lucuma.core.util.arb.ArbCalculatedValue.given
import munit.*

class CalculatedValueSuite extends DisciplineSuite:
  checkAll("CalculatedValue.Monoid", MonoidTests[CalculatedValue[Int]].monoid)

  // How do you fix this?
  // But both object given_Monad_CalculatedValue in object CalculationValue and object given_Traverse_CalculatedValue in object CalculationValue match type cats.Invariant[lucuma.core.util.CalculatedValue].
  // checkAll("CalculatedValue.Monad",  MonadTests[CalculatedValue].monad[Int, String, Long])

  checkAll("CalculatedValue.Traverse", TraverseTests[CalculatedValue].traverse[Int, Int, Int, Int, Option, Option])
