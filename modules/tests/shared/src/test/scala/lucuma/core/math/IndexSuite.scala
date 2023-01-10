// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import monocle.law.discipline._

final class IndexSuite extends munit.DisciplineSuite {
  import ArbIndex._

  // Laws
  checkAll("Index", OrderTests[Index].order)
  checkAll("Index.fromShort", PrismTests(Index.fromShort))
  checkAll("Index.fromString", PrismTests(Index.fromString))

}
