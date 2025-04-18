// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.syntax.order.*
import lucuma.core.model.sequence.arb.ArbCategorizedTimeRange
import munit.*
import org.scalacheck.Prop.forAll

final class CategorizedTimeRangeSuite extends DisciplineSuite {

  import ArbCategorizedTimeRange.given

  checkAll("Eq[CategorizedTimeEstimate]", EqTests[CategorizedTimeRange].eqv)

  test("min <= max") {
    forAll { (a: CategorizedTimeRange) => assert(a.min <= a.max) }
  }

}