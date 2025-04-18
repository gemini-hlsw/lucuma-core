// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.syntax.order.*
import lucuma.core.model.sequence.arb.ArbCategorizedTimeRange
import lucuma.core.model.sequence.arb.ArbPlannedTimeRange
import monocle.law.discipline.*
import munit.*
import org.scalacheck.Prop.forAll

final class PlannedTimeRangeSuite extends DisciplineSuite {

  import ArbPlannedTimeRange.given
  import ArbCategorizedTimeRange.given

  checkAll("Eq[PlannedTimeRange]", EqTests[PlannedTimeRange].eqv)
  checkAll("ToCategorizedTimeRange", IsoTests(PlannedTimeRange.ToCategorizedTimeRange))

  test("min <= max") {
    forAll { (a: PlannedTimeRange) => assert(a.min <= a.max) }
  }

}
