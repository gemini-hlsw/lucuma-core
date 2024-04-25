// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import lucuma.core.model.arb.*
import monocle.law.discipline.PrismTests
import munit.DisciplineSuite

class ExecutionEventSuite extends DisciplineSuite {
  import ArbExecutionEvent.given

  checkAll("Eq[ExecutionEvent]", EqTests[ExecutionEvent].eqv)
  checkAll("ExecutionEvent.atomEvent", PrismTests(ExecutionEvent.atomEvent))
  checkAll("ExecutionEvent.datasetEvent", PrismTests(ExecutionEvent.datasetEvent))
  checkAll("ExecutionEvent.sequenceEvent", PrismTests(ExecutionEvent.sequenceEvent))
  checkAll("ExecutionEvent.slewEvent", PrismTests(ExecutionEvent.slewEvent))
  checkAll("ExecutionEvent.stepEvent", PrismTests(ExecutionEvent.sequenceEvent))

}
