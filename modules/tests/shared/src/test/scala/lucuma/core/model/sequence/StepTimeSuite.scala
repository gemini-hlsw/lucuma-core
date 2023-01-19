// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb.ArbTimeSpan
import monocle.law.discipline._
import munit._
import org.typelevel.cats.time._

final class StepTimeSuite extends DisciplineSuite {
  import ArbStepTime._
  import ArbTimeSpan.given

  checkAll("Eq[StepTime]", EqTests[StepTime].eqv)
  checkAll("StepTime.configChange", LensTests(StepTime.configChange))
  checkAll("StepTime.exposure", LensTests(StepTime.exposure))
  checkAll("StepTime.readout", LensTests(StepTime.readout))
  checkAll("StepTime.write", LensTests(StepTime.write))
  checkAll("StepTime.total", LensTests(StepTime.total))
}
