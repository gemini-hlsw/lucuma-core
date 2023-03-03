// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._

final class StepConfigSuite extends DisciplineSuite {
  import ArbStepConfig._
  import ArbEnumerated._
  import ArbOffset._

  checkAll("Eq[StepConfig.Gcal]", EqTests[StepConfig.Gcal].eqv)
  checkAll("StepConfig.Gcal.lamp", LensTests(StepConfig.Gcal.lamp))
  checkAll("StepConfig.Gcal.continuum", OptionalTests(StepConfig.Gcal.continuum))
  checkAll("StepConfig.Gcal.arcs", OptionalTests(StepConfig.Gcal.arcs))
  checkAll("StepConfig.Gcal.filter", LensTests(StepConfig.Gcal.filter))
  checkAll("StepConfig.Gcal.diffuser", LensTests(StepConfig.Gcal.diffuser))
  checkAll("StepConfig.Gcal.shutter", LensTests(StepConfig.Gcal.shutter))

  checkAll("Eq[StepConfig.Science]", EqTests[StepConfig.Science].eqv)
  checkAll("StepConfig.Science.offset", LensTests(StepConfig.Science.offset))

  checkAll("Eq[StepConfig]", EqTests[StepConfig].eqv)
  checkAll("StepConfig.gcal", PrismTests(StepConfig.gcal))
  checkAll("StepConfig.science", PrismTests(StepConfig.science))
}
