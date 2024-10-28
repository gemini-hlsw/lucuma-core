// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.*
import lucuma.core.util.arb.*
import monocle.law.discipline.*
import munit.*

final class StepConfigSuite extends DisciplineSuite {
  import ArbEnumerated.given
  import ArbStepConfig.given
  import ArbTelescopeConfig.given

  checkAll("Eq[StepConfig.Gcal]", EqTests[StepConfig.Gcal].eqv)
  checkAll("StepConfig.Gcal.lamp", LensTests(StepConfig.Gcal.lamp))
  checkAll("StepConfig.Gcal.continuum", OptionalTests(StepConfig.Gcal.continuum))
  checkAll("StepConfig.Gcal.arcs", OptionalTests(StepConfig.Gcal.arcs))
  checkAll("StepConfig.Gcal.filter", LensTests(StepConfig.Gcal.filter))
  checkAll("StepConfig.Gcal.diffuser", LensTests(StepConfig.Gcal.diffuser))
  checkAll("StepConfig.Gcal.shutter", LensTests(StepConfig.Gcal.shutter))
  checkAll("StepConfig.Gcal.telescope", LensTests(StepConfig.Gcal.telescope))

  checkAll("Eq[StepConfig.Science]", EqTests[StepConfig.Science].eqv)
  checkAll("StepConfig.Science.telescope", LensTests(StepConfig.Science.telescope))

  checkAll("Eq[StepConfig.SmartGcal]", EqTests[StepConfig.SmartGcal].eqv)
  checkAll("StepConfig.SmartGcal.smartGcalType", LensTests(StepConfig.SmartGcal.smartGcalType))
  checkAll("StepConfig.SmartGcal.telescope", LensTests(StepConfig.SmartGcal.telescope))

  checkAll("Eq[StepConfig]", EqTests[StepConfig].eqv)
  checkAll("StepConfig.gcal", PrismTests(StepConfig.gcal))
  checkAll("StepConfig.science", PrismTests(StepConfig.science))
  checkAll("StepConfig.smartGcal", PrismTests(StepConfig.smartGcal))
}
