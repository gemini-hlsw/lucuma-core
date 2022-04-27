// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._

final class StepSuite extends DisciplineSuite {
  import ArbStep._
  import ArbEnumerated._
  import ArbUid._
  import ArbInstrumentConfig._
  import ArbStepConfig._
  import ArbStepTime._

  checkAll("Eq[Step.GmosNorth]", EqTests[Step.GmosNorth].eqv)
  checkAll("Step.GmosNorth.id", LensTests(Step.GmosNorth.id))
  checkAll("Step.GmosNorth.instrumentConfig", LensTests(Step.GmosNorth.instrumentConfig))
  checkAll("Step.GmosNorth.stepConfig", LensTests(Step.GmosNorth.stepConfig))
  checkAll("Step.GmosNorth.time", LensTests(Step.GmosNorth.time))
  checkAll("Step.GmosNorth.breakpoint", LensTests(Step.GmosNorth.breakpoint))

  checkAll("Eq[Step.GmosSouth]", EqTests[Step.GmosSouth].eqv)
  checkAll("Step.GmosSouth.id", LensTests(Step.GmosSouth.id))
  checkAll("Step.GmosSouth.instrumentConfig", LensTests(Step.GmosSouth.instrumentConfig))
  checkAll("Step.GmosSouth.stepConfig", LensTests(Step.GmosSouth.stepConfig))
  checkAll("Step.GmosSouth.time", LensTests(Step.GmosSouth.time))
  checkAll("Step.GmosSouth.breakpoint", LensTests(Step.GmosSouth.breakpoint))

  checkAll("Eq[Step]", EqTests[Step].eqv)
  checkAll("Step.Id", UidTests[Step.Id].uid)
  checkAll("Step.gmosNorth", PrismTests(Step.gmosNorth))
  checkAll("Step.gmosSouth", PrismTests(Step.gmosSouth))
}
