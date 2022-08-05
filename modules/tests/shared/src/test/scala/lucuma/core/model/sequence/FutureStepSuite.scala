// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import lucuma.core.util.laws.UidTests
import monocle.law.discipline._
import munit._

final class FutureStepSuite extends DisciplineSuite {
  import ArbFutureStep._
  import ArbEnumerated._
  import ArbUid._
  import ArbDynamicConfig._
  import ArbStepConfig._
  import ArbStepTime._

  checkAll("Step.Id", UidTests[Step.Id].uid)

  checkAll("Eq[FutureStep.GmosNorth]", EqTests[FutureStep.GmosNorth].eqv)
  checkAll("FutureStep.GmosNorth.id", LensTests(FutureStep.GmosNorth.id))
  checkAll(
    "FutureStep.GmosNorth.instrumentConfig",
    LensTests(FutureStep.GmosNorth.instrumentConfig)
  )
  checkAll("FutureStep.GmosNorth.stepConfig", LensTests(FutureStep.GmosNorth.stepConfig))
  checkAll("FutureStep.GmosNorth.time", LensTests(FutureStep.GmosNorth.time))
  checkAll("FutureStep.GmosNorth.breakpoint", LensTests(FutureStep.GmosNorth.breakpoint))

  checkAll("Eq[FutureStep.GmosSouth]", EqTests[FutureStep.GmosSouth].eqv)
  checkAll("FutureStep.GmosSouth.id", LensTests(FutureStep.GmosSouth.id))
  checkAll(
    "FutureStep.GmosSouth.instrumentConfig",
    LensTests(FutureStep.GmosSouth.instrumentConfig)
  )
  checkAll("FutureStep.GmosSouth.stepConfig", LensTests(FutureStep.GmosSouth.stepConfig))
  checkAll("FutureStep.GmosSouth.time", LensTests(FutureStep.GmosSouth.time))
  checkAll("FutureStep.GmosSouth.breakpoint", LensTests(FutureStep.GmosSouth.breakpoint))

  checkAll("Eq[FutureStep]", EqTests[FutureStep].eqv)
  checkAll("FutureStep.GmosNorth", PrismTests(FutureStep.gmosNorth))
  checkAll("FutureStep.GmosSouth", PrismTests(FutureStep.gmosSouth))
}
