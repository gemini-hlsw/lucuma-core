// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.arb.ArbTime
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import lucuma.core.util.laws.UidTests
import monocle.law.discipline._
import munit._
import org.typelevel.cats.time._

final class StepRecordSuite extends DisciplineSuite {
  import ArbStepRecord._
  import ArbEnumerated._
  import ArbUid._
  import ArbDynamicConfig._
  import ArbStepConfig._
  import ArbTime._

  checkAll("Eq[StepRecord.GmosNorth]", EqTests[StepRecord.GmosNorth].eqv)
  checkAll("StepRecord.GmosNorth.id", LensTests(StepRecord.GmosNorth.id))
  checkAll("StepRecord.GmosNorth.created", LensTests(StepRecord.GmosNorth.created))
  checkAll("StepRecord.GmosNorth.startTime", LensTests(StepRecord.GmosNorth.startTime))
  checkAll("StepRecord.GmosNorth.endTime", LensTests(StepRecord.GmosNorth.endTime))
  checkAll(
    "StepRecord.GmosNorth.instrumentConfig",
    LensTests(StepRecord.GmosNorth.instrumentConfig)
  )
  checkAll("StepRecord.GmosNorth.stepConfig", LensTests(StepRecord.GmosNorth.stepConfig))

  checkAll("Eq[StepRecord.GmosSouth]", EqTests[StepRecord.GmosSouth].eqv)
  checkAll("StepRecord.GmosSouth.id", LensTests(StepRecord.GmosSouth.id))
  checkAll("StepRecord.GmosSouth.created", LensTests(StepRecord.GmosSouth.created))
  checkAll("StepRecord.GmosSouth.startTime", LensTests(StepRecord.GmosSouth.startTime))
  checkAll("StepRecord.GmosSouth.endTime", LensTests(StepRecord.GmosSouth.endTime))
  checkAll(
    "StepRecord.GmosSouth.instrumentConfig",
    LensTests(StepRecord.GmosSouth.instrumentConfig)
  )
  checkAll("StepRecord.GmosSouth.stepConfig", LensTests(StepRecord.GmosSouth.stepConfig))

  checkAll("Eq[StepRecord]", EqTests[StepRecord].eqv)
  checkAll("Step.Id", UidTests[Step.Id].uid)
  checkAll("StepRecord.GmosNorth", PrismTests(StepRecord.gmosNorth))
  checkAll("StepRecord.GmosSouth", PrismTests(StepRecord.gmosSouth))
}
