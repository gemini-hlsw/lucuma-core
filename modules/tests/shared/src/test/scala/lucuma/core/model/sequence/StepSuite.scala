// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.*
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.arb.*
import lucuma.core.util.arb.*
import lucuma.core.util.laws.UidTests
import monocle.law.discipline.*
import munit.*

final class StepSuite extends DisciplineSuite {
  
  import ArbDetectorEstimate.given
  import ArbDynamicConfig._
  import ArbEnumerated._
  import ArbStep.given
  import ArbStepConfig._
  import ArbStepEstimate.given
  import ArbUid._

  checkAll("Step.Id", UidTests[Step.Id].uid)
  
  checkAll("Eq[Step[GmosNorth]]",    EqTests[Step[GmosNorth]].eqv)
  checkAll("Step.id",                LensTests(Step.id[GmosNorth]))
  checkAll("Step.instrumentConfig",  LensTests(Step.instrumentConfig[GmosNorth]))
  checkAll("Step.stepConfig",        LensTests(Step.stepConfig[GmosNorth]))
  checkAll("Step.breakpoint",        LensTests(Step.breakpoint[GmosNorth]))
  checkAll("Step.estimate",          LensTests(Step.estimate[GmosNorth]))
}
