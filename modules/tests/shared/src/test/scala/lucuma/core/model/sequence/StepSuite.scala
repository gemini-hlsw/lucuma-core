// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import io.circe.testing.KeyCodecTests
import lucuma.core.math.arb.ArbOffset
import lucuma.core.model.sequence.arb.*
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.arb.*
import lucuma.core.util.arb.*
import lucuma.core.util.laws.UidTests
import monocle.law.discipline.*
import munit.*

final class StepSuite extends DisciplineSuite:
  import ArbDynamicConfig.given
  import ArbEnumerated.given
  import ArbOffset.given
  import ArbStep.given
  import ArbStepConfig.given
  import ArbStepEstimate.given
  import ArbTelescopeConfig.given
  import ArbUid.given

  checkAll("Uid[Step.Id]", UidTests[Step.Id].uid)
  checkAll("KeyCodec[Step.Id]", KeyCodecTests[Step.Id].keyCodec)

  checkAll("Eq[Step[GmosNorth]]",    EqTests[Step[GmosNorth]].eqv)
  checkAll("Step.id",                LensTests(Step.id[GmosNorth]))
  checkAll("Step.instrumentConfig",  LensTests(Step.instrumentConfig[GmosNorth]))
  checkAll("Step.stepConfig",        LensTests(Step.stepConfig[GmosNorth]))
  checkAll("Step.telescopeConfig",   LensTests(Step.telescopeConfig[GmosNorth]))
  checkAll("Step.offset",            LensTests(Step.offset[GmosNorth]))
  checkAll("Step.guiding",           LensTests(Step.guiding[GmosNorth]))
  checkAll("Step.breakpoint",        LensTests(Step.breakpoint[GmosNorth]))
  checkAll("Step.estimate",          LensTests(Step.estimate[GmosNorth]))