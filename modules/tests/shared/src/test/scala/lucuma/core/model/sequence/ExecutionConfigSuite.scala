// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.*
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.model.sequence.gmos.arb.*
import monocle.law.discipline.*
import munit.*
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Test

final class ExecutionConfigSuite extends DisciplineSuite {

  import ArbDynamicConfig.*
  import ArbExecutionConfig.given
  import ArbExecutionSequence.given
  import ArbSetupTime.given
  import ArbStaticConfig.*

  override val scalaCheckTestParameters = Test.Parameters.default.withMaxSize(4)

  checkAll("Eq[ExecutionConfig[GmosNorth]", EqTests[ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]].eqv)
  checkAll("ExecutionConfig.static",        LensTests(ExecutionConfig.static[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]))
  checkAll("ExecutionConfig.acquisition",   LensTests(ExecutionConfig.acquisition[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]))
  checkAll("ExecutionConfig.science",       LensTests(ExecutionConfig.science[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]))
  checkAll("ExecutionConfig.setup",         LensTests(ExecutionConfig.setup[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]))
}
