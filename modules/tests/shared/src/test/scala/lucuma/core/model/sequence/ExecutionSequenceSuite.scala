// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.*
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.arb.*
import monocle.law.discipline.*
import munit.*
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Test


final class ExecutionSequenceSuite extends DisciplineSuite {
  import ArbAtom.given
  import ArbDynamicConfig.*
  import ArbExecutionSequence.given

  override val scalaCheckTestParameters = Test.Parameters.default.withMaxSize(4)

  checkAll("Eq[Sequence[GmosNorth]]", EqTests[ExecutionSequence[GmosNorth]].eqv)
  checkAll("Sequence.nextAtom",       LensTests(ExecutionSequence.nextAtom[GmosNorth]))
  checkAll("Sequence.possibleFuture", LensTests(ExecutionSequence.possibleFuture[GmosNorth]))

}
