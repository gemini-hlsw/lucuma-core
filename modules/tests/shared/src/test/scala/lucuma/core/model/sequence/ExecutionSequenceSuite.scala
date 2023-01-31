// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.model.sequence.arb._
import monocle.law.discipline._
import munit._

final class ExecutionSequenceSuite extends DisciplineSuite {
  import ArbAtom._
  import ArbExecutionSequence._

  checkAll("Eq[ExecutionSequence.GmosNorth]", EqTests[ExecutionSequence.GmosNorth].eqv)
  checkAll("ExecutionSequence.GmosNorth.nextAtom", LensTests(ExecutionSequence.GmosNorth.nextAtom))
  checkAll("ExecutionSequence.GmosNorth.possibleFuture",
           LensTests(ExecutionSequence.GmosNorth.possibleFuture)
  )

  checkAll("Eq[ExecutionSequence.GmosSouth]", EqTests[ExecutionSequence.GmosSouth].eqv)
  checkAll("ExecutionSequence.GmosSouth.nextAtom", LensTests(ExecutionSequence.GmosSouth.nextAtom))
  checkAll("ExecutionSequence.GmosSouth.possibleFuture",
           LensTests(ExecutionSequence.GmosSouth.possibleFuture)
  )

  checkAll("Eq[ExecutionSequence]", EqTests[ExecutionSequence].eqv)
  checkAll("ExecutionSequence.gmosNorth", PrismTests(ExecutionSequence.gmosNorth))
  checkAll("ExecutionSequence.gmosSouth", PrismTests(ExecutionSequence.gmosSouth))
}
