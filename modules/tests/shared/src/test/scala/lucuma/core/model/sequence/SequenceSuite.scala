// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._
import org.scalacheck.Test

final class SequenceSuite extends DisciplineSuite {
  import ArbEnumerated._
  import ArbUid._
  import ArbAtom._
  import ArbStepTime._
  import ArbSequence._

  override val scalaCheckTestParameters = Test.Parameters.default.withMaxSize(10)

  checkAll("Eq[Sequence.GmosNorth]", EqTests[Sequence.GmosNorth].eqv)
  checkAll("Sequence.GmosNorth.atoms", LensTests(Sequence.GmosNorth.atoms))
  checkAll("Sequence.GmosNorth.time", LensTests(Sequence.GmosNorth.time))

  checkAll("Eq[Sequence.GmosSouth]", EqTests[Sequence.GmosSouth].eqv)
  checkAll("Sequence.GmosSouth.atoms", LensTests(Sequence.GmosSouth.atoms))
  checkAll("Sequence.GmosSouth.time", LensTests(Sequence.GmosSouth.time))

  checkAll("Eq[Sequence]", EqTests[Sequence].eqv)
  checkAll("Sequence.gmosNorth", PrismTests(Sequence.gmosNorth))
  checkAll("Sequence.gmosSouth", PrismTests(Sequence.gmosSouth))
}
